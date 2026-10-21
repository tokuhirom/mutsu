//! Shared format construction, sprintf coercion and quote word processing.
use super::*;

impl Interpreter {
    // Cost: O(n + o) plus user coercion calls, n = input size, o = output length.
    pub(super) fn builtin_sprintf(
        &mut self,
        args: &[Value],
        z_mode: bool,
    ) -> Result<Value, RuntimeError> {
        // `sprintf`/`printf` declare the format as `Str(Cool) $format`, so a
        // non-`Str` format is *coerced*, dispatching a user `.Str` where there
        // is one (`sprintf(42)` is `"42"`, and a `Cool` subclass with its own
        // `.Str` formats through it). This used to fall back to an EMPTY format
        // for anything that was not already a `Str`, silently producing "".
        let fmt = match args.first() {
            None => String::new(),
            Some(v) => match v.view() {
                ValueView::Str(s) => s.to_string(),
                // A bare type object keeps the existing "" (rakudo rejects it
                // outright); everything else is a real `Cool` value.
                ValueView::Package(_) => String::new(),
                _ => self.render_str_value(v),
            },
        };
        // Raku's `sprintf($format, *@args)` slurps its arguments, so every
        // list-like argument (Array, List/Seq/Slip, Range) flattens into a single
        // flat positional list — `sprintf("%d %d", 1..2)` sees two args, and
        // `sprintf("%d %d %d", 1, 2..3)` sees three. `flatten_into_slurpy` performs
        // exactly the slurpy flatten, respecting itemization: a scalar-held array
        // (`my $x = [1,2,3]`) is itemized and stays a single argument, matching
        // raku, whereas a bare `[1,2,3]` literal flattens to its elements.
        let rest = &args[1..];
        let mut actual_args: Vec<Value> = Vec::with_capacity(rest.len());
        crate::runtime::types::flatten_into_slurpy(rest, &mut actual_args);
        super::sprintf::validate_sprintf_directives(&fmt, actual_args.len())?;
        super::sprintf::validate_sprintf_arg_types(&fmt, &actual_args)?;
        self.coerce_sprintf_args(&fmt, &mut actual_args)?;
        let rendered = if z_mode {
            super::sprintf::format_zprintf_args(&fmt, &actual_args)
        } else {
            super::sprintf::format_sprintf_args(&fmt, &actual_args)
        };
        Ok(Value::str(rendered))
    }

    /// The pure formatter (`format_sprintf`/`format_sprintf_args`) only knows
    /// `to_string_value`/`.gist`/numeric unbox, so a directive whose arg is a
    /// user object must first dispatch the user's coercion method matching the
    /// directive: `%s` → `.Str` (Raku uses `.Str` only, not `.Stringy`);
    /// integer directives (`%d %i %u %b %o %x %X %c`) → `.Int`; float
    /// directives (`%e %f %g`) → `.Numeric`. The coerced value replaces the arg
    /// in place (a string for `%s`, the numeric value otherwise). Any
    /// captured-outer write the method makes is recorded for the surrounding
    /// call op's `apply_pending_rw_writeback` to drain into the caller's slot
    /// (same as every other user-method call). Shared by `sprintf`/`printf`
    /// (`builtin_sprintf`) and `.fmt()` (`dispatch_fmt_with_user_coercion`),
    /// which both bottom out in the same pure formatter.
    // Cost: O(n + a) plus user method calls, n = format length, a = arguments coerced.
    pub(crate) fn coerce_sprintf_args(
        &mut self,
        fmt: &str,
        actual_args: &mut [Value],
    ) -> Result<(), RuntimeError> {
        for (idx, spec) in super::sprintf::sprintf_arg_specs(fmt) {
            let Some(arg) = actual_args.get(idx) else {
                continue;
            };
            let (class_name, is_type_object) = match arg.view() {
                ValueView::Instance { class_name, .. } => {
                    (Some(class_name.resolve().to_string()), false)
                }
                ValueView::Package(name) => (Some(name.resolve().to_string()), true),
                _ => (None, false),
            };
            let method = match spec.to_ascii_lowercase() {
                's' => "Str",
                'd' | 'i' | 'u' | 'b' | 'o' | 'x' | 'c' => "Int",
                'a' | 'e' | 'f' | 'g' => "Numeric",
                _ => continue,
            };
            // A role-mixed value is neither an `Instance` nor a `Package` view,
            // so it used to skip this coercion entirely and the pure formatter
            // rendered its base value (`sprintf("%s", @a but R)` printed the
            // list, not the role's `Str`).
            if arg.is_mixin_value() {
                // `%s` is an explicit `.Str` operation. Do not use the
                // Stringy-first helper here: a mixin whose wrapped class only
                // defines Stringy must retain its default `.Str` result.
                let coerced = if method == "Str" {
                    self.mixin_user_str(arg)
                } else {
                    let arg = arg.clone();
                    self.dispatch_mixin_method_call(&arg, method, vec![])
                };
                if let Some(v) = coerced {
                    let v = v?;
                    actual_args[idx] = if method == "Str" {
                        Value::str(v.to_string_value())
                    } else {
                        v
                    };
                }
                continue;
            }
            let Some(cn) = class_name else { continue };
            // `%s` on an instance is plain string context: dispatch `.Str` the
            // same way `~$obj` does, whether or not the class spells the method
            // out itself. Gating this on an *own* `Str` method rendered every
            // inherited one as the raw object — an Exception subclass, whose
            // `Str` comes from `message` up the MRO, printed
            // `My::X::Thing()` instead of its message.
            if method == "Str" && !is_type_object {
                let arg_clone = actual_args[idx].clone();
                actual_args[idx] = Value::str(self.stringify_value(arg_clone)?);
                continue;
            }
            if is_type_object {
                if !self.has_user_method(&cn, method) {
                    // A bare type object stringifies (`%s`) to "" with rakudo's
                    // "uninitialized value of type X in string context" warning
                    // (matching `~Int` / `Int.Str`), rather than the `(Int)` gist; a
                    // numeric directive coerces it to 0 with the "... in numeric
                    // context" warning (matching `+Int`). The pure formatter already
                    // renders the numeric 0, so the numeric arm only emits the
                    // warning.
                    if method == "Str" {
                        let coerced = self.warn_type_object_string_context(&cn, false)?;
                        actual_args[idx] = Value::str(coerced.to_string_value());
                    } else {
                        self.warn_type_object_numeric_context(&cn)?;
                    }
                    continue;
                }
                let arg_clone = actual_args[idx].clone();
                if let Ok(v) = self.call_method_with_values(arg_clone, method, vec![]) {
                    actual_args[idx] = if method == "Str" {
                        Value::str(v.to_string_value())
                    } else {
                        v
                    };
                }
                continue;
            }
            // A real instance's numeric coercion (`.Int`/`.Numeric`) is
            // dispatched unconditionally, whether it resolves to a
            // user-defined method or one of mutsu's own native-Rust methods
            // (e.g. `Match.Int`/`Match.Numeric`): a `has_user_method` gate
            // here would (and used to) skip exactly the native case, since a
            // builtin method is not a "user method" in the class registry's
            // sense — the root cause of a `Match` arg silently formatting as
            // 0 under a numeric directive (issue #8584). A method genuinely
            // missing on both fronts just errors, and that error is
            // swallowed here the same way it always was.
            let arg_clone = actual_args[idx].clone();
            if let Ok(v) = self.call_method_with_values(arg_clone, method, vec![]) {
                // `Real`'s `Numeric` is the object itself (`Duration`,
                // `Instant`): the directive formats its `Bridge`, the number.
                let v = if method == "Numeric" && matches!(v.view(), ValueView::Instance { .. }) {
                    self.call_method_with_values(v.clone(), "Bridge", vec![])
                        .unwrap_or(v)
                } else {
                    v
                };
                actual_args[idx] = v;
            }
        }
        Ok(())
    }

    // Cost: O(n), n = format string length.
    pub(super) fn builtin_make_format(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(value) = args.first() else {
            return Err(RuntimeError::new("Too few positionals passed; expected 1 argument but got 0"));
        };
        let Some(fmt) = value.as_str() else {
            return Err(self.typecheck_binding_parameter_failure("$format", "Str", value));
        };
        Ok(Self::make_format(fmt.to_string()))
    }

    // Cost: O(n + w), n = input bytes and collection elements, w = words produced.
    pub(super) fn builtin_quotewords_atom(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(Self::quotewords_args_result(args, true))
    }

    // Cost: O(n + w), n = input bytes and collection elements, w = words produced.
    pub(super) fn builtin_words_atom(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(Self::quotewords_args_result(args, false))
    }

    fn quotewords_args_result(args: &[Value], allomorphic: bool) -> Value {
        let mut items = Vec::new();
        let mut pending_literal = String::new();
        for value in args {
            if let Some(literal) = Self::quotewords_literal_value(value) {
                pending_literal.push_str(&literal);
                continue;
            }
            if !pending_literal.is_empty() {
                items.push(Value::str(std::mem::take(&mut pending_literal)));
            }
            Self::quotewords_value_into(value, &mut items, allomorphic);
        }
        if !pending_literal.is_empty() {
            items.push(Value::str(pending_literal));
        }
        if !allomorphic {
            return Value::array(items);
        }
        match items.len() {
            0 => Value::array(Vec::new()),
            1 => items.into_iter().next().unwrap(),
            _ => Value::array(items),
        }
    }

    fn quotewords_literal_value(value: &Value) -> Option<String> {
        let ValueView::Scalar(inner) = value.view() else {
            return None;
        };
        let ValueView::Pair(marker, payload) = inner.view() else {
            return None;
        };
        if marker == "__mutsu_qw_literal" {
            Some(payload.to_string_value())
        } else {
            None
        }
    }

    fn quotewords_value_into(value: &Value, out: &mut Vec<Value>, allomorphic: bool) {
        match value.view() {
            ValueView::Array(items, kind) if !kind.is_itemized() => {
                for item in items.iter() {
                    Self::quotewords_value_into(item, out, allomorphic);
                }
            }
            ValueView::Seq(items) => {
                for item in items.iter() {
                    Self::quotewords_value_into(item, out, allomorphic);
                }
            }
            ValueView::Slip(items) => {
                for item in items.iter() {
                    Self::quotewords_value_into(item, out, allomorphic);
                }
            }
            _ => {
                for word in value.to_string_value().split_whitespace() {
                    if allomorphic {
                        out.push(crate::parser::angle_word_value(word));
                    } else {
                        out.push(Value::str(word.to_string()));
                    }
                }
            }
        }
    }

}
