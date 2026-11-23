//! `dd`: print each argument's `.raku` to `$*ERR`, prefixed the way
//! rakudo's `dd` (`src/core.c/Any.rakumod`) names its container.

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// `dd` called as a routine value (`&dd`, `.map(&dd)`): each argument's
    /// value part is rendered by the static `raku_value`, a user-class
    /// instance's through its `raku` method.
    // Cost: O(n + r), n = arguments and r = total rendered representation size.
    pub(super) fn builtin_dd(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        for (i, val) in args.iter().enumerate() {
            // A user-class instance's `.raku` needs the class registry to collect
            // its public attributes, which the static `raku_value` cannot reach
            // (it would render `F()`).
            // TODO: compile to bytecode -- only a direct `dd` call gets its
            // `.raku` from the VM (`__mutsu_dd`); a routine-value call still
            // renders here.
            let rendered = if matches!(val.view(), ValueView::Instance { .. }) {
                self.call_method_with_values(val.clone(), "raku", vec![])
                    .ok()
                    .map(|v| v.to_string_value())
            } else {
                None
            };
            self.dd_line(val, i, rendered)?;
        }
        Ok(args.first().cloned().unwrap_or(Value::NIL))
    }

    /// `__mutsu_dd(v1, r1, v2, r2, ...)`: a direct `dd` call, which the
    /// compiler (`try_compile_dd_call`) hands each argument together with its
    /// `.raku`, dispatched by the VM. rakudo's `dd` is `note prefix ~
    /// $var.raku`, so a typed Array (`Array[Int].new(1)`), a Block's signature
    /// or a user `raku` method all show as `.raku` answers (#19047). Argument
    /// `k`'s call-site source is at index `2k`.
    // Cost: O(n + r), n = arguments and r = total rendered representation size.
    pub(super) fn builtin_dd_rendered(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        for (k, pair) in args.chunks(2).enumerate() {
            let [val, rendered] = pair else {
                return Err(RuntimeError::new("__mutsu_dd: an argument without its .raku"));
            };
            self.dd_line(val, 2 * k, Some(rendered.to_string_value()))?;
        }
        Ok(Value::NIL)
    }

    /// Write one `dd` line for `val`, whose call-site source is argument
    /// `source_index`, with `rendered` as its `.raku` when the caller has it.
    // Cost: O(|source| + r), r = the rendered representation size.
    fn dd_line(
        &mut self,
        val: &Value,
        source_index: usize,
        rendered: Option<String>,
    ) -> Result<(), RuntimeError> {
        // A RakuAST node prints as the source it stands for, without a
        // variable label, as rakudo 2026.11's `dd` does (#18980); inside a
        // container it stays `.raku`.
        if matches!(val.view(), ValueView::RakuAst(_)) {
            let text = self.deparse_node(val.clone(), &[])?.to_string_value();
            let text = text.strip_suffix('\n').unwrap_or(&text);
            // Through the dynamic `$*ERR`, like every other `dd` line (#19048).
            return self.write_to_named_handle("$*ERR", text, true).map(|_| ());
        }
        // An element argument arrives as its `.VAR` descriptor (the
        // compiler's `try_compile_dd_call`): print the element, named after
        // the container it lives in (#19046).
        if let Some((prefix, element)) = Self::dd_element_prefix(val) {
            let element_repr = rendered.unwrap_or_else(|| {
                crate::builtins::methods_0arg::raku_repr::raku_value(&element)
            });
            return self
                .write_to_named_handle("$*ERR", &format!("{prefix}{element_repr}"), true)
                .map(|_| ());
        }
        let value_repr =
            rendered.unwrap_or_else(|| crate::builtins::methods_0arg::raku_repr::raku_value(val));
        let source_name = self
            .pending_call_arg_sources
            .as_ref()
            .and_then(|sources| sources.get(source_index).cloned().flatten());
        let source_name = source_name.as_deref();
        // A bareword records its own name as the source, as a scalar `$x`
        // records `x`; the type object a bareword names is no container,
        // so it takes no `$name = ` prefix (`dd IO::Path`, #19065).
        let names_itself = matches!(val.view(), ValueView::Package(pkg)
            if source_name.is_some_and(|source| {
                crate::value::user_facing_type_name(&pkg.resolve()) == source
            }));
        let prefix = if names_itself {
            String::new()
        } else {
            // The argument's live cell: an `is rw` parameter's slot holds the
            // caller's container, which names the caller's variable.
            let cell = self.call_site_source_cell(source_index).cloned();
            self.dd_prefix(source_name, cell.as_ref().unwrap_or(val))
        };
        self.write_to_named_handle("$*ERR", &format!("{prefix}{value_repr}"), true)
            .map(|_| ())
    }

    /// The part of a `dd` line before the value, as rakudo's `dd`
    /// (`src/core.c/Any.rakumod`) builds it: a container argument shows its
    /// name (`$x = `), and a container whose `of` is not `Mu` shows that type
    /// in front (`Int $y = `). An `@`/`%` variable is not itself a container,
    /// so it shows only a declared element type (`Int  = `), and a value with
    /// no container (a literal, a parameter, a `:=`-bound value) shows
    /// nothing. `source` is the argument's variable key, if it is one, and
    /// `value` the argument. A `$` container reports the variable it was
    /// declared as, so an `is rw` parameter or a `:=` alias names the
    /// caller's variable, with that container's type (#19046).
    // Cost: O(|source| + a) plus a readonly and a type-constraint probe,
    // a = alias chain length.
    fn dd_prefix(&self, source: Option<&str>, value: &Value) -> String {
        let Some(source) = source else {
            return String::new();
        };
        let (name, of) = match source.chars().next() {
            Some('@' | '%') if Self::dd_is_identifier(&source[1..]) => {
                (String::new(), self.var_type_constraint(source))
            }
            Some('&') if Self::dd_is_identifier(&source[1..]) => (
                source.to_string(),
                Some(
                    self.var_type_constraint(source)
                        .unwrap_or_else(|| "Callable".to_string()),
                ),
            ),
            // `self` is a term, not a container, though it shares the key a
            // `$self` variable would have.
            _ if source != "self"
                && Self::dd_is_scalar_key(source)
                && !self.is_readonly(source) =>
            {
                let name = self
                    .scalar_container_name(source, value)
                    .unwrap_or_else(|| format!("${source}"));
                let cell_of = || match value.view() {
                    ValueView::ContainerRef(cell) => crate::value::lookup_container_constraint(&cell),
                    _ => match self.env.get(source).map(Value::view) {
                        Some(ValueView::ContainerRef(cell)) => {
                            crate::value::lookup_container_constraint(&cell)
                        }
                        _ => None,
                    },
                };
                (name, self.var_type_constraint(source).or_else(cell_of))
            }
            _ => return String::new(),
        };
        let prefix = match of.filter(|of| of != "Mu") {
            Some(of) => format!("{of} {name}"),
            None => name,
        };
        if prefix.is_empty() {
            prefix
        } else {
            format!("{prefix} = ")
        }
    }

    /// The `dd` prefix and the element of an element's `.VAR` descriptor:
    /// `element of @a = ` for an element of a named array or hash, with the
    /// container's `of` in front unless it is `Mu`, and no prefix for an
    /// element of an anonymous container. `None` for any other value.
    // Cost: O(1) attribute probes.
    fn dd_element_prefix(value: &Value) -> Option<(String, Value)> {
        let ValueView::Instance { attributes, .. } = value.view() else {
            return None;
        };
        let attrs = attributes.as_map();
        attrs.get("__mutsu_var_target")?;
        let element = attrs.get("__mutsu_var_value")?.clone().into_deref();
        let name = attrs.get("name").map(Value::to_string_value).unwrap_or_default();
        if !name.starts_with(['@', '%']) {
            return Some((String::new(), element));
        }
        let of = attrs
            .get("of")
            .and_then(|of| match of.view() {
                ValueView::Package(pkg) => {
                    Some(crate::value::user_facing_type_name(&pkg.resolve()).to_string())
                }
                _ => None,
            })
            .filter(|of| of != "Mu");
        let prefix = match of {
            Some(of) => format!("{of} element of {name} = "),
            None => format!("element of {name} = "),
        };
        Some((prefix, element))
    }

    /// Whether `key` names a `$` variable: `$/`, `$!`, or an optionally
    /// twigilled identifier (`x`, `*D`, `!a`, `_`).
    // Cost: O(|key|).
    fn dd_is_scalar_key(key: &str) -> bool {
        if matches!(key, "/" | "!") {
            return true;
        }
        let rest = key.strip_prefix(['*', '!', '?', '^', '.']).unwrap_or(key);
        Self::dd_is_identifier(rest)
    }

    /// Whether `name` is a non-empty identifier (letters, digits, `_`, `-`, `::`).
    // Cost: O(|name|).
    fn dd_is_identifier(name: &str) -> bool {
        name.chars().next().is_some_and(|c| c.is_alphabetic() || c == '_')
            && name
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '-' || c == ':')
    }
}
