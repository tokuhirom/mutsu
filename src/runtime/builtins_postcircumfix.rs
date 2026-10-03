use super::*;

/// Whether `name` is one of the subscript adverbs CORE's `postcircumfix`
/// candidates take (`:k :v :kv :p :exists :delete`); any other name is an
/// unexpected adverb, reported as `X::Adverb` or `X::Multi::NoMatch`.
// Cost: O(1).
fn is_builtin_subscript_adverb(name: &str) -> bool {
    matches!(name, "k" | "v" | "kv" | "p" | "exists" | "delete")
}

impl Interpreter {
    /// The CORE `postcircumfix:<[ ]>` / `postcircumfix:<{ }>` routines.
    ///
    /// Raku's subscript operators are ordinary (multi) subs in CORE, so they are
    /// callable by name (`postcircumfix:<[ ]>(@a, 1)`) and capturable as a term
    /// (`my constant &old-same = &postcircumfix:<[ ]>`). The latter is the
    /// standard idiom for a module that adds its own subscript candidates and
    /// wants to delegate the ordinary shapes back to the built-in behaviour
    /// (`Array::Rounded`). mutsu compiles `@a[...]` straight to the `Index`
    /// opcode family, so without this routine the operator existed only as
    /// syntax and `&postcircumfix:<[ ]>` resolved to nothing.
    ///
    /// The implementation drives the same opcode the syntax lowers to, with the
    /// user-candidate probe suppressed for exactly that one dispatch: the CORE
    /// candidate must perform native indexing, never re-enter a user override
    /// (which is what turned the delegation idiom into unbounded recursion).
    pub(crate) fn builtin_postcircumfix_subscript(
        &mut self,
        args: &[Value],
        is_positional: bool,
    ) -> Result<Value, RuntimeError> {
        let op = if is_positional {
            "postcircumfix:<[ ]>"
        } else {
            "postcircumfix:<{ }>"
        };
        let Some(target) = args.first().cloned() else {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller {op}(); no invocant given"
            )));
        };
        // Named arguments arrive materialized as `Value::Pair` in place among
        // the positionals (see `exec_call_func_named_op_inner`); a `ValuePair`
        // (an ordinary `key => value` expression) is left alone (ADR-0021).
        // Splitting them off first, instead of letting them inflate
        // `args.len()`, is what keeps `@a[0, :nonesuch)` from being
        // misread as the 3-arg assignment form with the adverb as the RHS.
        let mut positional: Vec<Value> = Vec::with_capacity(args.len());
        let mut adverbs: Vec<(String, Value)> = Vec::new();
        for a in args {
            if let ValueView::Pair(key, val) = a.view() {
                adverbs.push((key.clone(), val.clone()));
            } else {
                positional.push(a.clone());
            }
        }
        if !adverbs.is_empty() {
            return self.postcircumfix_subscript_adverb(
                op,
                target,
                &positional,
                &adverbs,
                args,
                is_positional,
            );
        }
        match positional.len() {
            // `@a[]` / `%h{}` — the zen slice, which the compiler lowers to its
            // own `ZenSlice` node rather than an empty subscript, and which
            // simply answers the whole container.
            1 => Ok(target),
            2 => self.core_subscript(target, positional[1].clone(), is_positional),
            // The assignment form: raku dispatches `@a[1] = 99` to a separate
            // three-argument candidate, and `postcircumfix:<[ ]>(@a, 1, 99)`
            // written out by hand does the same store.
            3 => {
                let index = positional[1].clone();
                let value = positional[2].clone();
                let method = if is_positional {
                    "ASSIGN-POS"
                } else {
                    "ASSIGN-KEY"
                };
                self.try_compiled_method_or_interpret(target, method, vec![index, value])
            }
            n => Err(RuntimeError::new(format!(
                "Cannot resolve caller {op}(); got {n} arguments"
            ))),
        }
    }

    /// The adverb-bearing call shapes of `postcircumfix:<[ ]>`/`<{ }>`
    /// (`postcircumfix:<[ ]>(@a, 1, :exists)`), mirroring what `@a[1]:exists`
    /// lowers to at the opcode level. A single index with a single built-in
    /// adverb is answered here. A call carrying an adverb that is not a
    /// built-in subscript adverb (`:nonesuch`) is classified exactly as the
    /// syntax form `@a[0,1]:nonesuch` is, by
    /// [`Interpreter::builtin_subscript_named_adverbs`]: a slice or an
    /// associative subscript raises `X::Adverb`, a single positional element
    /// `X::Multi::NoMatch`. Any other shape (several built-in adverbs, more
    /// than one index) has no matching CORE candidate, so it raises the same
    /// `X::Multi::NoMatch` a genuine multi-dispatch miss would.
    #[allow(clippy::too_many_arguments)]
    fn postcircumfix_subscript_adverb(
        &mut self,
        op: &str,
        target: Value,
        positional: &[Value],
        adverbs: &[(String, Value)],
        raw_args: &[Value],
        is_positional: bool,
    ) -> Result<Value, RuntimeError> {
        if adverbs
            .iter()
            .any(|(name, _)| !is_builtin_subscript_adverb(name))
        {
            // One positional is the zen slice (`postcircumfix:<[ ]>(@a, :foo)`),
            // two are the ordinary subscript; more than one index has no
            // candidate at all (`X::Multi::NoMatch`).
            let (index, zen) = match positional {
                [_] => (Value::NIL, true),
                [_, index] => (index.clone(), false),
                _ => return Err(self.multi_no_match_error(op, raw_args)),
            };
            let shape = match (is_positional, zen) {
                (true, false) => "[ ]",
                (true, true) => "[ ] zen",
                (false, false) => "{ }",
                (false, true) => "{ } zen",
            };
            // The variable the container was declared as (`@a` / `%h`), for the
            // report's `.source`; an anonymous container is the bare sigil, as
            // in `array_slot_ref` (ADR-0064).
            let source = match target.view() {
                ValueView::Array(data, _) => data
                    .descriptor_name
                    .as_deref()
                    .filter(|n| n.starts_with('@'))
                    .unwrap_or("@")
                    .to_string(),
                ValueView::Hash(data) => data
                    .descriptor_name
                    .as_deref()
                    .filter(|n| n.starts_with('%'))
                    .unwrap_or("%")
                    .to_string(),
                _ => String::new(),
            };
            let mut args = vec![target, index, Value::str(source), Value::str_from(shape)];
            args.extend(
                raw_args
                    .iter()
                    .filter(|a| matches!(a.view(), ValueView::Pair(..)))
                    .cloned(),
            );
            return self.builtin_subscript_named_adverbs(&args);
        }
        let ([_, index], [(key, val)]) = (positional, adverbs) else {
            return Err(self.multi_no_match_error(op, raw_args));
        };
        let index = index.clone();
        match key.as_str() {
            "exists" => {
                let method = if is_positional {
                    "EXISTS-POS"
                } else {
                    "EXISTS-KEY"
                };
                let exists = self
                    .try_compiled_method_or_interpret(target, method, vec![index])?
                    .truthy();
                Ok(Value::truth(exists ^ !val.truthy()))
            }
            "delete" if val.truthy() => {
                let method = if is_positional {
                    "DELETE-POS"
                } else {
                    "DELETE-KEY"
                };
                self.try_compiled_method_or_interpret(target, method, vec![index])
            }
            "delete" => self.core_subscript(target, index, is_positional),
            "k" => Ok(index),
            "v" => self.core_subscript(target, index, is_positional),
            "kv" => {
                let value = self.core_subscript(target, index.clone(), is_positional)?;
                Ok(Value::array(vec![index, value]))
            }
            "p" => {
                let value = self.core_subscript(target, index.clone(), is_positional)?;
                Ok(Value::value_pair(index, value))
            }
            // Only a built-in adverb reaches this match (an unknown one was
            // classified above), so this arm is the lone-adverb-name guard.
            _ => Err(self.multi_no_match_error(op, raw_args)),
        }
    }

    /// A subscript carrying an adverb that is not a built-in subscript adverb
    /// (`@a[0]:foo`, `%h<a>:$no`, `@a[1;0]:foo`, `@a[0]:k:foo`), with no user
    /// `postcircumfix` candidate in scope (see
    /// `parser::expr::postfix::named_adverb`). Raises what rakudo's CORE
    /// candidates raise for these arguments:
    ///
    /// - the multi-dimensional `postcircumfix:<[; ]>` / `<{; }>` have no
    ///   candidate taking an arbitrary named argument: `X::Multi::NoMatch`;
    /// - a single positional element (`Int:D`/`Any:D`/`Callable:D` index) has
    ///   only candidates that require a built-in adverb (`:$k!, *%_`), so
    ///   unknown adverbs alone are an `X::Multi::NoMatch`, and together with a
    ///   built-in one an `X::Adverb` on "element access";
    /// - every slice candidate (`Iterable`/`Range`/`Whatever`, the zen slice,
    ///   and every associative subscript) slurps `*%_`: an `X::Adverb`.
    ///
    /// Args: `(target, index, source, shape, |named adverbs)`, where `source`
    /// is the variable name the parser saw (empty when the target is not a
    /// variable) and `shape` names the subscript form (`"[ ]"`, `"[ ] zen"`,
    /// `"{ }"`, `"{ } zen"`, `"[; ]"`, `"{; }"`).
    // Cost: O(a log a), a = number of adverbs (sorted for the X::Adverb report).
    pub(crate) fn builtin_subscript_named_adverbs(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let [target, index, source, shape, adverbs @ ..] = args else {
            return Err(RuntimeError::new(
                "__mutsu_subscript_named_adverbs: missing arguments",
            ));
        };
        let shape = shape.to_string_value();
        let op = match shape.as_str() {
            "[ ]" | "[ ] zen" => "postcircumfix:<[ ]>",
            "{ }" | "{ } zen" => "postcircumfix:<{ }>",
            "[; ]" => "postcircumfix:<[; ]>",
            _ => "postcircumfix:<{; }>",
        };
        let no_match = |this: &Self| {
            let mut call_args = vec![target.clone(), index.clone()];
            call_args.extend(adverbs.iter().cloned());
            this.multi_no_match_error(op, &call_args)
        };
        if shape.starts_with("[;") || shape.starts_with("{;") {
            return Err(no_match(self));
        }
        let mut nogo = Vec::new();
        let mut unexpected = Vec::new();
        for adverb in adverbs {
            let ValueView::Pair(name, value) = adverb.view() else {
                continue;
            };
            if is_builtin_subscript_adverb(name) {
                // A built-in adverb is reported the way it was passed: `:!k`
                // (or `:k(0)`) is `!k`.
                nogo.push(if value.truthy() {
                    name.to_string()
                } else {
                    format!("!{name}")
                });
            } else {
                unexpected.push(name.to_string());
            }
        }
        let what = match shape.as_str() {
            "[ ] zen" => "zen slice",
            "{ } zen" if nogo.is_empty() => "{} slice",
            "{ }" | "{ } zen" => "slice",
            _ => match index.view() {
                ValueView::Whatever => "whatever slice",
                ValueView::Array(_, kind) if !kind.is_itemized() => "slice",
                ValueView::LazyList(ll) if !ll.is_itemized() => "slice",
                ValueView::Seq(_) | ValueView::HyperSeq(_) | ValueView::RaceSeq(_) => "slice",
                _ if index.is_range() => "slice",
                _ if nogo.is_empty() => return Err(no_match(self)),
                _ => "element access",
            },
        };
        let source = match container_descriptor_source(target) {
            Some(s) => s,
            None => match source.to_string_value() {
                s if s.is_empty() => crate::value::what_type_name(target),
                s => s,
            },
        };
        Err(RuntimeError::x_adverb(what, &source, &nogo, &unexpected))
    }

    /// `$x.AT-POS($i)` on a builtin positional (an Array/List or a Str): the
    /// CORE `postcircumfix:<[ ]>` itself, so the method and the subscript
    /// cannot disagree (ADR-0118). They used to: `@a.AT-POS(-1)` was `Nil`
    /// where `@a[-1]` is an `X::OutOfRange` Failure, `my Int @i; @i.AT-POS(5)`
    /// was `Any` where `@i[5]` is `Int`, and `"abc".AT-POS(1)` indexed a
    /// character where the one-element-list rule makes it out of range.
    /// `None` for any other receiver (a user class's own `AT-POS`, a Range,
    /// a multi-dimensional call, ...).
    // Cost: that of the `Index` opcode for one index.
    pub(crate) fn builtin_at_pos(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let [idx] = args else {
            return None;
        };
        if !matches!(target.view(), ValueView::Array(..) | ValueView::Str(_))
            || matches!(idx.view(), ValueView::Pair(..))
        {
            return None;
        }
        Some(self.core_subscript(target.clone(), idx.clone(), true))
    }

    /// Run the native subscript opcode for one (target, index) pair.
    fn core_subscript(
        &mut self,
        target: Value,
        index: Value,
        is_positional: bool,
    ) -> Result<Value, RuntimeError> {
        self.stack.push(target);
        self.stack.push(index);
        self.dispatch.skip_postcircumfix_overload = true;
        let result = self.exec_index_op_with_positional(is_positional);
        // The op consumes the flag itself; clear it on the error path too so a
        // failed subscript cannot leak the suppression onto the next dispatch.
        self.dispatch.skip_postcircumfix_overload = false;
        result?;
        Ok(self.stack.pop().unwrap_or(Value::NIL))
    }
}

/// The variable a subscripted container was declared as, from its container
/// descriptor (ADR-0064): what rakudo's `X::Adverb.source` reports, even when
/// the subscript spells a parameter bound to it (`-> @a { @a[1]:k:v }` called
/// with `@n` reports `@n`). `None` for a non-container or an unnamed one (the
/// `"element"` sentinel included), where the spelled name is the fallback.
// Cost: O(1).
pub(super) fn container_descriptor_source(target: &Value) -> Option<String> {
    let (name, sigil) = match target.view() {
        ValueView::Array(data, _) => (data.descriptor_name.clone()?, '@'),
        ValueView::Hash(data) => (data.descriptor_name.clone()?, '%'),
        _ => return None,
    };
    name.starts_with(sigil).then(|| name.to_string())
}
