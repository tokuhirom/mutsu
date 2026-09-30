use super::*;

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
    /// lowers to at the opcode level. Only a single index and a single
    /// recognized adverb are handled -- anything else (an unrecognized
    /// adverb name such as `:nonesuch`, or more than one adverb at once) has
    /// no matching CORE candidate, so it raises the same `X::Multi::NoMatch`
    /// a genuine multi-dispatch miss would.
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
            if matches!(name.as_str(), "k" | "v" | "kv" | "p" | "exists" | "delete") {
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
        let source = match source.to_string_value() {
            s if s.is_empty() => crate::value::what_type_name(target),
            s => s,
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
        self.skip_postcircumfix_overload = true;
        let result = self.exec_index_op_with_positional(is_positional);
        // The op consumes the flag itself; clear it on the error path too so a
        // failed subscript cannot leak the suppression onto the next dispatch.
        self.skip_postcircumfix_overload = false;
        result?;
        Ok(self.stack.pop().unwrap_or(Value::NIL))
    }
}
