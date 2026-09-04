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
        match args.len() {
            // `@a[]` / `%h{}` — the zen slice, which the compiler lowers to its
            // own `ZenSlice` node rather than an empty subscript, and which
            // simply answers the whole container.
            1 => Ok(target),
            2 => self.core_subscript(target, args[1].clone(), is_positional),
            // The assignment form: raku dispatches `@a[1] = 99` to a separate
            // three-argument candidate, and `postcircumfix:<[ ]>(@a, 1, 99)`
            // written out by hand does the same store.
            3 => {
                let index = args[1].clone();
                let value = args[2].clone();
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
