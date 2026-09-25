use super::*;

impl Interpreter {
    /// Call a lexical `&name` callable a bareword `name(...)` resolved to -- a
    /// `sub EXPORT`-installed import (#8746) or a `&name` parameter/binding --
    /// with the call site's argument sources in force.
    ///
    /// The binder reads `pending_call_arg_sources` to hand a raw (`\a`) or
    /// `is rw` parameter the caller's container rather than its value, exactly
    /// as `CallOnCodeVar` does for `my &f = &g; f($a)`. Without them a raw
    /// parameter of an EXPORT-imported sub saw the decontainerized value, so
    /// `a.VAR` and `nqp::iscont(a)` answered as for a literal (#9410).
    // Cost: O(1) beyond the call itself.
    pub(super) fn call_lexical_callable_with_sources(
        &mut self,
        callable: Value,
        args: Vec<Value>,
        arg_sources: &Option<Vec<Option<String>>>,
        compiled_fns: Option<&CompiledFns>,
    ) -> Result<Value, RuntimeError> {
        // A slip can change the argument count after the sources were
        // decoded; mismatched sources would bind the wrong containers.
        let sources = arg_sources
            .as_ref()
            .filter(|s| s.len() == args.len())
            .cloned();
        self.set_pending_call_arg_sources(sources);
        let result = self.vm_call_on_value(callable, args, compiled_fns);
        self.set_pending_call_arg_sources(None);
        result
    }
}
