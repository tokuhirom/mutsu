use super::*;

impl Interpreter {
    pub(super) fn call_compiled_function_named(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        // A routine declared directly in this body is lexical to the call.
        // Snapshot the routine registry around the (multi-exit) body so the
        // lexical routine is removed on return — UNLESS it escapes by being
        // returned (then its registry entry must survive so it stays callable
        // by name). The snapshot is cheap relative to the rare case it guards.
        if !cf.declares_inner_routines {
            return self.call_compiled_function_named_inner(
                cf,
                args,
                compiled_fns,
                fn_package,
                fn_name,
            );
        }
        let snapshot = self.snapshot_routine_registry();
        let result =
            self.call_compiled_function_named_inner(cf, args, compiled_fns, fn_package, fn_name);
        match &result {
            Ok(v) if Self::return_value_escapes_routine(v) => {}
            _ => self.restore_routine_registry(snapshot),
        }
        result
    }
}
