use super::*;

impl Interpreter {
    pub(crate) fn call_compiled_function_named(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &CompiledFns,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        // Gate user-infix overrides out of module code (source_file = Some):
        // operators are lexically scoped per compilation unit.
        let is_module_call = cf.source_file.is_some();
        if is_module_call {
            self.module_call_depth += 1;
        }
        // A routine declared directly in this body is lexical to the call.
        // Snapshot the routine registry around the (multi-exit) body so the
        // lexical routine is removed on return — UNLESS it escapes by being
        // returned (then its registry entry must survive so it stays callable
        // by name). The snapshot is cheap relative to the rare case it guards.
        let result = if !cf.declares_inner_routines {
            self.call_compiled_function_named_inner(cf, args, compiled_fns, fn_package, fn_name)
        } else {
            let snapshot = self.snapshot_routine_registry();
            let r = self.call_compiled_function_named_inner(
                cf,
                args,
                compiled_fns,
                fn_package,
                fn_name,
            );
            match &r {
                Ok(v) if Self::return_value_escapes_routine(v) => {}
                _ => self.restore_routine_registry(snapshot),
            }
            r
        };
        if is_module_call {
            self.module_call_depth -= 1;
        }
        result
    }
}
