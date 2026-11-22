//! Executing the declaration of a block's own operator routine or multi
//! candidate (#17771, #18617, `runtime::block_operator_sets`).

use super::*;

impl Interpreter {
    /// Declare the built-in operator routine of plan `plan_idx` in its
    /// block's operator set `set`, leaving the routine registry and the unit's
    /// operator gate as they were: a routine written outside the block, called
    /// from inside it, must not find the operator by name either.
    ///
    /// As for a frame-lexical routine (ADR-0113), the ordinary registration
    /// performs every check and derivation the declaration owes; its
    /// definition is kept and the registry rolled back. A `multi` candidate is
    /// the definition the registration added to the family.
    // Cost: O(r) for the registration and its rollback, r = routine-registry
    // entries the restore compares (O(1) when nothing else changed); a `multi`
    // adds two scans of the registry for the family's candidates. Plus the
    // binding (`bind_block_operator` / `bind_block_operator_candidate`).
    pub(super) fn exec_declare_block_operator(
        &mut self,
        code: &CompiledCode,
        plan_idx: u32,
        set: Symbol,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let Some((name, multi)) = code
            .sub_decl_plans
            .get(plan_idx as usize)
            .map(|plan| (plan.name, plan.multi))
        else {
            return Ok(());
        };
        let package = self.current_package();
        let snapshot = self.snapshot_routine_registry();
        let before: Vec<std::sync::Arc<crate::ast::FunctionDef>> = if multi {
            self.operator_family_defs(&package, &name.resolve())
        } else {
            Vec::new()
        };
        let registered = self.exec_register_sub_op_in_registry(code, plan_idx, compiled_fns);
        let def = registered.map(|()| {
            if multi {
                self.operator_family_defs(&package, &name.resolve())
                    .into_iter()
                    .find(|def| !before.iter().any(|b| std::sync::Arc::ptr_eq(b, def)))
            } else {
                let key = crate::qualified::qualified(self.current_package_sym(), name);
                self.routines().functions.get(&key).cloned()
            }
        });
        self.restore_routine_registry(snapshot);
        match def? {
            Some(def) if multi => self.bind_block_operator_candidate(set, def),
            Some(def) => self.bind_block_operator(set, def),
            None => {}
        }
        Ok(())
    }
}
