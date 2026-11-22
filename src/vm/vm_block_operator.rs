//! Executing the declaration of a block's own plain operator routine
//! (#17771, `runtime::block_operator_sets`).

use super::*;

impl Interpreter {
    /// Declare the plain built-in operator routine of plan `plan_idx` in its
    /// block's operator set `set`, leaving the routine registry and the unit's
    /// operator gate as they were: a routine written outside the block, called
    /// from inside it, must not find the operator by name either.
    ///
    /// As for a frame-lexical routine (ADR-0113), the ordinary registration
    /// performs every check and derivation the declaration owes; its
    /// definition is kept and the registry rolled back.
    // Cost: O(r) for the registration and its rollback, r = routine-registry
    // entries the restore compares (O(1) when nothing else changed), plus the
    // binding (`bind_block_operator`).
    pub(super) fn exec_declare_block_operator(
        &mut self,
        code: &CompiledCode,
        plan_idx: u32,
        set: Symbol,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let Some(name) = code
            .sub_decl_plans
            .get(plan_idx as usize)
            .map(|plan| plan.name)
        else {
            return Ok(());
        };
        let snapshot = self.snapshot_routine_registry();
        let registered = self.exec_register_sub_op_in_registry(code, plan_idx, compiled_fns);
        let def = registered.map(|()| {
            let key = crate::qualified::qualified(self.current_package_sym(), name);
            self.routines().functions.get(&key).cloned()
        });
        self.restore_routine_registry(snapshot);
        if let Some(def) = def? {
            self.bind_block_operator(set, def);
        }
        Ok(())
    }
}
