//! Compile-time export of `is export` routines declared inside another
//! routine's body (mutsu#10050).
use super::*;

impl Interpreter {
    /// Register and export every `is export` routine declared in `routine`'s
    /// body, as part of installing `routine` itself.
    ///
    /// Raku's `is export` trait runs at compile time: a `my sub ... is export`
    /// nested in a routine body is in the module's `EXPORT::<tag>` package as
    /// soon as the compunit is compiled, before the enclosing routine ever
    /// runs, so an importer can call it. mutsu registers a nested declaration
    /// only when its `RegisterDecl` executes, i.e. while the enclosing routine
    /// runs — long after the importer's `use` copied the export table. The
    /// enclosing routine's installation during the module load is the earliest
    /// point where the nested declaration's plan and compiled body exist, so
    /// the nested declaration is registered (and so exported) from here.
    ///
    /// The installed routine is the static code object: it captures no frame.
    /// A free variable of it is read by name in the env live at the call
    /// (routine-nested `multi` subs keep that dynamic resolution, ADR-0114
    /// §4), and a call made during the enclosing routine's dynamic extent runs
    /// in an overlay of that routine's frame — Rakudo's "autoclose" of an
    /// un-cloned inner routine to the outer frame found on the caller chain.
    ///
    /// Only done while a module loads: outside one there is no importer to
    /// hand the export to, and registering the nested routine early would
    /// only make it visible outside its lexical scope.
    ///
    /// Cost: O(p), p = sub declarations compiled into the routine's body (plus
    /// the registration of each exported one, recursively).
    pub(super) fn register_nested_exported_subs(
        &mut self,
        routine: &CompiledFunction,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        if self.suppress_exports || !self.module_load_in_progress() {
            return Ok(());
        }
        let code = &routine.code;
        for (idx, plan) in code.sub_decl_plans.iter().enumerate() {
            // The hoist pre-pass registers a stripped copy of the same
            // declaration; the in-sequence plan carries the full traits.
            if !plan.is_export
                || plan.name_chunk.is_some()
                || plan.custom_traits.iter().any(|(t, _)| t == "__hoisted")
            {
                continue;
            }
            self.exec_register_sub_op_in_registry(code, idx as u32, compiled_fns)?;
        }
        Ok(())
    }
}
