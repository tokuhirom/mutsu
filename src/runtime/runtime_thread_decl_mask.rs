//! The declaration mask of the cross-thread name lane (ADR-0010,
//! ADR-0129): which `my` declarations mask their name, and re-establishing
//! that mask at the store that completes a declaration.

use super::*;

impl Interpreter {
    /// Whether a `my` declaration of `name` in `code` masks the name in
    /// `thread_redeclared_vars` while the shared store is active: not a
    /// routine (`&`), not a `state` variable (those share through dedicated
    /// cells), and for `@`/`%` only a plain lexical — twigil'd forms share a
    /// name across instances/dynamic scopes by design and keep the lane.
    pub(crate) fn thread_decl_masks_name(code: &CompiledCode, name: &str) -> bool {
        !name.starts_with('&')
            && (!name.starts_with(['@', '%']) || Self::is_plain_lexical_name(name))
            && !code.is_state_name(name)
    }

    /// Re-mask a declaration's name at the store that gives it its first value.
    ///
    /// `SetVarDynamic` masks the name, and a spawn performed by the initializer
    /// keeps the mask because the declaration is in flight
    /// (`thread_decl_in_flight`). But that set is keyed by bare name, so a
    /// *callee* declaring the same name ends the window early:
    /// `my @promises = helper(0), helper(1)` where each `helper` has its own
    /// `my @promises` and spawns. The callee's store clears the in-flight
    /// entry, the callee's spawn then drops the mask, and this declaration's
    /// store would write the caller's new binding straight over the entry the
    /// callee's still-running child is reading (#9723, the Tinky
    /// `validate-apply` hang). A declaration's binding is unpublished until the
    /// next spawn after it, whatever happened inside its initializer, so the
    /// mask is put back before the store.
    ///
    /// Cost: O(1) — one set probe (plus an insert when the mask was lost).
    pub(crate) fn remask_declaration_store(&mut self, code: &CompiledCode, idx: usize) {
        let Some(name) = code.locals.get(idx) else {
            return;
        };
        if !Self::thread_decl_masks_name(code, name) {
            return;
        }
        let mut masked = self.threads.thread_redeclared_vars.borrow_mut();
        if !masked.contains(name.as_str()) {
            masked.insert(name.to_string());
        }
    }

    /// Record a plain `@`/`%` lexical the running routine frame just masked,
    /// so the frame's [`ThreadParamMaskGuard`](crate::vm::vm_call_state_guard::ThreadParamMaskGuard)
    /// re-masks it on return (#15020). Outside a guarded frame (the main
    /// program, a block) nothing is logged: nobody would drain it.
    ///
    /// Cost: O(1) amortized.
    pub(crate) fn note_frame_declared_aggregate(&self, name: &str) {
        if !name.starts_with(['@', '%']) {
            return;
        }
        let mut log = self.threads.frame_declared_aggregates.borrow_mut();
        if log.depth > 0 && log.names.last().is_none_or(|last| last != name) {
            log.names.push(name.to_string());
        }
    }
}
