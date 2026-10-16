//! A closure that rebinds a captured `@`/`%` of its creating frame
//! (ADR-12529 §3, slice 25).
//!
//! ```raku
//! sub run-it(&code) { code() }
//! { my @y; run-it({ @y := [3] }); say @y }   # [3]
//! ```
//!
//! A scalar the closure rebinds is boxed into a binding cell when the
//! closure is created (`box_captured_lexicals`, #9307). An `@`/`%` was not:
//! containers take their cell at the declaration, and only an escaping
//! closure's capture asked for one, so `run-it({ @y := [3] })` rebound the
//! closure's own env entry, and only the return merge of the call carried
//! the new binding to the block. The compiler now asks for the
//! declaration's container cell when a nested closure rebinds the name
//! (`needs_cell_unvouched_containers`), and the capture turns that cell into
//! a binding cell here; the closure's rebind then reseats it
//! (`reseat_env_binding_cell`), and the slot that shares it sees the new
//! binding.

use super::*;

impl Interpreter {
    /// Wrap each own `@`/`%` slot that `cc` rebinds, and that this frame
    /// records as rebound, in a binding cell shared by the slot and the env
    /// entry the capture copies.
    // Cost: O(r * f), r = names `cc` rebinds, f = `cc`'s free variables.
    pub(super) fn bind_rebound_captured_containers(
        &mut self,
        code: &CompiledCode,
        cc: &CompiledCode,
    ) {
        for sym in &cc.free_var_rebinds {
            if !sym.with_str(|s| s.starts_with(['@', '%'])) {
                continue;
            }
            let Some(fv_i) = cc.free_var_syms.iter().position(|s| s == sym) else {
                continue;
            };
            let Some(idx) = Self::resolve_capture_slot(code, &cc.free_var_parent_slots, fv_i, *sym)
            else {
                continue;
            };
            if !code.rebound_slots.contains(&(idx as u32)) {
                continue;
            }
            let cur = &self.locals[idx];
            if !cur.is_container_ref() || Self::binding_cell_of(cur).is_some() {
                continue;
            }
            let binding = Self::wrap_in_binding_cell(cur.clone());
            self.locals[idx] = binding.clone();
            self.env_mut().insert_sym(*sym, binding);
        }
    }
}
