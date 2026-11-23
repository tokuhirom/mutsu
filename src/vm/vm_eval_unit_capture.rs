//! A closure created in an `EVAL`'d unit's mainline, under the static-outer
//! latch (ADR-12529 amendment A1 (c), #18787): its capture also holds the
//! unit's own lexicals, so `CALLER::UNIT::` read from a frame the closure runs
//! (`Env::unit_scope`) finds the `EVAL`'s mainline, which is gone once the
//! `EVAL` returned.

use super::*;

impl Interpreter {
    /// Record in `env`, the capture of a closure the `EVAL` mainline `code` is
    /// creating, every plain user lexical `code` declares, as the shared cell
    /// of its slot (boxed in place, so a write the unit makes after creating
    /// the closure is seen, #19009), and mark the capture as the unit's. Each
    /// is kept under its `MetaNs::EvalUnit` key, which only `UNIT::` reads
    /// (`Env::unit_lexical`): under its own name it would answer the by-name
    /// lookups of the routines the closure calls, which still walk their
    /// callers (ADR-12529 §1.3; Red's `t/124-compiler-callables`).
    ///
    /// `UNIT::` names the unit's outermost scope, so a name a nested block
    /// redeclares is the variable of its first slot, not the shadow that may
    /// be visible where the closure is created. Boxing that slot rebinds the
    /// env's name only while the slot is its visible binding: rebinding a
    /// shadowed name would hand the shadow to every by-name reader of the
    /// outer variable.
    // Cost: O(l * d), l = `code`'s locals, d = env tiers probed per name.
    pub(super) fn capture_eval_unit_lexicals(&mut self, code: &CompiledCode, env: &mut Env) {
        if code.locals_sym.len() == code.locals.len() {
            let mut seen: Vec<Symbol> = Vec::new();
            for (slot, name) in code.locals.iter().enumerate() {
                if !crate::env::is_plain_user_lexical(name) {
                    continue;
                }
                let sym = code.locals_sym[slot];
                if seen.contains(&sym) {
                    continue;
                }
                seen.push(sym);
                if let Some(cell) = self.box_static_view_local(code, slot, sym) {
                    env.insert_sym(crate::meta_ns::MetaNs::EvalUnit.key(sym), cell);
                }
            }
        }
        env.mark_eval_unit_capture();
    }
}
