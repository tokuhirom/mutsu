//! A closure created in an `EVAL`'d unit's mainline, under the static-outer
//! latch (ADR-12529 amendment A1 (c), #18787): its capture also holds the
//! unit's own lexicals, so `CALLER::UNIT::` read from a frame the closure runs
//! (`Env::unit_scope`) finds the `EVAL`'s mainline, which is gone once the
//! `EVAL` returned.

use super::*;

impl Interpreter {
    /// Record in `env`, the capture of a closure the `EVAL` mainline `code` is
    /// creating, every plain user lexical `code` declares (its cell when the
    /// local is one, else its current value), and mark the capture as the unit's. Each
    /// is kept under its `MetaNs::EvalUnit` key, which only `UNIT::` reads
    /// (`Env::unit_lexical`): under its own name it would answer the by-name
    /// lookups of the routines the closure calls, which still walk their
    /// callers (ADR-12529 §1.3; Red's `t/124-compiler-callables`).
    // Cost: O(l * d), l = `code`'s locals, d = env tiers probed per name.
    pub(super) fn capture_eval_unit_lexicals(&mut self, code: &CompiledCode, env: &mut Env) {
        if code.locals_sym.len() == code.locals.len() {
            // The visible slot of each name: the one the env binds it to, else
            // the last one written.
            let mut chosen: Vec<(Symbol, usize)> = Vec::new();
            for (slot, name) in code.locals.iter().enumerate() {
                if !crate::env::is_plain_user_lexical(name) {
                    continue;
                }
                let sym = code.locals_sym[slot];
                let Some(value) = self.locals.get(slot) else {
                    continue;
                };
                if value.is_nil() {
                    continue;
                }
                let visible = self
                    .env()
                    .get_sym(sym)
                    .is_some_and(|v| v.same_binding(value));
                match chosen.iter_mut().find(|(s, _)| *s == sym) {
                    Some(entry) if visible || !self.chosen_is_visible(entry.0, entry.1) => {
                        entry.1 = slot;
                    }
                    Some(_) => {}
                    None => chosen.push((sym, slot)),
                }
            }
            // TODO(#19009): box each local into a shared cell (as `box_static_view_local`
            // does), so a write the unit makes after creating the closure is
            // seen too. Boxing the `EVAL` mainline's locals in place changed
            // what Red's re-`EVAL`ed callables resolve (`t/124-compiler-
            // callables` 59, a mainline helper read the caller's shadowing
            // lexical), so a local that is not a cell already is recorded by
            // its current value until that interaction is understood.
            for (sym, slot) in chosen {
                if let Some(value) = self.locals.get(slot).cloned() {
                    env.insert_sym(crate::meta_ns::MetaNs::EvalUnit.key(sym), value);
                }
            }
        }
        env.mark_eval_unit_capture();
    }

    /// Whether local `slot` holds the env's binding of `sym`.
    // Cost: O(d), d = env tiers probed.
    fn chosen_is_visible(&self, sym: Symbol, slot: usize) -> bool {
        self.locals.get(slot).is_some_and(|value| {
            self.env()
                .get_sym(sym)
                .is_some_and(|v| v.same_binding(value))
        })
    }
}
