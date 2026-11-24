//! A closure created in an `EVAL`'d unit's mainline, under the static-outer
//! latch (ADR-12529 amendment A1 (c), #18787): its capture also holds the
//! unit's own lexicals, so `CALLER::UNIT::` read from a frame the closure runs
//! (`Env::unit_scope`) finds the `EVAL`'s mainline, which is gone once the
//! `EVAL` returned.

use super::*;

impl Interpreter {
    /// Record in `env`, the capture of a closure the `EVAL` mainline `code` is
    /// creating, every plain user lexical `code` declares, and mark the
    /// capture as the unit's. Each is kept under its `MetaNs::EvalUnit` key,
    /// which only `UNIT::` reads (`Env::unit_lexical`): under its own name it
    /// would answer the by-name lookups of the routines the closure calls,
    /// which still walk their callers (ADR-12529 §1.3; Red's
    /// `t/124-compiler-callables`).
    ///
    /// `UNIT::` names the unit's outermost scope, so a name a nested block
    /// redeclares is the variable of its first slot. An `@`/`%`/`&` slot (or
    /// a sigilless term) is recorded as its object, which in-place writes
    /// share. A scalar slot that is already a shared cell is recorded as that
    /// cell. Any other scalar gets a holder cell of its own, which the unit
    /// fills with the variable's final value when its mainline ends
    /// ([`Interpreter::settle_eval_unit_holders`]), so a write the unit makes
    /// after creating the closure is seen (#19009). The slot itself is left
    /// alone: boxing it would turn every later read and write of the unit's
    /// own code into a cell access, which several code paths still treat
    /// differently from a plain local.
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
                let Some(current) = self.locals.get(slot).cloned() else {
                    continue;
                };
                let recorded =
                    if name.starts_with(['@', '%', '&', '\\']) || current.is_container_ref() {
                        if current.is_nil() {
                            continue;
                        }
                        current
                    } else {
                        let holder = current.into_container_ref();
                        self.lexicals.eval_unit_holders.push((
                            self.block_scope_depth(),
                            sym,
                            holder.clone(),
                        ));
                        holder
                    };
                env.insert_sym(crate::meta_ns::MetaNs::EvalUnit.key(sym), recorded);
            }
        }
        env.mark_eval_unit_capture();
    }

    /// The `EVAL`'d unit whose mainline ran at `block_scope_depth` `depth`
    /// has ended, its lexicals still bound in the env: fill each holder its
    /// closures recorded (`capture_eval_unit_lexicals`) with the variable's
    /// final value. Runs whether the mainline returned or died.
    // Cost: O(h * d), h = holders the unit recorded, d = env tiers probed.
    pub(crate) fn settle_eval_unit_holders(&mut self, depth: usize) {
        let holders = &mut self.lexicals.eval_unit_holders;
        let Some(first) = holders.iter().position(|(d, _, _)| *d > depth) else {
            return;
        };
        let settled = holders.split_off(first);
        for (_, sym, holder) in settled {
            if let Some(value) = self.env().get_sym(sym) {
                let value = if value.is_container_ref() {
                    value.deref_container()
                } else {
                    value.clone()
                };
                holder.assign_into_container(value);
            }
        }
    }
}
