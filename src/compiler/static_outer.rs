//! What the compiler does for a program that may read another frame's
//! lexical chain (`crate::static_outer_latch`, ADR-12529 amendment A1,
//! #18787).

use super::*;

impl Compiler {
    /// Mark `compiled`, a closure this compiler attaches, as created in an
    /// `EVAL`'d unit's mainline when this compiler compiles one: its capture
    /// is the unit's outermost scope (A1 (c);
    /// `Interpreter::capture_eval_unit_lexicals`).
    // Cost: O(1).
    pub(super) fn note_eval_mainline_closure(&self, compiled: &mut CompiledCode) {
        if self.in_eval_unit {
            compiled.eval_mainline_closure = true;
        }
    }
}

impl Compiler {
    /// Queue the sub declaration plans `decls` (the in-sequence plan and its
    /// hoisted twin) for a static view, when the static-outer latch is set and
    /// the sub is declared below its unit's top level (A1 (b)): the view is
    /// baked when the declaring scope closes, so it holds the scope's later
    /// declarations too (rakudo's pad holds them).
    // Cost: O(p), p = `decls`.
    pub(super) fn note_static_view_plans(&mut self, decls: &[u32]) {
        if !crate::static_outer_latch::static_outer_observed() {
            return;
        }
        let at_unit_top = !self.in_lexical_scope
            && self.local_scopes.len().checked_sub(1) == Some(self.unit_root_scope);
        if at_unit_top {
            return;
        }
        let depth = self.local_scopes.len().saturating_sub(1);
        self.static_view_pending
            .extend(decls.iter().map(|&decl| (depth, decl)));
    }

    /// Bake the static view of every queued plan declared at scope depth
    /// `depth` or deeper, from the names `local_map` resolves now -- the
    /// declaring scope's, before it closes.
    // Cost: O(p * l), p = plans baked, l = names in `local_map`.
    pub(super) fn bake_static_views_closing(&mut self, depth: usize) {
        if self.static_view_pending.is_empty() {
            return;
        }
        let mut closing = Vec::new();
        self.static_view_pending.retain(|&(level, decl)| {
            if level >= depth {
                closing.push(decl);
                false
            } else {
                true
            }
        });
        if closing.is_empty() {
            return;
        }
        let mut view: Vec<(crate::symbol::Symbol, u32)> = self
            .local_map
            .iter()
            .filter(|(name, _)| crate::env::is_plain_user_lexical(name))
            .map(|(name, &slot)| (crate::symbol::Symbol::intern(name), slot))
            .collect();
        view.sort_by(|a, b| a.0.as_str().cmp(b.0.as_str()));
        for decl in closing {
            self.code.set_sub_decl_static_view_slots(decl, view.clone());
        }
    }
}
