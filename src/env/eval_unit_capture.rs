//! A closure created in an `EVAL`'d unit's mainline: its capture is that
//! unit's outermost scope (ADR-12529 amendment A1 (c), #18787).
//!
//! The program scope a closure's frame links to is the *main* program's,
//! live when the closure runs. An `EVAL`'d unit's mainline is a scope of its
//! own, gone once the `EVAL` returned except in what its closures captured, so
//! such a closure's frame answers from its capture first and `UNIT::` of that
//! frame names the capture (`my $f = EVAL 'my $u = 1; -> { ... }'`, then
//! `$CALLER::UNIT::u` in code the pointy calls).

use super::Env;
use super::outer_scope::OuterScope;
use super::static_link::StaticLink;
use std::sync::Arc;

impl Env {
    /// Mark this env, a reflective closure's capture, as taken in an `EVAL`'d
    /// unit's mainline (see the module docs).
    // Cost: O(1), plus the overlay's copy-on-write when it is shared.
    pub(crate) fn mark_eval_unit_capture(&mut self) {
        self.cow_mut().mark_eval_unit_capture();
    }

    /// Whether this env is a capture marked by [`Self::mark_eval_unit_capture`].
    // Cost: O(1).
    pub(crate) fn is_eval_unit_capture(&self) -> bool {
        self.inner.is_eval_unit_capture()
    }

    /// The lexical `key` of the compilation unit the frame this env runs in
    /// belongs to: an `EVAL`'d unit's, recorded on the capture of a closure
    /// created in its mainline, else the program scope's.
    // Cost: O(d + l), d = chain tiers walked to the frame root, l = capture
    // layers.
    pub(crate) fn unit_lexical(&self, key: &str) -> Option<crate::value::Value> {
        match self.unit_scope()? {
            OuterScope::Capture(view) if view.is_eval_unit_capture() => {
                let recorded = crate::meta_ns::MetaNs::EvalUnit.key_for_str(key);
                view.get(&recorded)
                    .or_else(|| view.get(&crate::symbol::Symbol::intern(key)))
                    .cloned()
            }
            scope => scope.get(key),
        }
    }

    /// The scope `UNIT::` names for the frame this env runs in: the capture of
    /// a closure created in an `EVAL`'d unit's mainline, else the outermost of
    /// [`Self::enclosing_scopes`] (the program scope). `None` at the program
    /// scope itself.
    // Cost: O(d), d = chain tiers walked to the frame root.
    pub(crate) fn unit_scope(&self) -> Option<OuterScope> {
        let mut root = self;
        while !root.frame_root {
            match root.parent.as_deref() {
                Some(parent) => root = parent,
                None => return root.flattened_frame_scopes().pop(),
            }
        }
        if let Some(fb) = &root.fallback
            && fb.is_eval_unit_capture()
        {
            return Some(OuterScope::Capture(Arc::clone(fb)));
        }
        self.enclosing_scopes().pop()
    }
}

impl Env {
    /// [`Self::enclosing_scopes`] for the bottom tier of a chain a frame was
    /// flattened into: the record of its static link names the frame's
    /// declaring activation and the program scope. Empty when the tier holds
    /// no record (the chain is the program scope itself).
    // Cost: O(1).
    pub(super) fn flattened_frame_scopes(&self) -> Vec<OuterScope> {
        let Some(StaticLink::UnitOuter {
            target, activation, ..
        }) = self.inner.static_link()
        else {
            return Vec::new();
        };
        let mut scopes = Vec::new();
        if let Some(capture) = activation {
            scopes.push(OuterScope::Capture(Arc::clone(capture)));
        }
        scopes.push(OuterScope::Env(Env::clone(target)));
        scopes
    }
}
