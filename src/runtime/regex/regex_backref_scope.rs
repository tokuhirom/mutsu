//! Which slot a positional backreference names when it sits in an inline
//! sub-pattern (`[ … ]`, a `||` branch) that the walk matches as a level of
//! its own, reading the enclosing level's captures through
//! [`OuterBackrefCaps`].
//!
//! Also what the walk publishes to a `&` conjunction's later branches, which
//! read the enclosing level and the earlier branches' captures through the
//! same link.

use crate::runtime::regex_types::{OuterBackrefCaps, PosSlot, RegexCaptures};

impl OuterBackrefCaps {
    /// How many positional slots this level and every enclosing one show, in
    /// `append_captures` order; `None` when a level folds into a separated
    /// quantifier's slots (`merge_positional`), which does not number
    /// sequentially.
    // Cost: O(d), d = the nesting depth.
    fn visible_len(&self) -> Option<usize> {
        if self.merge_positional.is_some() {
            return None;
        }
        let outer = match self.parent.as_ref() {
            Some(p) => p.visible_len()?,
            None => 0,
        };
        Some(outer + self.positional.len())
    }

    /// The slot at `idx` in the sequential numbering `visible_len` counts.
    // Cost: O(d²), d = the nesting depth.
    fn visible_positional(&self, idx: usize) -> Option<&PosSlot> {
        let outer = match self.parent.as_ref() {
            Some(p) => p.visible_len()?,
            None => 0,
        };
        if idx < outer {
            self.parent.as_ref()?.visible_positional(idx)
        } else {
            self.positional.get(idx - outer)
        }
    }
}

impl RegexCaptures {
    /// How many positional slots `inline_capture_view` shows before any fold:
    /// every enclosing level's (`append_captures` order), then this level's
    /// own. A separated quantifier's `merge_positional` start is absolute in
    /// that list, so `$0` taken before a `[ … ]` that holds the quantifier
    /// keeps its own slot.
    // Cost: O(d), d = the nesting depth of inline levels.
    pub(crate) fn inline_visible_positional_len(&self) -> usize {
        let mut len = self.positional.len();
        let mut cur = self.outer_backref();
        while let Some(outer) = cur {
            len += outer.positional.len();
            cur = outer.parent.as_ref();
        }
        len
    }

    /// The slot a backreference `$idx` names. An inline `[ … ]` / `||` level
    /// continues the enclosing level's numbering (`/ (a) [ (b) $0 ] /`: `$0`
    /// is the `a`, as in raku), so its own slots come after the enclosing
    /// ones; under a separated quantifier's fold, the innermost slot wins.
    // Cost: O(d²), d = the nesting depth of inline levels.
    pub(crate) fn backref_positional(&self, idx: usize) -> Option<&PosSlot> {
        let Some(outer) = self.outer_backref() else {
            return self.positional.get(idx);
        };
        match outer.visible_len() {
            Some(n) if idx < n => outer.visible_positional(idx),
            Some(n) => self.positional.get(idx - n),
            None => self
                .positional
                .get(idx)
                .or_else(|| outer.lookup_positional(idx)),
        }
    }
}

/// The outer-captures seed published right now (see `INLINE_OUTER_CAPS_SEED`).
pub(crate) fn current_outer_caps_seed() -> Option<std::sync::Arc<OuterBackrefCaps>> {
    super::regex_helpers::INLINE_OUTER_CAPS_SEED.with(|s| s.borrow().clone())
}

/// Arm the seed a later branch of a `&` conjunction reads through: `outer`, the
/// one the conjunction atom published, then `merged`, the earlier branches'
/// captures. Rakudo matches every branch on one cursor, so code in `b` of
/// `/ (a) [ (\w) & b { … } ] /` sees `$0` and the first branch's capture, and
/// `$/` spans from the enclosing match's start. A conjunction that published
/// nothing (no code or backreference in it) leaves the seed alone.
// Cost: O(c), c = `merged`'s captures (one copy), when `outer` is published.
pub(crate) fn arm_conjunction_branch_seed(
    outer: Option<&std::sync::Arc<OuterBackrefCaps>>,
    merged: &RegexCaptures,
) -> super::regex_helpers::OuterCapsSeed {
    let Some(outer) = outer else {
        return super::regex_helpers::OuterCapsSeed::inert();
    };
    super::regex_helpers::OuterCapsSeed::arm(Some(std::sync::Arc::new(OuterBackrefCaps {
        named: merged.named.clone(),
        positional: merged.positional.clone(),
        parent: Some(outer.clone()),
        merge_positional: None,
        match_from: outer.match_from,
    })))
}
