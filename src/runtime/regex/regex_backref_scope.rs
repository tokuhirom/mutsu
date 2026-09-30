//! Which slot a positional backreference names when it sits in an inline
//! sub-pattern (`[ … ]`, a `||` branch) that the walk matches as a level of
//! its own, reading the enclosing level's captures through
//! [`OuterBackrefCaps`].

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
