//! Which slot a positional backreference names when it sits in an inline
//! sub-pattern (`[ … ]`, a `||` branch) that the walk matches as a level of
//! its own, reading the enclosing level's captures through
//! [`OuterBackrefCaps`].
//!
//! Also what the walk publishes to a `&` conjunction's later branches, which
//! read the enclosing level and the earlier branches' captures through the
//! same link, and the view code in such a level reads (`inline_capture_view`).
//!
//! A level that is one iteration of a separated quantifier reads the
//! enclosing view with its own captures folded into the iteration's slots
//! (`merge_positional`). Rakudo matches the whole chain on one cursor, so a
//! capture group under nested quantifiers is one slot: when the iteration is
//! itself inside another iteration, the captures it takes (and the inner
//! quantifier's iterations folded so far) fold into the *outer* iteration's
//! slots, not into slots of their own. [`ViewFold`] places them, level by
//! level, from the outermost link in.

use crate::runtime::regex_types::{OuterBackrefCaps, PosSlot, RegexCaptures};

/// How one level's own positional captures sit in the view it reads: the first
/// `folded` of them fold into the view's slots from `start` (the enclosing
/// iteration's slots, [`OuterBackrefCaps::merge_positional`]), the rest follow
/// the `base` slots the view had before them.
#[derive(Clone, Copy)]
pub(crate) struct ViewFold {
    start: usize,
    folded: usize,
    base: usize,
}

impl ViewFold {
    /// The placement of a level's captures in a view of `base` slots, folding
    /// per `merge` (`None`: nothing folds, they all follow).
    // Cost: O(1).
    pub(crate) fn new(base: usize, merge: Option<(usize, usize)>) -> Self {
        let (start, folded) = match merge {
            Some((start, stride)) => (start, stride.min(base.saturating_sub(start))),
            None => (0, 0),
        };
        ViewFold {
            start,
            folded,
            base,
        }
    }

    /// The view slot the level's own capture `k` ends up in.
    // Cost: O(1).
    pub(crate) fn slot(&self, k: usize) -> usize {
        if k < self.folded {
            self.start + k
        } else {
            self.base + (k - self.folded)
        }
    }

    /// Place the level's own capture `k` (in order, from the first): fold it
    /// into the iteration slot it belongs to, or append it.
    // Cost: O(e), e = the entries `slot` holds (one when it holds none).
    pub(crate) fn place(&self, view: &mut Vec<PosSlot>, k: usize, slot: &PosSlot) {
        if k >= self.folded {
            view.push(slot.clone());
            return;
        }
        let target = &mut view[self.start + k];
        let list = target.quantified.get_or_insert_with(Vec::new);
        slot.push_entries_to(list);
        if let Some((from, to, subcap)) = list.last() {
            target.from = *from;
            target.to = *to;
            target.subcap = subcap.clone();
        }
        target.nil = false;
    }
}

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

    /// Append captures from the outermost scope through this level in source
    /// order. This is the `$/` view for inline code; backreference lookup
    /// keeps its innermost-slot semantics instead. This link's own captures
    /// are the enclosing level's, so they fold as the parent's
    /// `merge_positional` says (the enclosing level may itself be an iteration
    /// of a separated quantifier).
    // Cost: O(c), c = the captures along the chain.
    pub(crate) fn append_captures(&self, out: &mut RegexCaptures) {
        match self.parent.as_ref() {
            Some(parent) => {
                parent.append_captures(out);
                let fold = ViewFold::new(out.positional.len(), parent.merge_positional);
                for (k, slot) in self.positional.iter().enumerate() {
                    fold.place(&mut out.positional, k, slot);
                }
            }
            None => out.positional.extend(self.positional.iter().cloned()),
        }
        for (key, slot) in &self.named {
            out.named.slot_mut(*key).merge(slot.clone());
        }
    }

    /// How many positional slots `append_captures` leaves.
    // Cost: O(d), d = the nesting depth.
    fn merged_len(&self) -> usize {
        let Some(parent) = self.parent.as_ref() else {
            return self.positional.len();
        };
        let base = parent.merged_len();
        let fold = ViewFold::new(base, parent.merge_positional);
        base + self.positional.len() - fold.folded.min(self.positional.len())
    }
}

impl RegexCaptures {
    /// Where this level's own positional capture `k` sits in the view it reads
    /// (`inline_capture_view`). An iteration of a separated quantifier folds
    /// the captures it takes into slot `inline_view_slot(first)`, where `first`
    /// is the level's own captures before it (so a capture taken before a
    /// `[ … ]` that holds the quantifier keeps its own slot) plus the
    /// iteration's offset in the quantifier's slots.
    // Cost: O(d), d = the nesting depth of inline levels.
    pub(crate) fn inline_view_slot(&self, k: usize) -> usize {
        self.inline_view_fold().slot(k)
    }

    /// The placement of this level's own captures in the view it reads.
    // Cost: O(d), d = the nesting depth of inline levels.
    pub(crate) fn inline_view_fold(&self) -> ViewFold {
        match self.outer_backref() {
            Some(outer) => ViewFold::new(outer.merged_len(), outer.merge_positional),
            None => ViewFold::new(0, None),
        }
    }

    /// Build the capture state visible to inline regex code. An inline walk
    /// has its own local accumulator, but code in a same-scope group sees the
    /// captures already taken by the enclosing regex as well.
    // Cost: O(c), c = the captures along the chain of enclosing levels.
    pub(crate) fn inline_capture_view(&self) -> RegexCaptures {
        let Some(outer) = self.outer_backref() else {
            return self.clone();
        };

        let mut visible = RegexCaptures {
            // Capture lookup crosses the inline-walk boundary, but the
            // in-progress `$/` span remains that walk's own span.  Code such
            // as XML's `{ make ~$/ }` must see the current attribute value,
            // not the whole enclosing element.
            match_from: self.match_from,
            ..Default::default()
        };
        outer.append_captures(&mut visible);

        let fold = ViewFold::new(visible.positional.len(), outer.merge_positional);
        for (k, slot) in self.positional.iter().enumerate() {
            fold.place(&mut visible.positional, k, slot);
        }
        for (key, slot) in &self.named {
            visible.named.slot_mut(*key).merge(slot.clone());
        }
        visible
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
