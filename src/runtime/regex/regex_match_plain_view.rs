//! What code inside an unseparated quantifier (`*`, `+`, `** n..m`) sees.
//!
//! Rakudo matches the whole chain on one cursor, so a capture group under the
//! quantifier is one slot from the first iteration on: `$/` in code inside the
//! quantified `[ … ]` shows the iterations matched so far folded into the
//! quantifier's slots, with the iteration in progress folded into them as one
//! more (`[ (\d) { say $/[0] } ]+` on "12" prints `[1]`, then `[1 2]`).
//!
//! The walk appends each iteration's captures to the level raw and folds them
//! only when the quantifier ends (`fold_quantified_captures`). So while an
//! iteration matches an atom whose code reads the level, the walk hands the atom
//! a view with the earlier iterations folded and arms the `InlineCaptureScope`
//! the atom's seed carries as `merge_positional`, exactly as a separated
//! quantifier does for its iterations (`regex_match_sep_view`). The compiled
//! engine gives such an iteration an inline level of its own (`OpenPlainIter`,
//! `rx_levels::Levels::open_plain_iter`) that reads the same view.

use super::super::*;
use super::regex_helpers::{atom_contains_code, fold_quantified_captures};

/// Does an iteration of an unseparated quantifier over `atom`, which takes
/// `stride` positional captures per iteration, need the folded view? Only a
/// sub-pattern of the same regex (`[ … ]`, an alternation, …) that runs code
/// reads the level its iterations append to: a capture group's code reads its
/// own level, and a quantifier that captures nothing has nothing to fold.
// Cost: O(1) (the code scan is memoized on the pattern).
pub(crate) fn plain_iter_needs_view(atom: &RegexAtom, stride: usize) -> bool {
    stride > 0 && atom_shares_backref_scope(atom) && atom_contains_code(atom)
}

/// `caps` with the iterations appended since `pos_base` folded into the
/// quantifier's `stride` slots (an empty list each before the first one), and
/// the slot the iteration in progress folds into from, counted in the whole
/// list `inline_capture_view` shows (`inline_view_slot`, so a level that is
/// itself an iteration of an outer quantifier folds into the outer one's
/// slots).
// Cost: O(c), c = `caps`'s captures (one copy).
pub(crate) fn plain_iter_view(
    caps: &RegexCaptures,
    pos_base: usize,
    stride: usize,
) -> (RegexCaptures, usize) {
    let mut view = caps.clone();
    fold_quantified_captures(&mut view, pos_base, stride, true);
    (view, caps.inline_view_slot(pos_base))
}

/// Is this atom's sub-pattern matched *in the same capture scope* as the
/// pattern containing it, as far as a backreference is concerned?
///
/// Verified against real `raku`: a non-capturing group, either flavour of
/// alternation, a conjunction and a `~` goal all see the enclosing level's
/// captures (`/ $<x>=(\w) [ $<x> ] /` matches "aa"), while a **capturing**
/// group and a lookaround do NOT — rakudo gives each of those its own
/// cursor, so `/ $<x>=(\w) ( $<x> ) /` and
/// `/ $<x>=(\w) <?before $<x>> . /` both fail there. Those two therefore
/// arm a barrier rather than a read-through, and the barrier also hides the
/// outer level from anything nested deeper inside them
/// (`/ $<x>=(\w) ( [ $<x> ] ) /` fails in raku too).
pub(super) fn atom_shares_backref_scope(atom: &RegexAtom) -> bool {
    matches!(
        atom,
        RegexAtom::Group(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::SequentialAlternation(_)
            | RegexAtom::Conjunction(_)
            | RegexAtom::GoalMatch { .. }
    )
}
