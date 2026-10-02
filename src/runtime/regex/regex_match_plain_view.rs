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
use super::regex_helpers::{InlineCaptureScope, atom_contains_code, fold_quantified_captures};

/// Does an iteration of an unseparated quantifier over `atom`, which takes
/// `stride` positional captures per iteration, need the folded view? Only a
/// sub-pattern of the same regex (`[ … ]`, an alternation, …) that runs code
/// reads the level its iterations append to: a capture group's code reads its
/// own level, and a quantifier that captures nothing has nothing to fold.
// Cost: O(1) (the code scan is memoized on the pattern).
pub(crate) fn plain_iter_needs_view(atom: &RegexAtom, stride: usize) -> bool {
    stride > 0 && Interpreter::atom_shares_backref_scope(atom) && atom_contains_code(atom)
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

/// Does `atom`, taken as one iteration of a quantifier, apply a numbered alias
/// (`$N=`) at its own level? The walk matches each iteration as a nested
/// sub-pattern, so `N` is counted from the iteration's first slot (`[ $0=(\d)
/// ]+` files one `$0` per iteration); the compiled engine numbers an inlined
/// body against the whole level, so such a body needs a level of its own per
/// iteration (`OpenPlainIter`). A capture group opens its own level already.
// Cost: O(t), t = the tokens of `atom` outside its capture groups (run once,
// when the pattern compiles).
pub(crate) fn atom_has_numbered_alias(atom: &RegexAtom) -> bool {
    let pattern_has = |p: &RegexPattern| {
        p.tokens.iter().any(|t| {
            t.named_capture
                .as_ref()
                .is_some_and(|n| n.parse::<usize>().is_ok())
                || atom_has_numbered_alias(&t.atom)
        })
    };
    match atom {
        RegexAtom::Group(p) => pattern_has(p),
        RegexAtom::Alternation(alts) | RegexAtom::SequentialAlternation(alts) => {
            alts.iter().any(pattern_has)
        }
        _ => false,
    }
}

/// The view and fold scope an iteration's atom matches under, when it needs
/// one (`plain_iter_needs_view`). The scope stays armed while the guard lives.
// Cost: O(c) when the atom needs the view, c = `caps`'s captures; else O(1).
pub(super) fn arm_plain_iter_view(
    atom: &RegexAtom,
    caps: &RegexCaptures,
    pos_base: usize,
    stride: usize,
) -> Option<(RegexCaptures, InlineCaptureScope)> {
    if !plain_iter_needs_view(atom, stride) {
        return None;
    }
    let (view, start) = plain_iter_view(caps, pos_base, stride);
    Some((view, InlineCaptureScope::enter(start, stride)))
}
