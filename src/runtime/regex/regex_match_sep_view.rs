//! What code inside a separated quantifier (`atom +% sep`) sees.
//!
//! Rakudo matches the whole chain on one cursor, so `$/` in code inside the
//! atom or the separator is the enclosing match so far: the captures before
//! the quantifier, then the quantifier's slots holding every iteration folded
//! so far, with the iteration in progress folded into them as one more
//! (`$/[*-1][*-1]` is the octet being matched in Net::Whois's
//! `[ (\d ** 1..3) <?{ $/[*-1][*-1] < 256 }> ] ** 4 % '.'`). An atom's own
//! captures fold into the atom's slots; a separator's into the separator's
//! slots, which follow them.
//!
//! The walk publishes that view to the nested walk of the iteration through
//! the outer-captures seed and `InlineCaptureScope` (whose fold
//! `inline_capture_view` applies); the compiled engine builds the same view
//! for an iteration's inline level (`rx_levels::inline_level_caps`).

use super::super::*;
use super::regex_helpers::{InlineCaptureScope, OuterCapsSeed};
use super::regex_trail::CapStore;

/// `enclosing`'s captures with the chain folded so far (`folded`, a
/// `separated_capture_delta` without a trailing separator) merged after them:
/// the level an iteration's walk reads through. The iteration's own captures
/// fold into the slots `folded` adds, starting at `sep_iteration_slot`.
// Cost: O(c), c = `enclosing`'s captures plus `folded`'s.
pub(super) fn sep_chain_view(enclosing: &RegexCaptures, folded: RegexCaptures) -> RegexCaptures {
    let mut store = CapStore::new(enclosing.clone());
    store.merge_delta(folded);
    store.into_caps()
}

/// Where an iteration's own captures fold: the first of the quantifier's atom
/// slots (`offset` 0) or separator slots (`offset` = the atom stride), counted
/// in the whole list `inline_capture_view` shows, so a capture taken before a
/// `[ … ]` that holds the quantifier keeps its own slot.
// Cost: O(d), d = the nesting depth of inline levels.
pub(super) fn sep_iteration_slot(enclosing: &RegexCaptures, offset: usize) -> usize {
    enclosing.inline_visible_positional_len() + offset
}

impl Interpreter {
    /// The ends of a separated quantifier's separator `sep` at `cur`, highest
    /// priority first (only the first when `first_only`). Code in the
    /// separator sees `enclosing`, the chain folded so far, and its own
    /// captures folded into the separator slots.
    // Cost: the separator's match; plus O(c) when it holds code, c = the
    // captures visible to it (one copy for the seed).
    #[allow(clippy::too_many_arguments)]
    pub(super) fn sep_ends_seeing_chain(
        &mut self,
        sep: &RegexPattern,
        chars: &[char],
        cur: usize,
        pkg: Symbol,
        enclosing: &RegexCaptures,
        folded: impl FnOnce() -> RegexCaptures,
        atom_stride: usize,
        first_only: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        let ends = |interp: &mut Interpreter| {
            if first_only {
                interp
                    .regex_match_end_from_caps_in_pkg(sep, chars, cur, pkg)
                    .into_iter()
                    .collect()
            } else {
                interp.regex_match_ends_from_caps_in_pkg(sep, chars, cur, pkg)
            }
        };
        if !super::regex_helpers::pattern_contains_code(sep) {
            return ends(self);
        }
        let start = sep_iteration_slot(enclosing, atom_stride);
        let stride = super::regex_match_sep::separator_stride(sep);
        let view = sep_chain_view(enclosing, folded());
        let _capture_scope = InlineCaptureScope::enter(start, stride);
        let _seed = OuterCapsSeed::arm(Some(std::sync::Arc::new(OuterBackrefCaps {
            named: view.named,
            positional: view.positional,
            parent: enclosing.outer_backref().cloned(),
            merge_positional: Some((start, stride)),
            match_from: enclosing.match_from,
        })));
        ends(self)
    }

    /// A ratcheted separated quantifier's atom at `at`: its highest-priority
    /// match only. Code in the atom sees `enclosing`, the chain folded so far
    /// (the separator just matched included) and its own captures folded into
    /// the atom slots.
    // Cost: the atom's match; plus O(c) when it holds code, c = the captures
    // visible to it (one view).
    #[allow(clippy::too_many_arguments)]
    pub(super) fn sep_atom_first_seeing_chain(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        at: usize,
        pkg: Symbol,
        ignore_case: bool,
        enclosing: &RegexCaptures,
        folded: impl FnOnce() -> RegexCaptures,
    ) -> Option<(usize, RegexCaptures)> {
        // The `_all_` enumeration's last candidate, not the singular matcher:
        // see `match_separated_quantifier_ratchet`.
        if !super::regex_helpers::atom_contains_code(&token.atom) {
            return self
                .regex_match_atom_all_with_capture_in_pkg(
                    &token.atom,
                    chars,
                    at,
                    enclosing,
                    pkg,
                    ignore_case,
                )
                .pop();
        }
        let view = sep_chain_view(enclosing, folded());
        let _capture_scope = InlineCaptureScope::enter(
            sep_iteration_slot(enclosing, 0),
            super::regex_helpers::count_capture_groups(&token.atom),
        );
        self.regex_match_atom_all_with_capture_in_pkg(
            &token.atom,
            chars,
            at,
            &view,
            pkg,
            ignore_case,
        )
        .pop()
    }
}
