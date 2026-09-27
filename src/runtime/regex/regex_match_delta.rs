//! How an atom candidate's inner captures become the enclosing level's capture
//! delta (ADR-0073).
//!
//! Split out of `regex_match_lazy.rs`: the demand-driven drivers live there,
//! and these are the pure per-shape capture transforms they and the eager
//! producer share.

use super::super::*;
use super::regex_helpers::AlternationListFlags;
use std::cell::Cell;

/// How a group atom turns one inner match into this level's capture delta.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum GroupShape {
    /// `[ ... ]` — the inner captures join the caller's numbering.
    Merge,
    /// `( ... )` — the inner captures become this group's sub-Match.
    Capture,
    /// `<$rx>` and friends — the inner captures are discarded entirely.
    Isolated,
}

impl GroupShape {
    pub(super) fn dedups_ends(self) -> bool {
        matches!(self, GroupShape::Capture)
    }

    pub(super) fn delta(self, pos: usize, end: usize, inner: RegexCaptures) -> RegexCaptures {
        match self {
            GroupShape::Merge => group_merge_delta(inner),
            GroupShape::Capture => capture_group_delta(pos, end, inner),
            GroupShape::Isolated => RegexCaptures::default(),
        }
    }
}

/// `[ ... ]`: named captures merge into the caller's map, positionals append,
/// an inline `make` and any `:my`/`:let` write leave the group with it, and a
/// `<(` / `)>` marker inside sets the whole pattern's match boundaries.
pub(super) fn group_merge_delta(mut inner_caps: RegexCaptures) -> RegexCaptures {
    let mut new_caps = RegexCaptures::default();
    for (k, v) in inner_caps.named.drain() {
        new_caps.named.entry(k).or_default().merge(v);
    }
    new_caps.extend_capture_alias_map(inner_caps.take_capture_alias_map());
    new_caps.positional.append(&mut inner_caps.positional);
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps.extend_regex_vars(inner_caps.take_regex_vars());
    if inner_caps.capture_start.is_some() {
        new_caps.capture_start = inner_caps.capture_start;
    }
    if inner_caps.capture_end.is_some() {
        new_caps.capture_end = inner_caps.capture_end;
    }
    new_caps
}

/// `( ... )`: the inner captures become this group's sub-Match (`$/[0]<name>`),
/// deliberately NOT merged into the parent's top-level named map.
pub(super) fn capture_group_delta(
    pos: usize,
    end: usize,
    inner_caps: RegexCaptures,
) -> RegexCaptures {
    let mut new_caps = RegexCaptures::default();
    let mut inner_caps = inner_caps;
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps.extend_regex_vars(inner_caps.regex_vars().clone());
    let mut subcap = inner_caps;
    subcap.from = pos;
    subcap.to = end;
    new_caps.positional.push(PosSlot {
        from: pos,
        to: end,
        subcap: Some(std::sync::Arc::new(subcap.into_cap_node())),
        ..Default::default()
    });
    new_caps
}

/// One `|` / `||` branch's inner match, padded into the alternation's shared
/// positional slot space, and — for a capture (name or positional slot)
/// [`AlternationListFlags`] marks list-valued anywhere in the alternation —
/// seeded as an empty LIST rather than left absent/Nil when this branch
/// never bound it (#9675: `'x' | <e>+` must leave `$<e>` as `[]`, not `Nil`,
/// when the `'x'` branch is the one that actually matched).
pub(super) fn alternation_branch_delta(
    flags: &AlternationListFlags,
    mut inner_caps: RegexCaptures,
) -> RegexCaptures {
    if !super::regex_helpers::IN_QUANTIFIED_ALTERNATION_MATCH.with(Cell::get) {
        pad_alternation_positional(&mut inner_caps, flags);
    }
    let mut new_caps = RegexCaptures::default();
    for (k, v) in inner_caps.named.drain() {
        new_caps.named.entry(k).or_default().merge(v);
    }
    for &name in &flags.named {
        new_caps.named.entry(name).or_insert_with(|| NamedSlot {
            nodes: Vec::new(),
            quantified: true,
        });
    }
    new_caps.extend_capture_alias_map(inner_caps.take_capture_alias_map());
    new_caps.positional.append(&mut inner_caps.positional);
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps.extend_regex_vars(inner_caps.take_regex_vars());
    new_caps
}

/// Grow `caps.positional` to the alternation's shared slot count, padding
/// each new slot as an empty LIST where `flags` says that slot sits under a
/// list quantifier ANYWHERE in the alternation, Nil (the historical
/// [`PosSlot::alternation_padding`]) otherwise.
fn pad_alternation_positional(caps: &mut RegexCaptures, flags: &AlternationListFlags) {
    while caps.positional.len() < flags.positional.len() {
        let idx = caps.positional.len();
        let slot = if flags.positional[idx] {
            PosSlot {
                quantified: Some(Vec::new()),
                ..Default::default()
            }
        } else {
            PosSlot::alternation_padding()
        };
        caps.positional.push(slot);
    }
}
