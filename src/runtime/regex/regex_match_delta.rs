//! How an atom candidate's inner captures become the enclosing level's capture
//! delta (ADR-0073).
//!
//! Split out of `regex_match_lazy.rs`: the demand-driven drivers live there,
//! and these are the pure per-shape capture transforms they and the eager
//! producer share.

use super::super::*;
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
    for (k, v) in inner_caps.capture_alias_map.drain() {
        new_caps.capture_alias_map.insert(k, v);
    }
    new_caps.positional.append(&mut inner_caps.positional);
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps
        .regex_vars
        .extend(std::mem::take(&mut inner_caps.regex_vars));
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
    new_caps.regex_vars.extend(inner_caps.regex_vars.clone());
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
/// positional slot space.
pub(super) fn alternation_branch_delta(
    capture_slots: usize,
    mut inner_caps: RegexCaptures,
) -> RegexCaptures {
    if !super::regex_helpers::IN_QUANTIFIED_ALTERNATION_MATCH.with(Cell::get) {
        inner_caps
            .positional
            .resize(capture_slots, PosSlot::alternation_padding());
    }
    let mut new_caps = RegexCaptures::default();
    for (k, v) in inner_caps.named.drain() {
        new_caps.named.entry(k).or_default().merge(v);
    }
    for (k, v) in inner_caps.capture_alias_map.drain() {
        new_caps.capture_alias_map.insert(k, v);
    }
    new_caps.positional.append(&mut inner_caps.positional);
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps
        .regex_vars
        .extend(std::mem::take(&mut inner_caps.regex_vars));
    new_caps
}
