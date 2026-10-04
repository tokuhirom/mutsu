//! How an atom candidate's inner captures become the enclosing level's capture
//! delta (ADR-0073).
//!
//! Split out of `regex_match_lazy.rs`: the demand-driven drivers live there,
//! and these are the pure per-shape capture transforms they and the eager
//! producer share.

use super::super::*;
use super::regex_helpers::AlternationListFlags;
use super::regex_trail::CapStore;
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
        new_caps.named.slot_mut(k).merge(v);
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
    let (from, to) = capture_group_span(&mut subcap, pos, end);
    new_caps.positional.push(PosSlot {
        from,
        to,
        subcap: Some(std::sync::Arc::new(subcap.into_cap_node())),
        ..Default::default()
    });
    new_caps
}

/// The span a capture group's sub-Match covers: `pos .. end`, narrowed by a
/// `<(` / `)>` inside the group. A capture group is its own Match, so those
/// markers set ITS boundaries and do not reach the enclosing match
/// (`"xab" ~~ /(a )> b)/` is `ab` with `$0` = `a`, as in rakudo, #11570).
/// The markers are consumed: `caps.from`/`caps.to` become the span.
// Cost: O(1).
pub(super) fn capture_group_span(
    caps: &mut RegexCaptures,
    pos: usize,
    end: usize,
) -> (usize, usize) {
    let from = caps.capture_start.take().unwrap_or(pos);
    let to = caps.capture_end.take().unwrap_or(end).max(from);
    caps.from = from;
    caps.to = to;
    (from, to)
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
        new_caps.named.slot_mut(k).merge(v);
    }
    for &name in &flags.named {
        new_caps.named.slot_mut(name).quantified = true;
    }
    new_caps.extend_capture_alias_map(inner_caps.take_capture_alias_map());
    new_caps.positional.append(&mut inner_caps.positional);
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps.extend_regex_vars(inner_caps.take_regex_vars());
    // A branch shares the enclosing capture scope, so a `<(` / `)>` marker in
    // it sets the whole match's boundaries, as in a `[ … ]` group
    // (`/ x [ c || a <( b ] /` matches `b`).
    if inner_caps.capture_start.is_some() {
        new_caps.capture_start = inner_caps.capture_start;
    }
    if inner_caps.capture_end.is_some() {
        new_caps.capture_end = inner_caps.capture_end;
    }
    new_caps
}

/// Grow `caps.positional` to the alternation's shared slot count, padding
/// each new slot as an empty LIST where `flags` says that slot sits under a
/// list quantifier ANYWHERE in the alternation, Nil (the historical
/// [`PosSlot::alternation_padding`]) otherwise.
fn pad_alternation_positional(caps: &mut RegexCaptures, flags: &AlternationListFlags) {
    while caps.positional.len() < flags.positional.len() {
        let idx = caps.positional.len();
        caps.positional.push(alternation_padding_slot(flags, idx));
    }
}

fn alternation_padding_slot(flags: &AlternationListFlags, idx: usize) -> PosSlot {
    if flags.positional[idx] {
        PosSlot {
            quantified: Some(Vec::new()),
            ..Default::default()
        }
    } else {
        PosSlot::alternation_padding()
    }
}

/// What [`alternation_branch_delta`] adds to a branch that wrote its captures
/// straight into the enclosing level (the compiled engine, ADR-0135): the
/// padding slots after the `taken` positionals the branch produced, and every
/// list-valued name marked quantified. `suppress_padding` is the compiled
/// form of [`super::regex_helpers::IN_QUANTIFIED_ALTERNATION_MATCH`] for an
/// alternation inside a quantified body; the thread-local is honored too.
/// `None` when there is nothing to add.
// Cost: O(p + n), p = the padding slots, n = the list-valued names.
pub(super) fn alternation_tail_delta(
    flags: &AlternationListFlags,
    taken: usize,
    suppress_padding: bool,
) -> Option<RegexCaptures> {
    let pad = !suppress_padding
        && taken < flags.positional.len()
        && !super::regex_helpers::IN_QUANTIFIED_ALTERNATION_MATCH.with(Cell::get);
    if !pad && flags.named.is_empty() {
        return None;
    }
    let mut caps = RegexCaptures::default();
    if pad {
        for idx in taken..flags.positional.len() {
            caps.positional.push(alternation_padding_slot(flags, idx));
        }
    }
    for &name in &flags.named {
        caps.named.insert(name, NamedSlot::empty_list());
    }
    Some(caps)
}

/// Apply `token`'s `%<name>=(...)` hash capture over `from..to` to `store`:
/// the key and value are the first two positional slots the atom filed from
/// `pos_base` on (inside a capture group's sub-Match), else the matched text is
/// the key. Shared by both engines.
// Cost: O(k), k = the characters of the key and value.
pub(super) fn apply_hash_capture(
    store: &mut CapStore,
    chars: &[char],
    token: &RegexToken,
    from: usize,
    to: usize,
    pos_base: usize,
) {
    let Some(name) = token.hash_capture.as_ref() else {
        return;
    };
    let (key, value) = {
        let caps = store.caps();
        // Count how many new positional captures this atom produced
        let new_count = caps.positional.len().saturating_sub(pos_base);
        // Look for inner subcaptures on the group's slot
        let subcap_idx = if new_count >= 1 {
            pos_base
        } else {
            caps.positional.len()
        };
        let inner_positionals = caps
            .positional
            .get(subcap_idx)
            .and_then(|slot| slot.subcap.as_ref())
            .map(|sc| &sc.kids().positional);
        // The inner slots' text derives from their spans through the same
        // `chars` this pattern level is matching against (ADR-0016 P4).
        let slot_text = |slot: &PosSlot| -> String {
            let a = slot.from.min(chars.len());
            let b = slot.to.min(chars.len()).max(a);
            chars[a..b].iter().collect()
        };
        if let Some(inner) = inner_positionals {
            if inner.len() >= 2 {
                // Two+ inner subcaptures: first = key, second = value
                (slot_text(&inner[0]), Some(slot_text(&inner[1])))
            } else if inner.len() == 1 {
                // One inner subcapture: it is the key, no value
                (slot_text(&inner[0]), None)
            } else {
                // No inner subcaptures in subcaps: use matched text
                let k: String = chars[from..to].iter().collect();
                (k, None)
            }
        } else {
            // No subcaptures: use matched text
            let k: String = chars[from..to].iter().collect();
            (k, None)
        }
    };
    store.push_hash_capture(name, (key, value));
}
