//! Separator quantifiers (`atom +% sep`, `atom **N..M %% sep`, ...): the atom
//! is matched repeatedly with `sep` interleaved between iterations, each
//! side's captures accumulated into its own folded group.
//!
//! Candidates are returned as *deltas* — `RegexCaptures` relative to an empty
//! baseline (ADR-0007); the engine merges the chosen candidate into its
//! capture store and rewinds on backtrack.

use super::super::*;
use super::regex_helpers::count_pattern_capture_groups;

/// How many positional slots one match of a separator pattern takes.
// Cost: O(1) once the pattern's count is memoized.
pub(super) fn separator_stride(sep: &RegexPattern) -> usize {
    count_pattern_capture_groups(sep)
}

/// [`separated_capture_delta`] for names already interned (the compiled
/// engine's, interned when the pattern compiled).
// Cost: O(n + c), n = the names, c = the captures across the chain.
pub(super) fn separated_capture_delta_syms(
    names: impl IntoIterator<Item = Symbol>,
    atom_caps: &[RegexCaptures],
    sep_caps: &[RegexCaptures],
    trailing: Option<&RegexCaptures>,
    atom_stride: usize,
    sep_stride: usize,
) -> RegexCaptures {
    let mut caps = RegexCaptures::default();
    for n in names {
        caps.named.slot_mut(n).quantified = true;
    }
    Interpreter::append_separated_captures(
        &mut caps,
        atom_caps,
        sep_caps,
        trailing,
        atom_stride,
        sep_stride,
    );
    caps
}

impl Interpreter {
    /// Append captures from a separated quantifier into `caps`, folding each
    /// side into its own positional/named group lists.
    pub(super) fn append_separated_captures(
        caps: &mut RegexCaptures,
        atom_caps: &[RegexCaptures],
        sep_caps: &[RegexCaptures],
        trailing_sep: Option<&RegexCaptures>,
        atom_stride: usize,
        sep_stride: usize,
    ) {
        // Positional captures: atom groups occupy the first `atom_stride` slots,
        // separator groups the next `sep_stride`. The folded slot keeps the
        // last iteration's span/subcap as its representative values.
        // An iteration's slot that an inner quantifier already folded
        // (`[ [ (\d) ] +% '.' ] +% ';'`) contributes all its entries: raku has
        // one flat list for a capture group under nested quantifiers.
        let fold_group = |sources: &[&RegexCaptures], g: usize| -> PosSlot {
            let mut list: Vec<QuantifiedCaptureEntry> = Vec::new();
            for src in sources {
                if let Some(slot) = src.positional.get(g) {
                    slot.push_entries_to(&mut list);
                }
            }
            PosSlot::folded(list)
        };
        let atom_refs: Vec<&RegexCaptures> = atom_caps.iter().collect();
        for g in 0..atom_stride {
            let slot = fold_group(&atom_refs, g);
            caps.positional.push(slot);
        }
        let mut all_sep: Vec<&RegexCaptures> = sep_caps.iter().collect();
        if let Some(ts) = trailing_sep {
            all_sep.push(ts);
        }
        for g in 0..sep_stride {
            let slot = fold_group(&all_sep, g);
            caps.positional.push(slot);
        }
        // Named captures: merge every iteration's named captures (as arrays)
        // in match order -- a0, s0, a1, s1, ..., then a trailing separator --
        // so a name both sides capture lists its entries as rakudo does
        // (#10574). Positional slots above stay side by side: they are
        // numbered by source position.
        let interleaved = (0..atom_caps.len().max(sep_caps.len()))
            .flat_map(|i| atom_caps.get(i).into_iter().chain(sep_caps.get(i)))
            .chain(trailing_sep);
        for src in interleaved {
            for (k, v) in &src.named {
                let slot = caps.named.slot_mut(*k);
                slot.merge(v.clone());
                slot.quantified = true;
            }
            for (k, v) in src.hash_captures() {
                caps.hash_captures_mut()
                    .entry(k.clone())
                    .or_default()
                    .extend(v.clone());
            }
        }
    }
}
