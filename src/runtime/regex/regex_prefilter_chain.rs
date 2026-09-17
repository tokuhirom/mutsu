//! ADR-0099 §5: a small NFA over the declarative leading run of a pattern —
//! the last piece of Stage 1 (#8272), and the mechanism the six earlier
//! slices' single-position derivations ([`super::regex_prefilter_firstset`],
//! [`super::regex_prefilter_composite`], [`super::regex_prefilter_subrule`])
//! are a special case of.
//!
//! # The gap this closes
//!
//! Every mechanism before this one answers one question: "may a match begin
//! with THIS character?" That rejects a start position using only its very
//! first character, so `/ \d\d\d /` scanning English prose still enters the
//! full engine at every isolated digit — the first-character set admits it
//! (it *is* a digit), and nothing before this module could look further
//! without walking the pattern's whole token tree per candidate position,
//! which is exactly the ~983-instruction cost Stage 1 exists to avoid
//! (ADR-0099 §2.4).
//!
//! This module answers the same question **for as many leading characters as
//! the pattern makes a fixed, declarative promise about**: a bounded sequence
//! of character-acceptance sets, one per offset from a candidate start, that
//! a position must satisfy *at every offset* to survive. `/ \d\d\d /`'s chain
//! is three digit-sets; a position with a digit at offset 0 but a letter at
//! offset 1 is rejected here, before the engine ever sees it.
//!
//! # Why this is the natural NFA (ADR-0099 §5)
//!
//! Concatenation, alternation and bounded repetition all compose the way a
//! textbook Thompson NFA's fragments do — a token consumes one character and
//! transitions onward; an alternation branches into several fragments that
//! rejoin once every branch is exhausted; a bounded repeat unrolls into that
//! many concatenated copies. What is deliberately NOT built is per-character
//! *correlated* subset simulation (tracking exactly which branch's edge a
//! specific subject character satisfied): every step's set is instead the
//! union of every live branch's requirement at that offset, checked
//! independently of what the previous offset's character actually was. That
//! is a strictly WEAKER claim than true NFA simulation would allow — it can
//! decline to narrow a position true simulation would have rejected — but it
//! is compositionally simple to build correctly and, critically, it is sound
//! by construction: a real match's character at offset `k` is guaranteed to
//! satisfy *some* live thread's requirement there, which is by definition a
//! member of that offset's union.
//!
//! # Why this cannot drift from the engine (ADR-0099 §4 constraint 1)
//!
//! Every leaf character set is produced by calling the exact same functions
//! the single-position mechanisms already call —
//! [`super::regex_prefilter_firstset::literal_first_set`],
//! [`super::regex_prefilter_firstset::class_first_set`],
//! [`super::regex_prefilter_firstset::unicode_prop_first_set`],
//! [`super::regex_prefilter_composite::composite_class_chain_set`] (factored
//! out of the existing composite-class derivation, now shared rather than
//! restated), and [`super::regex_prefilter_subrule`]'s resolver. This module
//! adds no new leaf predicate; it only composes existing ones across more
//! than one offset.
//!
//! # What "exact" means, and why most atoms cannot claim it
//!
//! A step may only be appended to a running chain when the atom that produced
//! it is provably consumed by EVERY match of this pattern at EXACTLY that
//! many characters — otherwise a later atom's position would be a guess. Only
//! a handful of atoms qualify: [`RegexAtom::Literal`], a
//! [`RegexAtom::CharClass`] with no [`ClassItem::Grapheme`] entry and no `\n`
//! member (the engine's `\r\n`-as-one-grapheme special case can otherwise
//! consume two characters for one class atom, see
//! `regex_match_atom_simple.rs`), [`RegexAtom::UnicodeProp`], and a
//! [`RegexAtom::CompositeClass`] whose grammar-token fallback cannot engage.
//! Everything else — `.`, a grapheme literal, `<.ws>`, a nullable quantifier,
//! `:i`/`:m` (multi-character folds and mark-stripping both break the
//! one-character-per-offset correspondence, ADR-0099 §4 constraint 2) —
//! contributes at most one final, non-extending step and stops the chain
//! there, exactly the way the single-position mechanisms already treat these
//! shapes as "no more to say" rather than as errors.
//!
//! # Declines
//!
//! Anything that can run user code, a backreference, `~` goal matching, and
//! left recursion all decline exactly as
//! [`super::regex_prefilter_analysis`] and [`super::regex_prefilter_subrule`]
//! already do — a construct strictly AFTER the chain's own steps only ever
//! stops the chain (sound: the engine would have failed on an earlier,
//! already-rejected atom before ever reaching it), but one BEFORE any step has
//! been pinned makes the whole chain unusable, mirroring
//! [`super::regex_prefilter_analysis::walk_pattern`]'s own nullable-gated
//! decline (in `walk_tokens`, its private helper).

use super::super::*;
use super::regex_prefilter_analysis::{Analyzer, Ctx, MAX_DEPTH};
use super::regex_prefilter_chain_atom::{chain_atom_once, is_zero_width_atom};
use super::regex_prefilter_firstset::FirstSet;
use crate::symbol::Symbol;

/// How many characters ahead the chain looks before giving up. Bounds both
/// the simulation cost per candidate position and the compile cost per
/// pattern — a pattern with a longer declarative run than this still gets a
/// chain, just truncated, which only costs precision, never soundness.
pub(super) const MAX_CHAIN_LEN: usize = 16;

/// The compiled chain: one character-acceptance set per offset from a
/// candidate start position.
pub(crate) struct Chain {
    steps: Vec<FirstSet>,
}

impl Chain {
    /// Whether `chars[start..]` survives every step. Sound in the same
    /// direction as every other Stage 1 mechanism: this only ever REJECTS a
    /// position the pattern truly cannot match, never one it can.
    pub(crate) fn admits(&self, chars: &[char], start: usize) -> bool {
        // Too little subject left to test every step is not this mechanism's
        // rejection to make — the minimum-length bound already prunes the
        // tail of the scan range, and claiming anything here would risk
        // rejecting a position on a step that never had a character to check.
        if start.saturating_add(self.steps.len()) > chars.len() {
            return true;
        }
        self.steps
            .iter()
            .enumerate()
            .all(|(i, step)| step.admits_at(chars, start + i))
    }
}

/// One chained-atom's contribution: the offsets it pins, and whether the
/// chain may safely keep extending past it into whatever follows.
///
/// Its fields stay private to this file; [`super::regex_prefilter_chain_atom`]
/// (the per-atom leaf dispatch) builds and reads one only through the
/// constructors and accessors below, so the two files cannot drift on what a
/// `Piece`'s own invariant — `exact` implies `steps.len()` is the atom's
/// EXACT total length — means.
pub(super) struct Piece {
    steps: Vec<FirstSet>,
    /// True when this atom consumes EXACTLY `steps.len()` characters on
    /// every path through it — the condition under which concatenating the
    /// next atom's own steps right after these stays sound.
    exact: bool,
}

impl Piece {
    /// A single mandatory character, safely followed by more.
    pub(super) fn one(set: FirstSet) -> Piece {
        Piece {
            steps: vec![set],
            exact: true,
        }
    }

    /// A single mandatory character that the chain may NOT extend past —
    /// for an atom that consumes exactly one *codepoint* here but may
    /// consume more of the subject overall (a grapheme cluster, `\r\n`), so
    /// the position right after it cannot be pinned.
    pub(super) fn final_step(set: FirstSet) -> Piece {
        Piece {
            steps: vec![set],
            exact: false,
        }
    }

    /// A zero-width construct: pins nothing, but the chain may still look
    /// straight through it at whatever comes next.
    pub(super) fn empty_exact() -> Piece {
        Piece {
            steps: Vec::new(),
            exact: true,
        }
    }

    /// Build directly from an already-computed step sequence — for
    /// [`super::regex_prefilter_chain_atom::merge_alternation`], which has to
    /// compute several branches' steps before it knows whether the result is
    /// exact.
    pub(super) fn from_steps(steps: Vec<FirstSet>, exact: bool) -> Piece {
        Piece { steps, exact }
    }

    /// The same claim, but never extending past it — for the `:i` downgrade,
    /// which keeps a leaf's own position-0 answer while declining to chain
    /// past it (ADR-0099 §4 constraint 2).
    pub(super) fn non_extending(self) -> Piece {
        Piece {
            exact: false,
            ..self
        }
    }

    pub(super) fn step_count(&self) -> usize {
        self.steps.len()
    }

    pub(super) fn step(&self, i: usize) -> &FirstSet {
        &self.steps[i]
    }

    pub(super) fn is_exact(&self) -> bool {
        self.exact
    }
}

/// The chain for `pattern` as seen from `pkg`, or `None` when it has fewer
/// than two usable steps — at that point it says nothing the existing
/// single-position first-character set does not already say, so there is
/// nothing worth the extra per-position check for.
pub(crate) fn build_chain(an: &mut Analyzer, pattern: &RegexPattern, pkg: Symbol) -> Option<Chain> {
    let piece = chain_pattern(an, pattern, pkg, 0)?;
    if piece.steps.len() < 2 || piece.steps.iter().all(FirstSet::is_universal) {
        return None;
    }
    Some(Chain { steps: piece.steps })
}

/// A pattern's own chain, handling the scoped-`:ignoremark` mapping the same
/// way [`super::regex_prefilter_analysis::walk_pattern`] does.
pub(super) fn chain_pattern(
    an: &mut Analyzer,
    pattern: &RegexPattern,
    pkg: Symbol,
    depth: u32,
) -> Option<Piece> {
    if pattern.ignore_mark {
        // The engine matches this sub-pattern as `strip_marks_pattern(p)`
        // against the mark-STRIPPED subject — the same derived tree the
        // matcher itself uses, off the same memo, per
        // `regex_prefilter_analysis::walk_pattern`'s own doc comment. What
        // comes out is a statement about stripped text, and stripping is not
        // injective on positions (both halves of a `\r\n` cluster map to the
        // cluster's start), so only the FIRST offset survives the mapping
        // back — a bound at any later offset would not be a lower bound in
        // the original and could prune a viable start.
        let stripped = super::regex_helpers::strip_marks_pattern(pattern);
        let mut piece = chain_pattern(an, &stripped, pkg, depth)?;
        if let Some(first) = piece.steps.first_mut() {
            first.set_mark_skewed();
        }
        piece.steps.truncate(1);
        piece.exact = false;
        return Some(piece);
    }
    chain_tokens(
        an,
        &pattern.tokens,
        Ctx {
            ignore_case: pattern.ignore_case,
            pkg,
            depth,
        },
    )
}

/// A token sequence's chain, extending it left to right while each token
/// stays exact.
fn chain_tokens(an: &mut Analyzer, tokens: &[RegexToken], ctx: Ctx) -> Option<Piece> {
    if ctx.depth > MAX_DEPTH {
        return None;
    }
    let mut steps: Vec<FirstSet> = Vec::new();
    for token in tokens {
        if steps.len() >= MAX_CHAIN_LEN {
            return Some(Piece {
                steps,
                exact: false,
            });
        }
        let Some(piece) = chain_token(an, token, ctx) else {
            // An opaque token before anything has been pinned leaves the
            // whole sequence unknown, mirroring `walk_tokens`'s own
            // nullable-gated decline; once something IS pinned, a later
            // opaque token only stops the chain there — the engine would
            // already have failed on the pinned atoms before ever reaching
            // it (see the module doc comment).
            return if steps.is_empty() {
                None
            } else {
                Some(Piece {
                    steps,
                    exact: false,
                })
            };
        };
        let room = MAX_CHAIN_LEN - steps.len();
        let truncated = piece.steps.len() > room;
        steps.extend(piece.steps.into_iter().take(room));
        if !piece.exact || truncated {
            return Some(Piece {
                steps,
                exact: false,
            });
        }
    }
    Some(Piece { steps, exact: true })
}

/// One token's chain, expanding its quantifier into the mandatory copies the
/// chain may rely on.
fn chain_token(an: &mut Analyzer, token: &RegexToken, ctx: Ctx) -> Option<Piece> {
    let deeper = Ctx {
        depth: ctx.depth + 1,
        ..ctx
    };
    // A `%`/`%%` separator sits BETWEEN repeated iterations, so unrolling more
    // than one mandatory copy of the atom itself would silently skip the
    // separator's own characters — a second, wrong, definition of what
    // occupies those offsets. Only the single-iteration case is safe here.
    let has_separator = token.separator.is_some();
    match &token.quant {
        RegexQuant::One => chain_atom_repeated(an, &token.atom, deeper, 1, true),
        RegexQuant::OneOrMore => chain_atom_repeated(an, &token.atom, deeper, 1, false),
        RegexQuant::Repeat(min, max) => {
            if *min == 0 {
                // Not even guaranteed once, so nothing about a subject
                // character can be pinned from it — unless it is itself
                // zero-width, which is safe to see straight through
                // regardless of how many times it repeats.
                zero_width_or_decline(&token.atom)
            } else if has_separator {
                chain_atom_repeated(an, &token.atom, deeper, 1, false)
            } else {
                chain_atom_repeated(an, &token.atom, deeper, *min, *max == Some(*min))
            }
        }
        RegexQuant::ZeroOrMore | RegexQuant::ZeroOrOne | RegexQuant::RepeatCode(_) => {
            zero_width_or_decline(&token.atom)
        }
    }
}

/// The one shape a nullable quantifier still permits: an atom that is
/// unconditionally zero-width, so the chain may look straight through it
/// however many (or few) times it repeats.
fn zero_width_or_decline(atom: &RegexAtom) -> Option<Piece> {
    if is_zero_width_atom(atom) {
        Some(Piece::empty_exact())
    } else {
        None
    }
}

/// `reps` mandatory concatenated copies of `atom`'s own chain, `fixed`
/// meaning the pattern can never require MORE than `reps` — repeating a
/// nested, already multi-step atom (a `Group`, an `Alternation`) works the
/// same way a plain literal repeated does, since each copy's own contribution
/// composes exactly like sequence concatenation.
fn chain_atom_repeated(
    an: &mut Analyzer,
    atom: &RegexAtom,
    ctx: Ctx,
    reps: usize,
    fixed: bool,
) -> Option<Piece> {
    if reps == 0 {
        return Some(Piece::empty_exact());
    }
    let mut steps: Vec<FirstSet> = Vec::new();
    for _ in 0..reps {
        if steps.len() >= MAX_CHAIN_LEN {
            return Some(Piece {
                steps,
                exact: false,
            });
        }
        let Some(once) = chain_atom_once(an, atom, ctx) else {
            return if steps.is_empty() {
                None
            } else {
                Some(Piece {
                    steps,
                    exact: false,
                })
            };
        };
        let room = MAX_CHAIN_LEN - steps.len();
        let truncated = once.steps.len() > room;
        let once_exact = once.exact;
        steps.extend(once.steps.into_iter().take(room));
        if !once_exact || truncated {
            return Some(Piece {
                steps,
                exact: false,
            });
        }
    }
    Some(Piece {
        steps,
        exact: fixed,
    })
}

#[cfg(test)]
#[path = "regex_prefilter_chain_tests.rs"]
mod tests;
