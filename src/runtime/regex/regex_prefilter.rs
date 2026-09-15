//! ADR-0099 Stage 1: a memoized static prefilter over an unanchored scan's
//! candidate start positions.
//!
//! Every unanchored scan (`~~ /pattern/`, `.subst`, `s:g///`, `.comb`,
//! `.contains`, `.grep` against a regex) enters the full backtracking engine
//! at every character position — ~983 instructions to establish that
//! `chars[i] != 'z'` (ADR-0099 §2.4). This module answers that same question
//! without entering the engine, three ways, in decreasing order of strength:
//!
//! - a **required literal prefix**, turning the scan into a substring search
//!   (mutsu already ships one 10x faster than rakudo's `.index`; the regex
//!   engine simply never used it, which is the 212x in the issue);
//! - a **first-character set**, a 128-bit ASCII bitmap plus a non-ASCII
//!   policy, rejecting a position in about one instruction instead of ~983;
//! - a **minimum match length**, truncating the tail of the start range.
//!
//! The engine below is untouched, and a prefilter that declines is exactly
//! the status quo: the caller walks every position, as it did before this
//! module existed. Only applying a WRONG prefilter can silently drop a valid
//! match, so everything here over-approximates — see
//! [`super::regex_prefilter_analysis`], which derives the first-set and the
//! length bound and documents what it declines on.
//!
//! Per ADR-0099 §4 constraint 1, none of this may become a second definition
//! of what the engine matches. [`required_literal_prefix`] satisfies that by
//! recognizing a strict *subset* of `regex_ltm_rank.rs`'s `ltm_litlen_walk`
//! cases rather than re-deriving its table (see its own doc comment), and the
//! first-set analysis satisfies it by calling the engine's own class
//! evaluator instead of restating it.
//!
//! A fourth derivation, the **required inner literal**, catches the patterns
//! the first two leave behind — `/ \w+ '=>' /` has no leading prefix and a
//! first-set as wide as `\w`, but `'=>'` must still appear somewhere in any
//! match, so one substring search answers the whole scan. It lives in
//! [`super::regex_prefilter_inner`], which states the claim it makes exactly.
//!
//! The first-set analysis also looks *through* a `<subrule>`, which is what
//! ADR-0099 §4 constraint 3 asks for ("keyed by invocant package and
//! `TOKEN_DEFS_GEN` ... or decline") rather than the decline the first three
//! slices took. That makes such a derivation no longer a pure function of the
//! pattern, so it lives in a second, package-and-generation-keyed memo —
//! [`pattern_prefilter_in_pkg`] — while a pattern that mentions no rule name
//! keeps the single pattern-keyed slot and the path it had before. The
//! resolution and everything it still declines on are in
//! [`super::regex_prefilter_subrule`].
//!
//! A *scoped* `:ignoremark` is derived too, rather than sinking the pattern it
//! sits in: the engine matches such a sub-pattern against the mark-stripped
//! subject, so the analysis walks the same stripped tree the matcher does and
//! flags the resulting set as a statement about *stripped* text — which
//! `FirstSet::admits_at` maps back onto the original subject. (A top-level
//! `:ignoremark` already arrives here mark-stripped.)
//!
//! Still out of scope, and still simply declining: a `<+a -b>` composite class
//! (a `<subrule>` in disguise — see [`super::regex_prefilter_analysis`]), and
//! the required literal prefix / required inner literal through a rule name
//! (both make a claim about *text*, and the inner literal's decline on
//! anything that can run code is deliberately stronger than the first-set's —
//! see [`super::regex_prefilter_inner`]).

use super::super::*;
use super::regex_prefilter_analysis::{Analyzer, Derivation, derive};
use super::regex_prefilter_firstset::FirstSet;
use super::regex_prefilter_inner::{InnerLiteral, required_inner_literal};
use super::regex_prefilter_memo::{mentions_subrule, pattern_prefilter, pattern_prefilter_in_pkg};
use super::regex_prefilter_scan::ScanPositions;
use crate::symbol::Symbol;
use crate::vm::vm_stats::RegexPrefilterKind;
use std::sync::Arc;

/// Whether the prefilter is enabled. `MUTSU_REGEX_PREFILTER=off` is the kill
/// switch the ADR's testing section asks for: a bisect handle in production,
/// and what makes the differential property test
/// (`tests/regex_prefilter_differential.rs`) cheap to write (run the same
/// corpus with it forced off and compare).
#[inline]
pub(crate) fn prefilter_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| {
        std::env::var_os("MUTSU_REGEX_PREFILTER").as_deref() != Some(std::ffi::OsStr::new("off"))
    })
}

/// Everything derived from one parsed pattern that can narrow a scan.
///
/// Derived once per pattern and cached in its `PatternDerived`, because
/// static patterns are shared through the regex parse cache: the derivation
/// walks the whole token tree and evaluates every character class over the
/// ASCII range, which would swamp the scan it saves if it were repeated per
/// `regex_scan_positions` call (a `:g` loop makes one call per match).
pub(crate) struct Prefilter {
    /// A literal run every match must begin with.
    pub(super) prefix: Option<Box<[char]>>,
    /// A superset of the characters a match may begin with.
    pub(super) first: Option<FirstSet>,
    /// A literal run every match must *contain*, for the patterns that have no
    /// usable prefix. Bounds the match start rather than fixing it.
    pub(super) inner: Option<InnerLiteral>,
    /// A lower bound on the characters any match consumes.
    min_len: usize,
}

impl Prefilter {
    pub(super) fn build(an: &mut Analyzer, pattern: &RegexPattern, pkg: Symbol) -> Prefilter {
        let Derivation { first, min_len } = derive(an, pattern, pkg);
        let prefix = required_literal_prefix(pattern)
            .map(|p| p.chars().collect::<Vec<char>>().into_boxed_slice());
        Prefilter {
            // A required prefix is strictly stronger than a required inner
            // literal (it fixes the start rather than bounding it), so the
            // inner analysis is not even run when one exists.
            inner: if prefix.is_some() {
                None
            } else {
                required_inner_literal(pattern)
            },
            prefix,
            first,
            min_len,
        }
    }
}

/// The required literal prefix of `pattern`, or `None` when the pattern has
/// no usable one.
///
/// Recognizes only the single simplest case in ADR-0022 §4.3's
/// declarative-leading-literal construction table — a top-level run of
/// `RegexQuant::One`, uncaptured, non-interpolated `RegexAtom::Literal`
/// tokens — and declines on everything else. That is what makes it the
/// memoized static form of `ltm_litlen_at` rather than a second definition of
/// it (ADR-0099 §4 constraint 1): every pattern shape this calls "prefix P"
/// is one `regex_ltm_rank.rs`'s `ltm_litlen_walk` would also walk as pure
/// literal for exactly `P.len()` characters, because this recognizes a strict
/// subset of that walk's cases rather than an independent reading of the same
/// pattern.
///
/// `:i` / `:m` decline: a case-folded literal is *variable-length* thanks to
/// multi-character folds (`ß`/`SS`, `ﬁ`/`fi`), so searching for a folded
/// needle would be unsound (ADR-0099 §4 constraint 2). Those patterns are
/// narrowed by the first-character set instead, which needs no fixed-length
/// needle.
pub(crate) fn required_literal_prefix(pattern: &RegexPattern) -> Option<String> {
    if pattern.ignore_case || pattern.ignore_mark {
        return None;
    }
    let mut prefix = String::new();
    for token in &pattern.tokens {
        // Mirrors `ltm_litlen_walk`'s own chain-ending conditions exactly
        // (regex_ltm_rank.rs) for the plain-literal case: a non-constant
        // interpolated literal, any non-`One` quantifier (with or without a
        // separator), or a capture alias all end the declarative chain there,
        // whatever accumulated before it stays valid as a required prefix.
        if token.from_runtime_interpolation {
            break;
        }
        if !matches!(token.quant, RegexQuant::One) || token.separator.is_some() {
            break;
        }
        if token.named_capture.is_some()
            || token.secondary_named_capture.is_some()
            || token.hash_capture.is_some()
        {
            break;
        }
        match &token.atom {
            RegexAtom::Literal(ch) => prefix.push(*ch),
            // A grapheme literal is still a fixed run of codepoints, so it
            // extends the required prefix like any other literal.
            RegexAtom::LiteralGrapheme(g) => prefix.extend(g.chars()),
            _ => break,
        }
    }
    if prefix.is_empty() {
        None
    } else {
        Some(prefix)
    }
}

/// Candidate start positions for an unanchored scan of `pattern` over
/// `chars`, from `from` onward inclusive, as invoked from `pkg`. Narrowed by whichever of the
/// prefilter's mechanisms applies; otherwise every position in `[from,
/// chars.len()]`, exactly as before this module existed.
///
/// Returns a lazy iterator, not a `Vec`: the common case is "try the first
/// few positions and return on the first match", and the un-prefiltered
/// branch must stay a zero-allocation `Range` for that case — building the
/// full remaining-position list up front for every scan (as one call site
/// already did before this module) is exactly the per-scan cost Stage 0
/// exists to remove elsewhere, and this module must not reintroduce it here.
pub(crate) fn regex_scan_positions<'c>(
    interp: &mut Interpreter,
    pattern: &RegexPattern,
    chars: &'c [char],
    from: usize,
    pkg: Symbol,
) -> ScanPositions<'c> {
    if !prefilter_enabled() {
        crate::vm::vm_stats::record_regex_prefilter_declined();
        return ScanPositions::Range(from..=chars.len());
    }
    // Only a pattern that names a rule needs the package-keyed derivation;
    // every other one keeps the single pattern-keyed slot, so the common case
    // pays neither the lock nor the generation read.
    let prefilter = if mentions_subrule(pattern) {
        pattern_prefilter_in_pkg(interp, pattern, pkg)
    } else {
        Arc::clone(pattern_prefilter(pattern))
    };
    // Applied/declined + positions-offered are recorded eagerly here (not
    // lazily per `next()`) because "applied" is a fact about the SCAN, decided
    // once, not about how far the caller happened to iterate before an early
    // return.
    let offered = chars.len().saturating_sub(from).saturating_add(1);

    if let Some(prefix) = prefilter.prefix.as_deref() {
        // A match must both contain the prefix at its start and be long
        // enough overall, so the last viable start is bounded by the stricter
        // of the two.
        let Some(last) = chars.len().checked_sub(prefix.len().max(prefilter.min_len)) else {
            return ScanPositions::empty();
        };
        crate::vm::vm_stats::record_regex_prefilter_applied(
            RegexPrefilterKind::LiteralPrefix,
            offered,
        );
        return ScanPositions::Literal {
            chars,
            prefilter,
            pos: from,
            last,
        };
    }

    if let Some(inner) = prefilter.inner.as_ref() {
        // A match holds `min_before` characters plus the literal itself, which
        // is a tighter bound than the derived minimum whenever the walk that
        // found the literal saw further than the first-set walk did.
        let needed = prefilter
            .min_len
            .max(inner.min_before.saturating_add(inner.literal.len()));
        let Some(last) = chars.len().checked_sub(needed) else {
            return ScanPositions::empty();
        };
        crate::vm::vm_stats::record_regex_prefilter_applied(
            RegexPrefilterKind::InnerLiteral,
            offered,
        );
        return ScanPositions::Inner {
            chars,
            prefilter,
            pos: from,
            last,
            search: from,
            window_end: None,
            exhausted: false,
        };
    }

    if prefilter.first.is_some() {
        // A pattern with a first-character set consumes at least one
        // character by construction, so `chars.len()` is never a viable start
        // even when no larger bound was derived.
        let Some(last) = chars.len().checked_sub(prefilter.min_len.max(1)) else {
            return ScanPositions::empty();
        };
        crate::vm::vm_stats::record_regex_prefilter_applied(
            RegexPrefilterKind::FirstCharSet,
            offered,
        );
        return ScanPositions::FirstChar {
            chars,
            prefilter,
            pos: from,
            last,
        };
    }

    crate::vm::vm_stats::record_regex_prefilter_declined();
    // Even a pattern with no usable first-set can carry a length bound (`/. . ./`
    // derives one), and trimming the tail of the range costs nothing.
    match chars.len().checked_sub(prefilter.min_len) {
        Some(last) => ScanPositions::Range(from..=last),
        None => ScanPositions::empty(),
    }
}

#[cfg(test)]
#[path = "regex_prefilter_tests.rs"]
mod tests;
