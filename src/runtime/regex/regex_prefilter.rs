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
//! Still out of scope, and still simply declining: a required *inner* literal
//! for patterns with no usable prefix, subrule-derived prefixes keyed by
//! package + `TOKEN_DEFS_GEN`, and `:m` NFD-aware first-sets for a *scoped*
//! `:ignoremark` (a top-level one already arrives here mark-stripped).

use super::super::*;
use super::regex_prefilter_analysis::{Derivation, derive};
use super::regex_prefilter_firstset::FirstSet;
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
    prefix: Option<Box<[char]>>,
    /// A superset of the characters a match may begin with.
    first: Option<FirstSet>,
    /// A lower bound on the characters any match consumes.
    min_len: usize,
}

impl Prefilter {
    fn build(pattern: &RegexPattern) -> Prefilter {
        let Derivation { first, min_len } = derive(pattern);
        Prefilter {
            prefix: required_literal_prefix(pattern)
                .map(|p| p.chars().collect::<Vec<char>>().into_boxed_slice()),
            first,
            min_len,
        }
    }
}

/// This pattern's memoized [`Prefilter`].
pub(crate) fn pattern_prefilter(pattern: &RegexPattern) -> &Arc<Prefilter> {
    pattern
        .derived
        .prefilter
        .get_or_init(|| Arc::new(Prefilter::build(pattern)))
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
/// `chars`, from `from` onward inclusive. Narrowed by whichever of the
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
    pattern: &RegexPattern,
    chars: &'c [char],
    from: usize,
) -> ScanPositions<'c> {
    if !prefilter_enabled() {
        crate::vm::vm_stats::record_regex_prefilter_declined();
        return ScanPositions::Range(from..=chars.len());
    }
    let prefilter = Arc::clone(pattern_prefilter(pattern));
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

/// Iterator returned by [`regex_scan_positions`]. See that function's doc
/// comment for why this is an iterator rather than a `Vec`.
pub(crate) enum ScanPositions<'c> {
    Range(std::ops::RangeInclusive<usize>),
    /// Positions where the pattern's required literal prefix occurs.
    Literal {
        chars: &'c [char],
        prefilter: Arc<Prefilter>,
        pos: usize,
        /// Inclusive last viable start.
        last: usize,
    },
    /// Positions whose character is in the pattern's first-character set.
    FirstChar {
        chars: &'c [char],
        prefilter: Arc<Prefilter>,
        pos: usize,
        /// Inclusive last viable start.
        last: usize,
    },
}

impl ScanPositions<'_> {
    /// No candidate at all — the subject is shorter than any match could be.
    fn empty() -> Self {
        #[expect(clippy::reversed_empty_ranges, reason = "an empty RangeInclusive")]
        ScanPositions::Range(1..=0)
    }
}

impl Iterator for ScanPositions<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        match self {
            ScanPositions::Range(r) => r.next(),
            ScanPositions::Literal {
                chars,
                prefilter,
                pos,
                last,
            } => {
                let needle = prefilter.prefix.as_deref().unwrap_or_default();
                while *pos <= *last {
                    let candidate = *pos;
                    *pos += 1;
                    if chars[candidate..candidate + needle.len()] == *needle {
                        crate::vm::vm_stats::record_regex_prefilter_position_hit();
                        return Some(candidate);
                    }
                }
                None
            }
            ScanPositions::FirstChar {
                chars,
                prefilter,
                pos,
                last,
            } => {
                let set = prefilter.first.as_ref()?;
                while *pos <= *last {
                    let candidate = *pos;
                    *pos += 1;
                    if set.contains(chars[candidate]) {
                        crate::vm::vm_stats::record_regex_prefilter_position_hit();
                        return Some(candidate);
                    }
                }
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(pattern: &str) -> std::sync::Arc<RegexPattern> {
        let interp = crate::runtime::Interpreter::new();
        interp
            .parse_regex(pattern)
            .expect("pattern should parse for this test")
    }

    fn positions(pattern: &str, subject: &str) -> Vec<usize> {
        let parsed = parse(pattern);
        let chars: Vec<char> = subject.chars().collect();
        regex_scan_positions(&parsed, &chars, 0).collect()
    }

    #[test]
    fn plain_literal_is_a_usable_prefix() {
        assert_eq!(
            required_literal_prefix(&parse("'hello'")),
            Some("hello".to_string())
        );
        assert_eq!(
            required_literal_prefix(&parse("hello")),
            Some("hello".to_string())
        );
    }

    #[test]
    fn a_trailing_non_literal_still_yields_the_leading_prefix() {
        // "abc" must occur, whatever \d+ then requires -- the required
        // prefix is a valid (if not tight) necessary condition either way.
        assert_eq!(
            required_literal_prefix(&parse(r"abc \d+")),
            Some("abc".to_string())
        );
    }

    #[test]
    fn quantified_leading_literal_declines() {
        // "a" is optional here, so it is not REQUIRED at all.
        assert_eq!(required_literal_prefix(&parse("a? bc")), None);
    }

    #[test]
    fn case_insensitive_declines() {
        assert_eq!(required_literal_prefix(&parse(":i 'ABC'")), None);
    }

    #[test]
    fn ignoremark_declines() {
        assert_eq!(required_literal_prefix(&parse(":m 'cafe'")), None);
    }

    #[test]
    fn alternation_has_no_top_level_literal_prefix() {
        assert_eq!(required_literal_prefix(&parse("'foo' | 'bar'")), None);
    }

    #[test]
    fn a_named_capture_ends_the_chain_immediately() {
        assert_eq!(required_literal_prefix(&parse("$<x>=[a] bc")), None);
    }

    #[test]
    fn scan_positions_finds_the_literal_and_nothing_else() {
        assert_eq!(positions("'ab'", "xxabxxabxx"), vec![2, 6]);
    }

    #[test]
    fn scan_positions_respects_the_kill_switch() {
        // `prefilter_enabled` memoizes process-wide on first read, so a test
        // cannot flip it; this only pins that a real occurrence is never
        // missed either way.
        assert_eq!(positions("'zz'", "no zz here at all"), vec![3]);
    }

    #[test]
    fn a_subrule_call_declines_so_every_position_is_kept() {
        // A prefix derived through `<foo>` would have to be keyed by invocant
        // package and `TOKEN_DEFS_GEN` to survive a dynamic override
        // (ADR-0099 §4 constraint 3), so the analysis declines instead.
        assert_eq!(positions("<foo>", "abc"), vec![0, 1, 2, 3]);
    }

    #[test]
    fn a_character_class_narrows_the_scan_to_its_members() {
        assert_eq!(positions(r"\d+", "ab1cd2"), vec![2, 5]);
        assert_eq!(positions(r"<[xyz]>", "axbycz"), vec![1, 3, 5]);
    }

    #[test]
    fn a_negated_class_narrows_to_its_complement() {
        assert_eq!(positions(r"<-[abc]>", "abXcd"), vec![2, 4]);
    }

    #[test]
    fn an_alternation_unions_its_branches_first_characters() {
        // Only the two branch-leading characters are candidates -- and the
        // last two positions are ruled out by the 3-character minimum both
        // branches share.
        assert_eq!(positions("'foo' | 'bar'", "zfoozbarz"), vec![1, 5]);
    }

    #[test]
    fn ignorecase_uses_the_fold_closure_not_a_folded_needle() {
        // Both cases of the leading letter are candidates, and nothing else.
        assert_eq!(positions(":i 'z'", "aZbzc"), vec![1, 3]);
    }

    #[test]
    fn a_leading_zero_width_assertion_is_passed_through() {
        // `^^` consumes nothing, so the first-set is still the literal's.
        assert_eq!(positions("^^ 'a'", "xaya"), vec![1, 3]);
    }

    #[test]
    fn a_nullable_pattern_keeps_every_position() {
        assert_eq!(positions(r"\d*", "ab"), vec![0, 1, 2]);
    }

    #[test]
    fn a_leading_code_block_declines_so_it_still_runs_per_position() {
        // ADR-0009: a leading `{ … }` runs once per start position in both
        // mutsu and rakudo, so the prefilter must not skip any.
        assert_eq!(positions(r"{ 1 } 'z'", "ab"), vec![0, 1, 2]);
    }

    #[test]
    fn a_minimum_length_truncates_the_tail_of_the_range() {
        // `. . .` has a universal first-set (so no character filtering) but
        // still cannot start within two characters of the end.
        assert_eq!(positions(". . .", "abcde"), vec![0, 1, 2]);
        assert!(positions(". . .", "ab").is_empty());
    }

    #[test]
    fn a_literal_prefix_longer_than_the_subject_yields_nothing() {
        assert!(positions("'abcdef'", "abc").is_empty());
    }
}
