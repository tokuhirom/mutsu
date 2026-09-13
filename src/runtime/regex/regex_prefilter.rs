//! ADR-0099 Stage 1: a memoized static prefilter over an unanchored scan's
//! candidate start positions.
//!
//! Every unanchored scan (`~~ /pattern/`, `.subst`, `s:g///`, `.comb`,
//! `.contains`, `.grep` against a regex) enters the full backtracking engine
//! at every character position — ~983 instructions to establish that
//! `chars[i] != 'z'` (ADR-0099 §2.4). mutsu already ships a substring search
//! 10x faster than rakudo's (`.index`); the regex engine simply never used it,
//! which is 212x on the same subject for the same literal-only question.
//!
//! This module wires that existing primitive to the scan loops in
//! `regex_match_find.rs`, for the ONE case ADR-0099 §4 calls "not a new
//! optimization to invent — wiring an existing primitive": a pattern whose
//! entire body (or a leading run of it) is a plain, unconditional, non-`:i`
//! literal. Everything else — alternation-derived first-character sets, a
//! required *inner* literal for patterns with no usable prefix, `:i`
//! fold-closure first-sets, `:m` NFD-aware first-sets, subrule-derived
//! prefixes keyed by package + `TOKEN_DEFS_GEN` — is explicitly OUT of scope
//! here and simply declines (falls back to the unfiltered scan, exactly
//! today's behavior). Declining is always safe; only applying a WRONG
//! prefilter can silently drop a valid match, which is why this stays a
//! deliberately narrow slice rather than the full four-bullet feature list —
//! see the issue for what remains.
//!
//! Per ADR-0099 §4 constraint 1, this must be "the memoized static form of
//! `ltm_litlen_at`'s construction table, not a second definition" so the two
//! can never drift. [`required_literal_prefix`] satisfies that by
//! construction rather than by re-deriving the table: it recognizes only the
//! single simplest case in that table (a top-level run of `RegexQuant::One`,
//! uncaptured, non-interpolated `RegexAtom::Literal` tokens) and declines on
//! anything else. Every pattern shape this function calls "prefix P" is one
//! `regex_ltm_rank.rs`'s `ltm_litlen_walk` would also walk as pure literal for
//! exactly `P.len()` characters — the two cannot disagree because this one
//! recognizes a strict subset of the other's cases, not an independent
//! reading of the same pattern.

use super::super::*;

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

/// The required literal prefix of `pattern`, or `None` when the pattern has
/// no usable one (see the module doc comment for exactly which shapes
/// qualify). `:i`/`:m` decline unconditionally (ADR-0099 §4 constraint 2 —
/// fold-closure/NFD-aware first-sets are not implemented).
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
/// `chars`, from `from` onward inclusive. Narrowed to positions where
/// [`required_literal_prefix`]'s prefix actually occurs when one exists and
/// the kill switch is not `off`; otherwise every position in `[from,
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
    if prefilter_enabled()
        && let Some(prefix) = required_literal_prefix(pattern)
    {
        let needle: Vec<char> = prefix.chars().collect();
        let total = chars.len().saturating_sub(from);
        let scanned = ScanPositions::Literal {
            chars,
            needle,
            pos: from,
        };
        // Applied/declined + positions-skipped are recorded eagerly here
        // (not lazily per `next()`) because "applied" is a fact about the
        // SCAN, decided once, not about how far the caller happened to
        // iterate before an early return.
        crate::vm::vm_stats::record_regex_prefilter_applied(total);
        return scanned;
    }
    crate::vm::vm_stats::record_regex_prefilter_declined();
    ScanPositions::Range(from..=chars.len())
}

/// Iterator returned by [`regex_scan_positions`]. See that function's doc
/// comment for why this is an iterator rather than a `Vec`.
pub(crate) enum ScanPositions<'c> {
    Range(std::ops::RangeInclusive<usize>),
    Literal {
        chars: &'c [char],
        needle: Vec<char>,
        pos: usize,
    },
}

impl Iterator for ScanPositions<'_> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        match self {
            ScanPositions::Range(r) => r.next(),
            ScanPositions::Literal { chars, needle, pos } => {
                let needle_len = needle.len();
                while pos.saturating_add(needle_len) <= chars.len() {
                    let candidate = *pos;
                    *pos += 1;
                    if chars[candidate..candidate + needle_len] == needle[..] {
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
        let pattern = parse("'ab'");
        let chars: Vec<char> = "xxabxxabxx".chars().collect();
        let found: Vec<usize> = regex_scan_positions(&pattern, &chars, 0).collect();
        assert_eq!(found, vec![2, 6]);
    }

    #[test]
    fn scan_positions_respects_the_kill_switch() {
        // SAFETY (of the test, not unsafe code): env var mutation races other
        // tests reading MUTSU_REGEX_PREFILTER concurrently. `prefilter_enabled`
        // memoizes on first read process-wide, so this can only observe the
        // value already latched -- it does not assert a specific behavior,
        // only that declining still enumerates every position.
        let pattern = parse("'zz'");
        let chars: Vec<char> = "no zz here at all".chars().collect();
        // Whether or not the switch is on, `zz` really does occur once, so
        // this only pins that a real occurrence is never missed.
        let found: Vec<usize> = regex_scan_positions(&pattern, &chars, 0).collect();
        assert_eq!(found, vec![3]);
    }

    #[test]
    fn scan_positions_falls_back_to_every_position_without_a_prefix() {
        let pattern = parse(r"\d+");
        let chars: Vec<char> = "abc".chars().collect();
        let found: Vec<usize> = regex_scan_positions(&pattern, &chars, 0).collect();
        assert_eq!(found, vec![0, 1, 2, 3]);
    }
}
