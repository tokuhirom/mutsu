//! The **required inner literal**: a literal run that every match must
//! contain, for the patterns that have no usable *leading* literal prefix.
//!
//! This is the third of ADR-0099 Stage 1's four derivations (the first two —
//! required literal prefix and first-character set — live in
//! [`super::regex_prefilter`] and [`super::regex_prefilter_analysis`]).
//! `/ \w+ '=>' /` has no leading prefix and a first-character set as wide as
//! `\w`, so both of those leave a 640 KB failing scan entering the engine at
//! essentially every position. But `'=>'` must appear *somewhere* in any
//! match, so one substring search over the subject answers the whole scan:
//! no occurrence, no match, zero engine entries.
//!
//! # The claim, stated exactly
//!
//! A derivation of `(literal, min_before, max_before)` asserts: **every match
//! of this pattern contains `literal` at some absolute position `p`, with
//! `start + min_before <= p <= start + max_before`** (`max_before: None`
//! meaning unbounded). Contrapositively, a start position is viable only if
//! some occurrence `p` of `literal` satisfies `p - max_before <= start <= p -
//! min_before` — which is what [`super::regex_prefilter`]'s `Inner` scan
//! iterates, and why the filter degenerates gracefully: with no occurrence at
//! all the scan is empty, and with `max_before` unbounded it still rules out
//! every start beyond the last occurrence.
//!
//! Both bounds must therefore be *wide*: `min_before` a lower bound on the
//! characters consumed before the literal, `max_before` an upper bound (or
//! `None`). Anything the walk cannot bound contributes `(0, None)`, and
//! anything it cannot reason about at all declines the derivation outright —
//! which is exactly the status quo, an unnarrowed scan.
//!
//! # Why this cannot drift from the engine (ADR-0099 §4 constraint 1)
//!
//! The literal run it looks for is a run of [`RegexAtom::Literal`] tokens at
//! quantifier `One`, and `regex_match_atom_simple.rs` matches one of those
//! with `*ch == c` against a single subject character — an exact character
//! identity, not a predicate this module could restate differently. Every
//! other atom is opaque here: it contributes only a length bound, never a
//! claim about what it matches. So unlike the first-character set (which has
//! to call the engine's class evaluator to avoid a second definition of `\w`),
//! this analysis has no predicate to duplicate in the first place.
//!
//! # Declines
//!
//! - **`:i` / `:m` anywhere on the path.** A case-folded literal is
//!   *variable-length* (`ß`/`SS`), so neither the needle nor the offsets
//!   survive; a mark-stripped sub-pattern is matched against different
//!   characters than the ones in hand. Same reasoning as the literal prefix
//!   (ADR-0099 §4 constraint 2).
//! - **Any pattern that can run user code** ([`pattern_runs_code`], which also
//!   covers `<subrule>` per constraint 3). This decline is *stronger* than the
//!   first-set analysis's, and deliberately so: the first-set only ever skips
//!   positions where the leading atom itself fails, so a `{ … }` block placed
//!   after it would not have run there anyway — whereas skipping a position
//!   because a literal is missing *later* in the pattern skips a block that
//!   would have run (ADR-0009: a code assertion runs once per start position
//!   in both mutsu and rakudo).
//! - **`<.ws>`**, which dispatches to whatever `ws` the enclosing grammar
//!   resolves to — a subrule call by another name (constraint 3).
//! - **`~` goal matching** ([`RegexAtom::TildeMarker`] / [`RegexAtom::GoalMatch`]),
//!   whose tokens do not compose as a plain concatenation.

use super::super::*;
use super::regex_prefilter_analysis::pattern_runs_code;

/// A literal run every match must contain, and where it can sit relative to
/// the match start. See the module doc comment for the exact claim.
pub(crate) struct InnerLiteral {
    pub(crate) literal: Box<[char]>,
    /// Lower bound on the characters consumed before the literal run.
    pub(crate) min_before: usize,
    /// Upper bound on the same, or `None` when unbounded.
    pub(crate) max_before: Option<usize>,
}

/// Guard against a pathologically nested pattern recursing into a stack
/// overflow. Reaching it declines, like any other unanswerable shape.
const MAX_DEPTH: u32 = 48;

/// How many characters a construct consumes: `[min, max]`, `max: None` meaning
/// unbounded.
#[derive(Clone, Copy)]
struct Span {
    min: usize,
    max: Option<usize>,
}

impl Span {
    const ZERO: Span = Span {
        min: 0,
        max: Some(0),
    };
    /// "At least nothing, at most anything" — what every construct this module
    /// declines to measure contributes.
    const OPAQUE: Span = Span { min: 0, max: None };

    fn exactly(n: usize) -> Span {
        Span {
            min: n,
            max: Some(n),
        }
    }

    fn concat(self, other: Span) -> Span {
        Span {
            min: self.min.saturating_add(other.min),
            max: match (self.max, other.max) {
                (Some(a), Some(b)) => a.checked_add(b),
                _ => None,
            },
        }
    }

    fn union(self, other: Span) -> Span {
        Span {
            min: self.min.min(other.min),
            max: match (self.max, other.max) {
                (Some(a), Some(b)) => Some(a.max(b)),
                _ => None,
            },
        }
    }

    fn repeat(self, min_reps: usize, max_reps: Option<usize>) -> Span {
        Span {
            min: self.min.saturating_mul(min_reps),
            max: match (self.max, max_reps) {
                // A construct that consumes nothing still consumes nothing
                // however often it repeats, so an unbounded repetition count
                // does not make the span unbounded.
                (Some(0), _) => Some(0),
                (Some(a), Some(b)) => a.checked_mul(b),
                _ => None,
            },
        }
    }
}

/// One literal run found during the walk, with the span of everything before
/// it within the *enclosing* sequence.
struct Candidate {
    literal: Vec<char>,
    before: Span,
}

/// The best required inner literal for `pattern`, or `None` when it has none.
///
/// Only called for patterns with no usable leading prefix (that filter is both
/// stronger and cheaper), so a run starting at offset zero is not special-cased
/// here — it simply cannot arise for a pattern the prefix analysis declined.
pub(crate) fn required_inner_literal(pattern: &RegexPattern) -> Option<InnerLiteral> {
    // A code assertion, a closure interpolation or a subrule call must keep
    // running once per start position (ADR-0009, and constraint 3 for the
    // subrule), which skipping positions on a later literal's account would
    // silently stop doing.
    if pattern_runs_code(pattern, 0) {
        return None;
    }
    let (_, candidates) = walk_pattern(pattern, 0)?;
    candidates
        .into_iter()
        // Longest first: a longer needle is both rarer in the subject and
        // cheaper per rejected position. A bounded `max_before` breaks ties
        // (it turns each occurrence into a short window of starts rather than
        // "everything up to here"), and an earlier run breaks what is left.
        .max_by_key(|c| {
            (
                c.literal.len(),
                c.before.max.is_some(),
                usize::MAX - c.before.min,
            )
        })
        .map(|c| InnerLiteral {
            literal: c.literal.into_boxed_slice(),
            min_before: c.before.min,
            max_before: c.before.max,
        })
}

/// Walk one sub-pattern, returning its own span and every literal run inside
/// it (each with the span of what precedes it *within this sub-pattern*).
fn walk_pattern(pattern: &RegexPattern, depth: u32) -> Option<(Span, Vec<Candidate>)> {
    if depth > MAX_DEPTH {
        return None;
    }
    // Under `:i` a pattern character and a subject character are not one-to-one
    // (a multi-character fold lets `'ss'` match the single character `ß`), so
    // neither the needle nor the offsets are meaningful; `:m` matches against a
    // separately mark-stripped subject. Both are opaque rather than fatal: an
    // enclosing sequence can still carry a literal run of its own.
    if pattern.ignore_case || pattern.ignore_mark {
        return Some((Span::OPAQUE, Vec::new()));
    }

    let mut before = Span::ZERO;
    let mut candidates: Vec<Candidate> = Vec::new();
    let mut run: Vec<char> = Vec::new();
    let mut run_before = Span::ZERO;

    for token in &pattern.tokens {
        if matches!(
            token.atom,
            RegexAtom::TildeMarker | RegexAtom::GoalMatch { .. }
        ) {
            // `a ~ b c` does not compose as a plain concatenation, so nothing
            // in this sequence can be claimed as required.
            return None;
        }
        if let Some(ch) = plain_literal_char(token) {
            if run.is_empty() {
                run_before = before;
            }
            run.push(ch);
            before = before.concat(Span::exactly(1));
            continue;
        }
        flush(&mut run, run_before, &mut candidates);

        let (span, inner) = token_span_and_candidates(token, depth)?;
        // A literal inside a token only stays required if the token itself is
        // mandatory and matched exactly once; `token_span_and_candidates`
        // returns inner candidates only in that case, and their `before` is
        // relative to the token, so it composes onto what precedes the token.
        candidates.extend(inner.into_iter().map(|c| Candidate {
            literal: c.literal,
            before: before.concat(c.before),
        }));
        before = before.concat(span);
    }
    flush(&mut run, run_before, &mut candidates);

    Some((before, candidates))
}

fn flush(run: &mut Vec<char>, run_before: Span, candidates: &mut Vec<Candidate>) {
    if !run.is_empty() {
        candidates.push(Candidate {
            literal: std::mem::take(run),
            before: run_before,
        });
    }
}

/// The character a token contributes to a required literal run, when it is one
/// un-quantified, un-separated, non-interpolated [`RegexAtom::Literal`].
///
/// Captures are *not* excluded, unlike in the leading-prefix analysis: an alias
/// ends ADR-0022's declarative chain, but it changes nothing about the text the
/// atom matches, and text is the only claim this module makes.
/// [`RegexAtom::LiteralGrapheme`] is excluded — it matches a whole cluster
/// under normalization rules the raw character comparison here does not model.
fn plain_literal_char(token: &RegexToken) -> Option<char> {
    let RegexAtom::Literal(ch) = token.atom else {
        return None;
    };
    (matches!(token.quant, RegexQuant::One)
        && token.separator.is_none()
        && !token.from_runtime_interpolation)
        .then_some(ch)
}

/// A token's span, plus any required literal runs nested inside it.
fn token_span_and_candidates(token: &RegexToken, depth: u32) -> Option<(Span, Vec<Candidate>)> {
    let (atom_span, inner) = atom_span_and_candidates(&token.atom, depth)?;
    let (min_reps, max_reps) = match &token.quant {
        RegexQuant::One => (1usize, Some(1usize)),
        RegexQuant::OneOrMore => (1, None),
        RegexQuant::ZeroOrMore => (0, None),
        RegexQuant::ZeroOrOne => (0, Some(1)),
        RegexQuant::Repeat(min, max) => (*min, *max),
        // `** {code}` picks its count by running user code, which
        // `pattern_runs_code` has already declined the whole pattern for; this
        // arm only keeps the match total.
        RegexQuant::RepeatCode(_) => return None,
    };
    let exactly_once = min_reps == 1 && max_reps == Some(1);
    let mut span = atom_span.repeat(min_reps, max_reps);
    if token.separator.is_some() && !exactly_once {
        // A `%` / `%%` separator sits between iterations, so it can only widen
        // the span. Its own length is not worth measuring: the quantifier it
        // decorates is already the imprecise part.
        span.max = None;
    }
    // A repeated or optional token's content is not required at any fixed
    // offset, so only a token matched exactly once passes its literals up.
    Some((span, if exactly_once { inner } else { Vec::new() }))
}

fn atom_span_and_candidates(atom: &RegexAtom, depth: u32) -> Option<(Span, Vec<Candidate>)> {
    let deeper = depth + 1;
    let span = match atom {
        // The only atom whose text this module claims anything about; see the
        // module doc comment. Exactly one subject character
        // (`regex_match_atom_simple.rs`'s `Some(pos + 1)`).
        RegexAtom::Literal(_) => Span::exactly(1),
        // Every other consuming atom matches a whole grapheme cluster, so it
        // consumes at least one character and no fixed number of them.
        RegexAtom::LiteralGrapheme(_)
        | RegexAtom::CharClass(_)
        | RegexAtom::Any
        | RegexAtom::Newline
        | RegexAtom::NotNewline
        | RegexAtom::UnicodeProp { .. }
        | RegexAtom::CompositeClass { .. } => Span { min: 1, max: None },
        // Zero-width assertions constrain the position without consuming it.
        RegexAtom::ZeroWidth
        | RegexAtom::CaptureStartMarker
        | RegexAtom::CaptureEndMarker
        | RegexAtom::UnicodePropAssert { .. }
        | RegexAtom::LeftWordBoundary
        | RegexAtom::RightWordBoundary
        | RegexAtom::WordBoundary { .. }
        | RegexAtom::WithinWord { .. }
        | RegexAtom::StartOfLine
        | RegexAtom::EndOfLine
        | RegexAtom::EndOfString
        | RegexAtom::SameAssertion { .. }
        | RegexAtom::AtPosition(_)
        | RegexAtom::Lookaround { .. } => Span::ZERO,
        RegexAtom::Group(p)
        | RegexAtom::CaptureGroup(p)
        | RegexAtom::CaptureIsolatedGroup(p)
        | RegexAtom::CaptureIsolatedGroupScoped(p, _) => {
            return walk_pattern(p, deeper);
        }
        // Any branch may be the one that matches, so no branch's literal is
        // required — but the span is still bounded by the branches'.
        RegexAtom::Alternation(branches) | RegexAtom::SequentialAlternation(branches) => {
            let mut span: Option<Span> = None;
            for branch in branches {
                let (branch_span, _) = walk_pattern(branch, deeper)?;
                span = Some(match span {
                    None => branch_span,
                    Some(acc) => acc.union(branch_span),
                });
            }
            span.unwrap_or(Span::ZERO)
        }
        // Every branch matches the same text at the same position, so any one
        // branch's literals are required — but taking them would need the
        // branch's own offsets to agree with the conjunction's, which is not
        // worth establishing for how rare `&&` is. The span is the tightest
        // branch's.
        RegexAtom::Conjunction(branches) => {
            let mut span = Span::OPAQUE;
            for branch in branches {
                let (branch_span, _) = walk_pattern(branch, deeper)?;
                span = Span {
                    min: span.min.max(branch_span.min),
                    max: match (span.max, branch_span.max) {
                        (Some(a), Some(b)) => Some(a.min(b)),
                        (Some(a), None) | (None, Some(a)) => Some(a),
                        (None, None) => None,
                    },
                };
            }
            span
        }
        // `<.ws>` dispatches to whatever `ws` the enclosing grammar resolves
        // to (`regex_call_graph.rs` records exactly that edge), which is a
        // subrule call by another name — constraint 3, and possibly user code.
        RegexAtom::WsRule => return None,
        // Matching text that is not known until match time, and the code-ish
        // atoms `pattern_runs_code` has already declined the pattern for.
        RegexAtom::Backref(_)
        | RegexAtom::NamedBackref(_)
        | RegexAtom::VarInterp(_)
        | RegexAtom::Named(_)
        | RegexAtom::CodeAssertion { .. }
        | RegexAtom::ClosureInterpolation { .. }
        | RegexAtom::VarDecl { .. }
        | RegexAtom::RecurseSelf(_) => Span::OPAQUE,
        // Rejected at the top of `walk_pattern`.
        RegexAtom::TildeMarker | RegexAtom::GoalMatch { .. } => return None,
    };
    Some((span, Vec::new()))
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

    fn inner(pattern: &str) -> Option<(String, usize, Option<usize>)> {
        required_inner_literal(&parse(pattern)).map(|i| {
            (
                i.literal.iter().collect::<String>(),
                i.min_before,
                i.max_before,
            )
        })
    }

    #[test]
    fn a_literal_after_an_unbounded_quantifier_is_required() {
        assert_eq!(inner(r"\w+ '=>'"), Some(("=>".to_string(), 1, None)));
    }

    #[test]
    fn an_optional_leading_literal_gives_a_bounded_window() {
        assert_eq!(inner("'a'? 'bc'"), Some(("bc".to_string(), 0, Some(1))));
    }

    #[test]
    fn a_fixed_width_prefix_gives_an_exact_offset() {
        assert_eq!(inner("... ':'"), Some((":".to_string(), 3, None)));
    }

    #[test]
    fn the_longest_run_wins() {
        assert_eq!(
            inner(r"\d+ '-' \d+ 'END'"),
            Some(("END".to_string(), 3, None))
        );
    }

    #[test]
    fn a_literal_inside_a_mandatory_group_still_counts() {
        assert_eq!(inner(r"\d+ [ '=>' ] \s"), Some(("=>".to_string(), 1, None)));
    }

    #[test]
    fn a_literal_inside_an_optional_group_does_not() {
        assert_eq!(inner(r"\d+ [ '=>' ]?"), None);
    }

    #[test]
    fn a_literal_inside_an_alternation_branch_does_not() {
        assert_eq!(inner(r"\d+ [ '=>' | '->' ]"), None);
    }

    #[test]
    fn a_quantified_literal_is_not_a_run() {
        // `'x'+` still requires one `x`, but treating it as a run would claim
        // the WHOLE run at a fixed offset; only the un-quantified case does.
        assert_eq!(inner(r"\d+ 'x'+ 'y'"), Some(("y".to_string(), 2, None)));
    }

    #[test]
    fn ignorecase_declines() {
        assert_eq!(inner(r":i \w+ '=>'"), None);
    }

    #[test]
    fn a_scoped_ignorecase_group_is_opaque_but_not_fatal() {
        assert_eq!(inner(r"[:i \w+ ] '=>'"), Some(("=>".to_string(), 0, None)));
    }

    #[test]
    fn a_code_assertion_declines_so_it_keeps_running_per_position() {
        assert_eq!(inner(r"\w+ { 1 } '=>'"), None);
    }

    #[test]
    fn a_subrule_declines() {
        assert_eq!(inner(r"<foo> '=>'"), None);
    }

    #[test]
    fn a_pattern_with_no_literal_at_all_has_none() {
        assert_eq!(inner(r"\d+ \w+"), None);
    }

    #[test]
    fn a_leading_lookahead_is_zero_width() {
        assert_eq!(
            inner(r"<?before \d> \w+ '!'"),
            Some(("!".to_string(), 1, None))
        );
    }

    #[test]
    fn a_backreference_is_opaque_but_not_fatal() {
        assert_eq!(inner(r"(\w) $0 'zz'"), Some(("zz".to_string(), 1, None)));
    }
}
