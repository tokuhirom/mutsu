//! The static analysis behind the ADR-0099 Stage 1 scan prefilter: one pass
//! over a parsed [`RegexPattern`] deriving a **first-character set** and a
//! **minimum match length**, memoized alongside the pattern (see
//! [`super::regex_prefilter`] for how the result is used, and
//! `PatternDerived` for where it is cached).
//!
//! # What "sound" means here
//!
//! Every value this module produces is an *over-approximation of what can
//! match*, never an under-approximation:
//!
//! - the first-character set is a **superset** of the characters a match may
//!   begin with, so a position it rejects provably cannot start a match;
//! - the minimum length is a **lower bound** on the characters any match
//!   consumes, so a position within that many characters of the end provably
//!   cannot start one either.
//!
//! Anything the analysis cannot decide therefore widens (a universal set, a
//! bound of zero) or declines outright — never narrows on a guess. Declining
//! is exactly the status quo: the caller enumerates every position, as it did
//! before this module existed.
//!
//! # Why the engine's own predicates are called rather than re-stated
//!
//! ADR-0099 §4 constraint 1 is that the prefilter must not become a second,
//! independently-written definition of what the engine matches, because the
//! two would drift and the drift would be silent — a dropped valid match, not
//! an error. That applies to character classes just as much as to the
//! declarative literal prefix: `\w`, `<:Lu>`, `<[a..z]>` and every negated or
//! `:i` combination of them have a single definition in
//! [`super::regex_eval_class::class_matches_ignorecase`], and the per-atom
//! first-set constructors this module composes ([`class_first_set`] and its
//! neighbours) derive a class's set by *calling* that evaluator over the ASCII
//! range rather than by restating its table. Non-ASCII is admitted wholesale
//! instead (see [`FirstSet`]), which needs no table at all.
//!
//! # Looking through a `<subrule>`
//!
//! A rule call is walked rather than declined, which is what constraint 3
//! actually asks for ("keyed by invocant package and `TOKEN_DEFS_GEN` ... or
//! decline"). The resolution, and every shape it still declines on, is
//! [`super::regex_prefilter_subrule`]; the key itself is
//! [`super::regex_prefilter_memo::pattern_prefilter_in_pkg`]. A derivation that
//! looked through a rule name is therefore *not* a pure function of the
//! pattern and must never be stored in the pattern-keyed memo — which is why
//! [`Analyzer::resolved_subrule`] reports whether it did.
//!
//! # Looking through a scoped `:ignoremark`
//!
//! The engine matches a `:m` sub-pattern as `strip_marks_pattern(p)` against
//! the mark-stripped subject, so [`walk_pattern`] walks exactly that derived
//! tree — off the same memo the matcher uses, so there is no second reading of
//! what `:m` means to drift from. What comes out is a statement about the
//! *stripped* subject, so the set is flagged and
//! [`super::regex_prefilter_firstset::FirstSet::admits_at`] carries it back to
//! the original one; the length bound is dropped outright, because stripping is
//! not injective on positions.
//!
//! # Out of scope (declines)
//!
//! Anything that runs user code before the first character is consumed
//! (constraint 3 — a leading `{ … }` block runs once per start position in
//! both mutsu and rakudo, ADR-0009), backreferences, and `<~~>`.

use super::super::*;
use super::regex_prefilter_composite::analyze_composite_class;
use super::regex_prefilter_firstset::{
    FirstSet, class_first_set, literal_first_set, newline_first_set, unicode_prop_first_set,
    whitespace_first_set,
};
use crate::symbol::Symbol;

/// What one pass over a pattern established.
pub(crate) struct Derivation {
    /// A usable first-character set, or `None` when the pattern has none
    /// (it can match empty, the set came out universal, or the analysis
    /// declined).
    pub(crate) first: Option<FirstSet>,
    /// A lower bound on the characters any match consumes. Zero when unknown.
    pub(crate) min_len: usize,
}

/// Guard against a pathologically nested pattern recursing this analysis into
/// a stack overflow. Reaching it declines, like any other unanswerable shape.
pub(super) const MAX_DEPTH: u32 = 48;

#[derive(Clone, Copy)]
pub(super) struct Ctx {
    /// The `:i` flag of the pattern whose tokens are being walked — which is
    /// exactly what the engine passes to `regex_match_atom_in_pkg` for those
    /// tokens (`regex_match_atom_simple.rs` reads `pattern.ignore_case` of the
    /// token's own pattern, and a `Group` body is matched through its own
    /// sub-pattern's flag). Taking the sub-pattern's flag rather than OR-ing
    /// the enclosing one keeps this exact: a *negated* class matches FEWER
    /// characters under `:i` (`<-[a]>` rejects `A` there), so widening the
    /// flag would have narrowed the set — the one direction that is unsound.
    pub(super) ignore_case: bool,
    /// The package a `<subrule>` reference in these tokens resolves against.
    /// A candidate body resolves its own unqualified references against the
    /// package that DEFINED it, not against the caller's — the same rule
    /// `subrule_candidate_ends` matches under — so this follows the candidate
    /// down rather than staying at the scan's invocant.
    pub(super) pkg: Symbol,
    pub(super) depth: u32,
}

/// Per-token analysis result, in the same three terms as [`Seq`].
pub(super) struct Info {
    pub(super) first: FirstSet,
    pub(super) nullable: bool,
    pub(super) min_len: usize,
}

impl Info {
    /// An atom that consumes at least one character, starting with one of
    /// `first`.
    pub(super) fn consuming(first: FirstSet, ctx: Ctx) -> Info {
        Info {
            first,
            nullable: false,
            // Under `:i` a pattern character and a subject character are not
            // necessarily one-to-one (a multi-character fold lets `'ss'` match
            // the single character `ß`), so an atom count is not a character
            // count and no bound is claimed.
            min_len: if ctx.ignore_case { 0 } else { 1 },
        }
    }

    /// An assertion that consumes nothing, so the search for the first
    /// consumed character continues past it.
    fn zero_width() -> Info {
        Info {
            first: FirstSet::empty(),
            nullable: true,
            min_len: 0,
        }
    }
}

/// A token sequence's analysis.
pub(super) struct Seq {
    /// Characters that may appear at the sequence's first consumed position.
    pub(super) first: FirstSet,
    /// The whole sequence can match without consuming anything.
    pub(super) nullable: bool,
    pub(super) min_len: usize,
}

/// How many `<subrule>` references one derivation may resolve before it gives
/// up. A rule cone is walked breadth-first with no per-rule memo, so a
/// pathological grammar could otherwise make the derivation cost more than the
/// scan it saves; exhausting the budget declines, like any other unanswerable
/// shape.
const SUBRULE_BUDGET: u32 = 256;

/// The mutable state one derivation carries: the interpreter a `<subrule>` is
/// resolved through, the rule nodes currently on the resolution stack, and
/// whether any rule name was looked through at all.
pub(crate) struct Analyzer<'i> {
    /// `None` runs the pattern-only analysis every slice before this one ran:
    /// a rule call simply declines, and the result is a pure function of the
    /// pattern.
    pub(super) interp: Option<&'i mut Interpreter>,
    /// `(pkg, name)` nodes being walked right now. A rule reached from itself
    /// is answered "unknown" rather than unrolled — see
    /// [`super::regex_prefilter_subrule`].
    pub(super) active: Vec<(Symbol, Symbol)>,
    pub(super) budget: u32,
    pub(super) resolved_subrule: bool,
}

impl<'i> Analyzer<'i> {
    /// The pattern-only analysis: `<subrule>` declines, so the result depends
    /// on nothing but the pattern and may be memoized against it.
    pub(crate) fn pattern_only() -> Analyzer<'i> {
        Analyzer {
            interp: None,
            active: Vec::new(),
            budget: 0,
            resolved_subrule: false,
        }
    }

    /// The analysis that may look through a rule name, resolving against
    /// `interp`.
    pub(crate) fn with_interpreter(interp: &'i mut Interpreter) -> Analyzer<'i> {
        Analyzer {
            interp: Some(interp),
            active: Vec::new(),
            budget: SUBRULE_BUDGET,
            resolved_subrule: false,
        }
    }

    /// Whether the derivation actually looked through a rule name. When it
    /// did, the result is specific to the invocant package and the current
    /// `TOKEN_DEFS_GEN` and must not reach the pattern-keyed memo.
    pub(crate) fn resolved_subrule(&self) -> bool {
        self.resolved_subrule
    }
}

/// Derive the prefilter facts for `pattern`, as seen from `pkg`. Never fails:
/// an unanalyzable pattern yields the empty derivation, which filters nothing.
pub(crate) fn derive(an: &mut Analyzer, pattern: &RegexPattern, pkg: Symbol) -> Derivation {
    let Some(seq) = walk_pattern(an, pattern, pkg, 0) else {
        return Derivation {
            first: None,
            min_len: 0,
        };
    };
    // A pattern that can match empty matches at every position, so no
    // first-character set can rule one out. (Such a pattern also has a
    // minimum length of zero by construction, so nothing is lost.)
    let first = if seq.nullable || seq.first.is_universal() || seq.first.is_empty() {
        None
    } else {
        Some(seq.first)
    };
    Derivation {
        first,
        min_len: seq.min_len,
    }
}

pub(super) fn walk_pattern(
    an: &mut Analyzer,
    pattern: &RegexPattern,
    pkg: Symbol,
    depth: u32,
) -> Option<Seq> {
    // A scoped `:m` sub-pattern is matched by the engine as
    // `strip_marks_pattern(pattern)` against the mark-STRIPPED subject, so that
    // is what is walked — the same derived tree the matcher itself uses, off
    // the same memo, rather than a second reading of what `:m` means. The
    // characters that come out are a statement about the stripped text, which
    // is why the result is flagged: see [`FirstSet::admits_at`] for how a scan
    // maps it back onto the original subject. (A top-level `:m` never reaches
    // here as itself — the scan paths hand this module the already-stripped
    // pattern.)
    if pattern.ignore_mark {
        let stripped = super::regex_helpers::strip_marks_pattern(pattern);
        let mut seq = walk_pattern(an, &stripped, pkg, depth)?;
        seq.first.set_mark_skewed();
        // The length bound does NOT survive the mapping back. Stripping is not
        // injective on positions: both characters of a `\r\n` cluster map to
        // the cluster's start, so a sub-pattern consuming two STRIPPED
        // characters there covers zero ORIGINAL ones — and a bound that is not
        // a lower bound would prune a viable start. Claiming nothing is always
        // sound, and the tail-pruning it gives up is the weakest of the three
        // mechanisms anyway.
        seq.min_len = 0;
        return Some(seq);
    }
    walk_tokens(
        an,
        &pattern.tokens,
        Ctx {
            ignore_case: pattern.ignore_case,
            pkg,
            depth,
        },
    )
}

fn walk_tokens(an: &mut Analyzer, tokens: &[RegexToken], ctx: Ctx) -> Option<Seq> {
    if ctx.depth > MAX_DEPTH {
        return None;
    }
    let mut first = FirstSet::empty();
    let mut nullable = true;
    let mut min_len: usize = 0;
    for token in tokens {
        match analyze_token(an, token, ctx) {
            Some(info) => {
                if nullable {
                    first.union(&info.first);
                    nullable = info.nullable;
                }
                min_len = min_len.saturating_add(info.min_len);
            }
            None => {
                // An opaque token while the leading run can still match empty
                // leaves the first-character set undetermined, so nothing can
                // be said about the pattern at all. Once a mandatory
                // character has been seen the set is closed, and an opaque
                // token past that point only costs precision in `min_len` —
                // which is a lower bound, so contributing zero is sound.
                if nullable {
                    return None;
                }
            }
        }
    }
    Some(Seq {
        first,
        nullable,
        min_len,
    })
}

fn analyze_token(an: &mut Analyzer, token: &RegexToken, ctx: Ctx) -> Option<Info> {
    let atom = analyze_atom(an, &token.atom, ctx)?;
    let (min_reps, optional) = match &token.quant {
        RegexQuant::One | RegexQuant::OneOrMore => (1usize, false),
        RegexQuant::ZeroOrMore | RegexQuant::ZeroOrOne => (0, true),
        RegexQuant::Repeat(min, _) => (*min, *min == 0),
        // `** {code}` picks its repetition count by running user code at every
        // start position. Neither the count nor the side effect can be
        // answered statically (ADR-0009).
        RegexQuant::RepeatCode(_) => return None,
    };
    Some(Info {
        first: atom.first,
        nullable: optional || atom.nullable,
        // A `%` / `%%` separator only appears BETWEEN iterations, so it adds
        // nothing to a bound that already assumes the fewest iterations.
        min_len: atom.min_len.saturating_mul(min_reps),
    })
}

fn analyze_atom(an: &mut Analyzer, atom: &RegexAtom, ctx: Ctx) -> Option<Info> {
    let deeper = Ctx {
        depth: ctx.depth + 1,
        ..ctx
    };
    match atom {
        RegexAtom::Literal(ch) => Some(Info::consuming(
            literal_first_set(*ch, ctx.ignore_case),
            ctx,
        )),
        // A grapheme literal consumes its whole cluster, but only its first
        // codepoint decides whether the scan position is worth entering.
        RegexAtom::LiteralGrapheme(g) => {
            let lead = g.chars().next()?;
            Some(Info::consuming(
                literal_first_set(lead, ctx.ignore_case),
                ctx,
            ))
        }
        RegexAtom::CharClass(class) => Some(Info::consuming(
            class_first_set(class, ctx.ignore_case),
            ctx,
        )),
        RegexAtom::Newline => Some(Info::consuming(newline_first_set(), ctx)),
        // A `<:prop>` atom is answered the same way a character class is: by
        // calling the engine's own property predicate over the ASCII range,
        // never by restating a property table.
        RegexAtom::UnicodeProp {
            name,
            negated,
            args,
        } => Some(Info::consuming(
            unicode_prop_first_set(name, *negated, args.as_deref()),
            ctx,
        )),
        // A `<+a -b>` composite class carries a `NamedBuiltin` item whose
        // built-in predicate, when it rejects, falls back to resolving a
        // grammar token of that name — so its positive half is answered
        // against the rule registry (and reaches the package-keyed memo) while
        // its negative half narrows on character evidence alone. See
        // [`super::regex_prefilter_composite`].
        RegexAtom::CompositeClass { positive, negative } => {
            Some(analyze_composite_class(an, positive, negative, ctx))
        }
        // `.` matches every character, and `\N` all but one — deriving that
        // precisely would reject one position in a hundred for a bitmap test
        // at every one of them.
        RegexAtom::Any | RegexAtom::NotNewline => Some(Info::consuming(FirstSet::universal(), ctx)),
        // `<.ws>` is `\s+` between two word characters and `\s*` anywhere
        // else, so it may match empty — but when it does consume, it consumes
        // whitespace.
        RegexAtom::WsRule => Some(Info {
            first: whitespace_first_set(),
            nullable: true,
            min_len: 0,
        }),
        RegexAtom::Group(p)
        | RegexAtom::CaptureGroup(p)
        | RegexAtom::CaptureIsolatedGroup(p)
        | RegexAtom::CaptureIsolatedGroupScoped(p, _) => {
            let seq = walk_pattern(an, p, ctx.pkg, deeper.depth)?;
            Some(Info {
                first: seq.first,
                nullable: seq.nullable,
                min_len: seq.min_len,
            })
        }
        // Any branch may be the one that matches, so the set is their union
        // and the bound is their minimum. One unanalyzable branch sinks the
        // whole alternation: the others say nothing about what it could match.
        RegexAtom::Alternation(branches) | RegexAtom::SequentialAlternation(branches) => {
            let mut first = FirstSet::empty();
            let mut nullable = false;
            let mut min_len = usize::MAX;
            for branch in branches {
                let seq = walk_pattern(an, branch, ctx.pkg, deeper.depth)?;
                first.union(&seq.first);
                nullable |= seq.nullable;
                min_len = min_len.min(seq.min_len);
            }
            Some(Info {
                first,
                nullable,
                min_len: if branches.is_empty() { 0 } else { min_len },
            })
        }
        // Every branch has to match at this same position, so ANY analyzable
        // branch's first-set already covers the conjunction.
        RegexAtom::Conjunction(branches) => {
            let seq = branches
                .iter()
                .find_map(|branch| walk_pattern(an, branch, ctx.pkg, deeper.depth))?;
            Some(Info {
                first: seq.first,
                nullable: seq.nullable,
                min_len: seq.min_len,
            })
        }
        // Zero-width assertions constrain the position without consuming it,
        // so the search for the first consumed character passes through them.
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
        | RegexAtom::AtPosition(_) => Some(Info::zero_width()),
        // A lookaround is zero-width too, but its body still RUNS at every
        // start position, so it may only be passed through when running it is
        // unobservable.
        RegexAtom::Lookaround { pattern, .. } => {
            if pattern_runs_code(pattern, deeper.depth) {
                None
            } else {
                Some(Info::zero_width())
            }
        }
        // A rule call is walked, not declined — constraint 3's "or decline"
        // taken by its first half. The resolution and its own declines are
        // [`super::regex_prefilter_subrule`].
        RegexAtom::Named(name) => super::regex_prefilter_subrule::analyze_subrule(an, name, ctx),
        // Declines. The code atoms are ADR-0009 (they must keep running once
        // per start position); the rest match text that is not known until
        // match time.
        RegexAtom::CodeAssertion { .. }
        | RegexAtom::ClosureInterpolation { .. }
        | RegexAtom::VarDecl { .. }
        | RegexAtom::Backref(_)
        | RegexAtom::NamedBackref(_)
        | RegexAtom::VarInterp(_)
        | RegexAtom::RecurseSelf(_)
        | RegexAtom::TildeMarker
        | RegexAtom::GoalMatch { .. } => None,
    }
}

/// Whether matching `pattern` can run user code or dispatch a subrule — the
/// test for whether a zero-width construct may be passed through silently.
/// Conservative in the safe direction: an unknown shape counts as "runs code".
pub(super) fn pattern_runs_code(pattern: &RegexPattern, depth: u32) -> bool {
    if depth > MAX_DEPTH {
        return true;
    }
    pattern.tokens.iter().any(|token| {
        matches!(token.quant, RegexQuant::RepeatCode(_))
            || token
                .separator
                .as_ref()
                .is_some_and(|s| pattern_runs_code(&s.pattern, depth + 1))
            || atom_runs_code(&token.atom, depth)
    })
}

fn atom_runs_code(atom: &RegexAtom, depth: u32) -> bool {
    match atom {
        RegexAtom::CodeAssertion { .. }
        | RegexAtom::ClosureInterpolation { .. }
        | RegexAtom::VarDecl { .. }
        | RegexAtom::Named(_)
        | RegexAtom::RecurseSelf(_)
        | RegexAtom::CompositeClass { .. } => true,
        RegexAtom::Group(p)
        | RegexAtom::CaptureGroup(p)
        | RegexAtom::CaptureIsolatedGroup(p)
        | RegexAtom::CaptureIsolatedGroupScoped(p, _) => pattern_runs_code(p, depth + 1),
        RegexAtom::Alternation(v)
        | RegexAtom::SequentialAlternation(v)
        | RegexAtom::Conjunction(v) => v.iter().any(|p| pattern_runs_code(p, depth + 1)),
        RegexAtom::Lookaround { pattern, .. } => pattern_runs_code(pattern, depth + 1),
        RegexAtom::GoalMatch { goal, inner, .. } => {
            pattern_runs_code(goal, depth + 1) || pattern_runs_code(inner, depth + 1)
        }
        _ => false,
    }
}
