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
//! [`class_matches_ignorecase`], and this module derives a class's first-set
//! by *calling* it over the ASCII range rather than by restating its table.
//! Non-ASCII is admitted wholesale instead (see [`FirstSet`]), which needs no
//! table at all.
//!
//! # Out of scope (declines)
//!
//! `<subrule>` calls (constraint 3: a prefix derived through one would have to
//! be keyed by invocant package and `TOKEN_DEFS_GEN`), anything that runs user
//! code before the first character is consumed (constraint 3 again — a leading
//! `{ … }` block runs once per start position in both mutsu and rakudo, ADR-0009),
//! backreferences, `<~~>`, and `:m` sub-patterns.

use super::super::*;
use super::regex_eval_class::class_matches_ignorecase;
use super::regex_prefilter_firstset::FirstSet;

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
const MAX_DEPTH: u32 = 48;

#[derive(Clone, Copy)]
struct Ctx {
    /// The `:i` flag of the pattern whose tokens are being walked — which is
    /// exactly what the engine passes to `regex_match_atom_in_pkg` for those
    /// tokens (`regex_match_atom_simple.rs` reads `pattern.ignore_case` of the
    /// token's own pattern, and a `Group` body is matched through its own
    /// sub-pattern's flag). Taking the sub-pattern's flag rather than OR-ing
    /// the enclosing one keeps this exact: a *negated* class matches FEWER
    /// characters under `:i` (`<-[a]>` rejects `A` there), so widening the
    /// flag would have narrowed the set — the one direction that is unsound.
    ignore_case: bool,
    depth: u32,
}

/// Per-token analysis result, in the same three terms as [`Seq`].
struct Info {
    first: FirstSet,
    nullable: bool,
    min_len: usize,
}

impl Info {
    /// An atom that consumes at least one character, starting with one of
    /// `first`.
    fn consuming(first: FirstSet, ctx: Ctx) -> Info {
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
struct Seq {
    /// Characters that may appear at the sequence's first consumed position.
    first: FirstSet,
    /// The whole sequence can match without consuming anything.
    nullable: bool,
    min_len: usize,
}

/// Derive the prefilter facts for `pattern`. Never fails: an unanalyzable
/// pattern yields the empty derivation, which filters nothing.
pub(crate) fn derive(pattern: &RegexPattern) -> Derivation {
    let Some(seq) = walk_pattern(pattern, 0) else {
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

fn walk_pattern(pattern: &RegexPattern, depth: u32) -> Option<Seq> {
    // `:m` matches against a mark-stripped subject through a separately
    // stripped pattern; a scoped one inside an otherwise unstripped pattern
    // would make the characters analyzed here the wrong ones. The scan paths
    // hand this module the already-stripped pattern for a top-level `:m`, so
    // this only declines the scoped case.
    if pattern.ignore_mark {
        return None;
    }
    walk_tokens(
        &pattern.tokens,
        Ctx {
            ignore_case: pattern.ignore_case,
            depth,
        },
    )
}

fn walk_tokens(tokens: &[RegexToken], ctx: Ctx) -> Option<Seq> {
    if ctx.depth > MAX_DEPTH {
        return None;
    }
    let mut first = FirstSet::empty();
    let mut nullable = true;
    let mut min_len: usize = 0;
    for token in tokens {
        match analyze_token(token, ctx) {
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

fn analyze_token(token: &RegexToken, ctx: Ctx) -> Option<Info> {
    let atom = analyze_atom(&token.atom, ctx)?;
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

fn analyze_atom(atom: &RegexAtom, ctx: Ctx) -> Option<Info> {
    let deeper = Ctx {
        depth: ctx.depth + 1,
        ..ctx
    };
    match atom {
        RegexAtom::Literal(ch) => Some(Info::consuming(literal_first_set(*ch, ctx), ctx)),
        // A grapheme literal consumes its whole cluster, but only its first
        // codepoint decides whether the scan position is worth entering.
        RegexAtom::LiteralGrapheme(g) => {
            let lead = g.chars().next()?;
            Some(Info::consuming(literal_first_set(lead, ctx), ctx))
        }
        RegexAtom::CharClass(class) => Some(Info::consuming(class_first_set(class, ctx), ctx)),
        RegexAtom::Newline => Some(Info::consuming(newline_first_set(), ctx)),
        // `.` matches every character; `\N` and a `<:prop>` / `<+a-b>` class
        // are decidable in principle but only through tables this module
        // declines to restate, so both widen to "anything".
        RegexAtom::Any
        | RegexAtom::NotNewline
        | RegexAtom::UnicodeProp { .. }
        | RegexAtom::CompositeClass { .. } => Some(Info::consuming(FirstSet::universal(), ctx)),
        // `<.ws>` is `\s+` between two word characters and `\s*` anywhere
        // else, so it may match empty — but when it does consume, it consumes
        // whitespace.
        RegexAtom::WsRule => Some(Info {
            first: whitespace_first_set(),
            nullable: true,
            min_len: 0,
        }),
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) | RegexAtom::CaptureIsolatedGroup(p) => {
            let seq = walk_pattern(p, deeper.depth)?;
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
                let seq = walk_pattern(branch, deeper.depth)?;
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
                .find_map(|branch| walk_pattern(branch, deeper.depth))?;
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
        // Declines. `Named` is constraint 3 (a subrule-derived set would have
        // to be keyed by invocant package and `TOKEN_DEFS_GEN`); the code
        // atoms are ADR-0009 (they must keep running once per start position);
        // the rest match text that is not known until match time.
        RegexAtom::Named(_)
        | RegexAtom::CodeAssertion { .. }
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

/// The characters a literal atom can match at a start position.
///
/// Without `:i` that is the character itself. With it, the engine's test is
/// `ch.to_lowercase().to_string() == c.to_lowercase().to_string()`
/// (`regex_match_atom_simple.rs`), which this enumerates exactly over ASCII —
/// the fold closure, not a folded needle: a folded needle is unsound because
/// multi-character folds (`ß`/`SS`, `ﬁ`/`fi`) make a case-folded literal
/// *variable-length* (ADR-0099 §4 constraint 2). Every non-ASCII character is
/// admitted, since the reverse closure over Unicode reaches ASCII targets from
/// far away (`K` U+212A lowercases to `k`, `ſ` U+017F to `s`).
fn literal_first_set(ch: char, ctx: Ctx) -> FirstSet {
    if !ctx.ignore_case {
        return FirstSet::single(ch);
    }
    let want = ch.to_lowercase().to_string();
    let mut set = FirstSet::ascii_none_rest_all();
    for cp in 0u8..128 {
        let c = cp as char;
        if c.to_lowercase().to_string() == want {
            set.insert(c);
        }
    }
    set
}

/// The characters a character-class atom can match at a start position,
/// derived by asking the engine's own evaluator about each ASCII character
/// rather than by restating its table (see the module doc comment).
fn class_first_set(class: &CharClass, ctx: Ctx) -> FirstSet {
    let mut set = if class_is_ascii_only(class, ctx) {
        FirstSet::empty()
    } else {
        FirstSet::ascii_none_rest_all()
    };
    for cp in 0u8..128 {
        let c = cp as char;
        if class_matches_ignorecase(class, c, ctx.ignore_case) {
            set.insert(c);
        }
    }
    // `\r\n` is one grapheme, and the class arm accepts a class containing
    // `\n` at the `\r` that starts it.
    if set.contains('\n') {
        set.insert('\r');
    }
    // A `Grapheme` item matches a whole multi-codepoint cluster, which the
    // per-character evaluator above necessarily answers `false` for — the
    // comparison happens at the atom, where the subject text is. Its leading
    // codepoint is the one a scan position would be tested at.
    for item in &class.items {
        if let ClassItem::Grapheme(g) = item
            && let Some(lead) = g.chars().next()
        {
            set.insert(lead);
        }
    }
    set
}

/// Whether `class` provably matches no non-ASCII character, which is what
/// lets its first-set be exact rather than "ASCII plus everything else".
///
/// Deliberately a short whitelist of the items whose non-ASCII behaviour is
/// obvious from the item itself: an explicit character or range below U+0080,
/// and `\d` (which the evaluator defines as `is_ascii_digit`). A negated class
/// matches almost every non-ASCII character by construction; under `:i` a
/// non-ASCII character can fold onto an ASCII member; and a `Grapheme` entry is
/// compared in NFC against a normalized subject cluster, so its leading
/// codepoint as stored is not quite a promise about the subject's. None of the
/// three qualifies.
fn class_is_ascii_only(class: &CharClass, ctx: Ctx) -> bool {
    if class.negated || ctx.ignore_case {
        return false;
    }
    class.items.iter().all(|item| match item {
        ClassItem::Char(c) => c.is_ascii(),
        ClassItem::Range(a, b) => a.is_ascii() && b.is_ascii(),
        ClassItem::Digit => true,
        _ => false,
    })
}

/// `\n` as the engine's `Newline` atom defines it.
fn newline_first_set() -> FirstSet {
    let mut set = FirstSet::empty();
    for c in ['\n', '\r', '\u{85}', '\u{2028}'] {
        set.insert(c);
    }
    set
}

fn whitespace_first_set() -> FirstSet {
    let mut set = FirstSet::ascii_none_rest_all();
    for cp in 0u8..128 {
        let c = cp as char;
        if c.is_whitespace() {
            set.insert(c);
        }
    }
    set
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
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) | RegexAtom::CaptureIsolatedGroup(p) => {
            pattern_runs_code(p, depth + 1)
        }
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
