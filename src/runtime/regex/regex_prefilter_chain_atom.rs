//! The per-atom leaf of the ADR-0099 §5 NFA chain
//! ([`super::regex_prefilter_chain`]): [`chain_atom_once`]'s dispatch over
//! every [`RegexAtom`] variant, and [`merge_alternation`], the rule shared by
//! an alternation and a multi-candidate `<subrule>` (see
//! [`super::regex_prefilter_subrule::chain_subrule`]).
//!
//! Split out of `regex_prefilter_chain.rs` to keep that file under the
//! repository's 500-line rule (the same split `regex_prefilter_tests.rs`
//! uses, just for production code rather than tests) — this file has no
//! independent purpose beyond that: read
//! [`super::regex_prefilter_chain`]'s module doc comment first, which is
//! where the mechanism as a whole is explained.

use super::super::*;
use super::regex_prefilter_analysis::{Analyzer, Ctx, pattern_runs_code};
use super::regex_prefilter_chain::{Piece, chain_pattern};
use super::regex_prefilter_composite::composite_class_chain_set;
use super::regex_prefilter_firstset::{
    FirstSet, class_first_set, literal_first_set, newline_first_set, unicode_prop_first_set,
};

/// Whether an atom is unconditionally zero-width regardless of how its
/// enclosing token is quantified — the one shape safe to see through even
/// when the quantifier itself permits zero occurrences.
pub(super) fn is_zero_width_atom(atom: &RegexAtom) -> bool {
    matches!(
        atom,
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
    )
}

/// Whether `class` is guaranteed to consume exactly one subject character
/// wherever it matches at all — see the module doc comment for the two ways
/// it can consume two instead (a `Grapheme` entry, or the `\r\n`-as-one-`\n`
/// special case in `regex_match_atom_simple.rs`).
fn class_consumes_exactly_one_char(class: &CharClass, ignore_case: bool) -> bool {
    !class
        .items
        .iter()
        .any(|item| matches!(item, ClassItem::Grapheme(_)))
        && !super::regex_eval_class::class_matches_ignorecase(class, '\n', ignore_case)
}

/// One occurrence of `atom`'s chain — the same dispatch
/// [`super::regex_prefilter_analysis::walk_pattern`]'s private `analyze_atom`
/// helper performs, restructured to report exactness rather than an
/// aggregate nullable/min-length summary.
pub(super) fn chain_atom_once(an: &mut Analyzer, atom: &RegexAtom, ctx: Ctx) -> Option<Piece> {
    let deeper = Ctx {
        depth: ctx.depth + 1,
        ..ctx
    };
    // Under `:i`, whether a subject character and a pattern character
    // correspond one-to-one is not something this module re-derives (a
    // multi-character fold is exactly ADR-0099 §4 constraint 2's territory);
    // `Info::consuming` in the single-position analysis takes the same
    // position by zeroing `min_len` under `:i`. So every atom below is
    // computed the ordinary way and then downgraded to non-extending when
    // `ctx.ignore_case` holds, keeping the position-0 answer but declining to
    // chain past it.
    let piece = match atom {
        RegexAtom::Literal(ch) => Some(Piece::one(literal_first_set(*ch, ctx.ignore_case))),
        // Consumes a whole, possibly multi-codepoint grapheme cluster, so
        // nothing can be said about the position right after it.
        RegexAtom::LiteralGrapheme(g) => {
            let lead = g.chars().next()?;
            Some(Piece::final_step(literal_first_set(lead, ctx.ignore_case)))
        }
        RegexAtom::CharClass(class) => {
            let set = class_first_set(class, ctx.ignore_case);
            if class_consumes_exactly_one_char(class, ctx.ignore_case) {
                Some(Piece::one(set))
            } else {
                Some(Piece::final_step(set))
            }
        }
        RegexAtom::Newline => Some(Piece::final_step(newline_first_set())),
        RegexAtom::UnicodeProp {
            name,
            negated,
            args,
        } => Some(Piece::one(unicode_prop_first_set(
            name,
            *negated,
            args.as_deref(),
        ))),
        RegexAtom::CompositeClass { positive, negative } => {
            match composite_class_chain_set(an, positive, negative, ctx) {
                Some(set) => Some(Piece::one(set)),
                None => Some(Piece::final_step(FirstSet::universal())),
            }
        }
        // `.`/`\N` match a whole grapheme too, and `<.ws>` is nullable in
        // general (`\s+` between word characters, `\s*` elsewhere) — neither
        // is a fixed one-character consumer.
        RegexAtom::Any | RegexAtom::NotNewline => Some(Piece::final_step(FirstSet::universal())),
        RegexAtom::WsRule => None,
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) | RegexAtom::CaptureIsolatedGroup(p) => {
            chain_pattern(an, p, ctx.pkg, deeper.depth)
        }
        RegexAtom::Alternation(branches) | RegexAtom::SequentialAlternation(branches) => {
            if branches.is_empty() {
                return None;
            }
            let mut pieces = Vec::with_capacity(branches.len());
            for branch in branches {
                pieces.push(chain_pattern(an, branch, ctx.pkg, deeper.depth)?);
            }
            Some(merge_alternation(pieces))
        }
        // Every branch matches the same position, so any one analyzable
        // branch already speaks for the conjunction.
        RegexAtom::Conjunction(branches) => branches
            .iter()
            .find_map(|branch| chain_pattern(an, branch, ctx.pkg, deeper.depth)),
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
        | RegexAtom::AtPosition(_) => Some(Piece::empty_exact()),
        RegexAtom::Lookaround { pattern, .. } => {
            if pattern_runs_code(pattern, deeper.depth) {
                None
            } else {
                Some(Piece::empty_exact())
            }
        }
        RegexAtom::Named(name) => super::regex_prefilter_subrule::chain_subrule(an, name, ctx),
        RegexAtom::CodeAssertion { .. }
        | RegexAtom::ClosureInterpolation { .. }
        | RegexAtom::VarDecl { .. }
        | RegexAtom::Backref(_)
        | RegexAtom::NamedBackref(_)
        | RegexAtom::VarInterp(_)
        | RegexAtom::RecurseSelf(_)
        | RegexAtom::TildeMarker
        | RegexAtom::GoalMatch { .. } => None,
    }?;
    Some(if ctx.ignore_case {
        piece.non_extending()
    } else {
        piece
    })
}

/// The chain of an atom that may resolve to several bodies (an alternation,
/// or several candidates of the same rule name): the union of what every
/// branch requires at each offset both branches still have one, `exact` only
/// when every branch is itself exact AND all branches are the SAME total
/// length — a shorter branch has already handed control to whatever follows,
/// which this merge cannot see, so extending past it would assume a
/// character the shorter branch never claimed.
pub(super) fn merge_alternation(pieces: Vec<Piece>) -> Piece {
    let Some(min_len) = pieces.iter().map(|p| p.step_count()).min() else {
        return Piece::empty_exact();
    };
    let mut steps = Vec::with_capacity(min_len);
    for i in 0..min_len {
        let mut set = FirstSet::empty();
        for p in &pieces {
            set.union(p.step(i));
        }
        steps.push(set);
    }
    let same_len = pieces.iter().all(|p| p.step_count() == min_len);
    let exact = same_len && pieces.iter().all(Piece::is_exact);
    Piece::from_steps(steps, exact)
}
