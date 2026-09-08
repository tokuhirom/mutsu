//! ADR-0073 Slice 2: a `<subrule>` call under a ratcheted caller produces ONE
//! candidate.
//!
//! `token` and `rule` declarators are ratcheted by definition, so a caller
//! written with either cannot backtrack into the subrule it calls: only the
//! subrule's highest-priority end can ever be used. The eager producer computed
//! every end anyway and then threw all but that one away — which, because an
//! embedded `{ ... }` block runs inline for real (ADR-0009), fired the block
//! once per end *computed* instead of once per end *entered*:
//!
//! ```raku
//! my $n = 0;
//! grammar G { regex part { \w* { $n++ } }; token TOP { <part> 'c' } }
//! G.parse("abc");   # raku: $n == 1     mutsu (before): $n == 5
//! ```
//!
//! Walking the subrule's body with `first_only` fixes that outright, and cuts
//! the wasted enumeration on every grammar path where the callee is a leaf.
//!
//! # Why the guard below exists
//!
//! The `Named` arm carries the left-recursion growing-seed loop
//! (`LR_ACTIVE` / `LR_MEMO` / `LR_SEED_READ`, `regex_match_atom.rs`), which
//! discovers that a rule is left-recursive at this position by *evaluating* its
//! candidates and then checking whether the seed was consulted. A `first_only`
//! walk stops at the first complete match, so it can return before ever
//! entering the branch that re-enters the rule — and the loop would then
//! conclude "not left-recursive" and keep an unGrown seed. That is not
//! hypothetical: `token expr { <term> | <expr> '+' <term> }` ranks `<term>`
//! ahead of the recursive branch, so an unguarded `first_only` walk stops on
//! `<term>` and `.parse('1+2+3')` fails.
//!
//! [`pattern_is_rule_call_free`] is the sound, cheap precondition that rules
//! that hazard out: a body that cannot invoke a named rule at all cannot
//! re-enter *itself*, so its seed can never be consulted and the growing loop
//! is guaranteed to stop after one iteration whether or not the walk was cut
//! short. It is an over-approximation — it admits leaf rules only.
//!
//! The rule-call-graph analysis that answers the real question lives in
//! [`super::regex_call_graph`] and gates the *streamed* subrule path
//! (`regex_match_lazy::drive_named_subrule_candidates`), which handles a
//! single-candidate, argument-less call under either kind of caller. This
//! predicate is what the eager arm still falls back on for everything that path
//! declines — a proto, several candidates, a call with arguments — where the
//! ratchet is the only thing making truncation legal.
//!
//! The predicate lists the safe atom kinds explicitly and answers `false` for
//! anything else, so a newly added `RegexAtom` variant is excluded until
//! somebody has thought about it.

use crate::runtime::regex_types::{RegexAtom, RegexPattern};

/// `true` when nothing in `pattern` can dispatch to a named grammar rule, so
/// matching it can never re-enter the rule whose body it is.
pub(super) fn pattern_is_rule_call_free(pattern: &RegexPattern) -> bool {
    pattern.tokens.iter().all(|token| {
        atom_is_rule_call_free(&token.atom)
            && token
                .separator
                .as_ref()
                .is_none_or(|sep| pattern_is_rule_call_free(&sep.pattern))
    })
}

fn atom_is_rule_call_free(atom: &RegexAtom) -> bool {
    match atom {
        // Every one of these matches text (or asserts) on its own; none of them
        // reaches the subrule dispatcher.
        RegexAtom::Literal(_)
        | RegexAtom::Any
        | RegexAtom::CharClass(_)
        | RegexAtom::Newline
        | RegexAtom::NotNewline
        | RegexAtom::ZeroWidth
        // An embedded `{ ... }` / `<?{ ... }>` runs user code, which is exactly
        // what this slice exists to stop over-firing. Code that re-entered the
        // *same* rule at the *same* position from inside such a block would
        // defeat the guard, so the growing-seed loop keeps a runtime fallback
        // for it (see `regex_match_atom.rs`).
        | RegexAtom::CodeAssertion { .. }
        | RegexAtom::UnicodeProp { .. }
        | RegexAtom::UnicodePropAssert { .. }
        | RegexAtom::CaptureStartMarker
        | RegexAtom::CaptureEndMarker
        | RegexAtom::VarDecl { .. }
        | RegexAtom::CompositeClass { .. }
        | RegexAtom::LeftWordBoundary
        | RegexAtom::RightWordBoundary
        | RegexAtom::WordBoundary { .. }
        | RegexAtom::WithinWord { .. }
        | RegexAtom::StartOfLine
        | RegexAtom::EndOfLine
        | RegexAtom::EndOfString
        | RegexAtom::Backref(_)
        | RegexAtom::NamedBackref(_)
        // Interpolates a variable's STRING value as a literal, not as a rule.
        | RegexAtom::VarInterp(_)
        | RegexAtom::SameAssertion { .. }
        | RegexAtom::AtPosition(_)
        | RegexAtom::TildeMarker => true,
        RegexAtom::Group(p)
        | RegexAtom::CaptureGroup(p)
        | RegexAtom::CaptureIsolatedGroup(p) => pattern_is_rule_call_free(p),
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => alts.iter().all(pattern_is_rule_call_free),
        RegexAtom::Lookaround { pattern, .. } => pattern_is_rule_call_free(pattern),
        RegexAtom::GoalMatch { goal, inner, .. } => {
            pattern_is_rule_call_free(goal) && pattern_is_rule_call_free(inner)
        }
        // `<name>` dispatches to a rule; `<.ws>` dispatches to an overridable
        // `ws`; `<{ ... }>` matches whatever regex the code returns; `<~~>`
        // re-enters the enclosing rule by construction.
        RegexAtom::Named(_)
        | RegexAtom::WsRule
        | RegexAtom::ClosureInterpolation { .. }
        | RegexAtom::RecurseSelf(_) => false,
    }
}
