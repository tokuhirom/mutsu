//! The first-character set of a `<+a -b>` composite class — the sixth slice of
//! ADR-0099 Stage 1 (#8272), and the last atom that widened the derivation to
//! "anything" rather than answering.
//!
//! # Why it was left declining, and why that was a cliff
//!
//! `RegexAtom::CompositeClass` is not a character set. Its
//! [`ClassItem::NamedBuiltin`] items are matched by the engine as *two*
//! predicates in sequence: the built-in one (`alpha`, `xdigit`, `upper`, …)
//! and then, when that rejects, a fallback that resolves a **grammar token**
//! of that name in the invocant package and matches it against the *remaining
//! input*. The second half is a `<subrule>` in disguise — package-dependent,
//! generation-dependent, and not even single-character — so the first five
//! slices widened the whole atom to [`FirstSet::universal`], which makes the
//! pattern it sits in unfilterable.
//!
//! That is a cliff rather than a shortfall. On a 144,000-character subject, a
//! failing `/ <+upper -[A]> ** 3 /` scan cost the same 205 ms with the
//! prefilter on as with it off, against 0.8 ms for a literal scan of the same
//! subject: the composite class bought exactly nothing, and every one of the
//! subject's positions still paid a full engine entry.
//!
//! # What is derived, and what each half is allowed to claim
//!
//! The engine's arm is `pos_match && !neg_match`, so the two halves are used
//! in opposite directions and each needs its own justification:
//!
//! - a **positive** item must be *over*-approximated, because a character it
//!   can match has to stay in the set. Its built-in half is enumerated over
//!   ASCII by calling [`composite_item_matches`] — the engine's own predicate,
//!   shared rather than restated (ADR-0099 §4 constraint 1) — but its
//!   grammar-token half cannot be, so a name any rule in the package could
//!   answer to declines the whole atom back to universal;
//! - a **negative** item may narrow, because a character it matches is one the
//!   arm rejects outright. Here the same shared predicate answering `true` is
//!   already proof: the engine runs the character half first and
//!   short-circuits, so a `true` here is a `true` there whatever the fallback
//!   would have done. The fallback can only reject *more*, which is the safe
//!   direction — so a negative item needs no resolution at all.
//!
//! An empty `positive` means "any character" to the engine (the shape a
//! purely-negated enumerated class like `<-[;] - [q]>` parses to), so the
//! derivation starts from universal there and lets the negatives carve it
//! down.
//!
//! # The registry consultation, and the memo it forces
//!
//! Testing whether a name could dispatch a grammar token reads the rule
//! registry, which makes the result a fact about `(pkg, TOKEN_DEFS_GEN)` and
//! not about the pattern — exactly the condition ADR-0099 §4 constraint 3
//! attaches to looking through a rule name. Such a pattern must therefore
//! reach the package-keyed memo, which is why
//! [`super::regex_prefilter_memo::mentions_subrule`] reports a composite class
//! carrying a `NamedBuiltin` item, and why [`composite_class_reads_registry`]
//! is the single shared statement of when that happens.

use super::super::*;
use super::regex_eval_class::{composite_item_matches, composite_probe_chars};
use super::regex_prefilter_analysis::{Analyzer, Ctx, Info};
use super::regex_prefilter_firstset::{FirstSet, class_is_ascii_only};
use crate::symbol::Symbol;

/// Whether deriving a first-set for this composite class has to read the rule
/// registry — i.e. whether its result is specific to a package and a
/// `TOKEN_DEFS_GEN` rather than a pure function of the pattern.
///
/// One definition, used by both the derivation below and
/// [`super::regex_prefilter_memo::mentions_subrule`], because the two
/// disagreeing is not a slow scan but a wrong answer: a registry-dependent set
/// stored in the pattern-keyed memo would answer for every package.
pub(super) fn composite_class_reads_registry(positive: &[ClassItem]) -> bool {
    positive
        .iter()
        .any(|item| matches!(item, ClassItem::NamedBuiltin(_)))
}

/// The first-set of a `<+a -b>` composite class, as seen from `ctx.pkg`.
///
/// Never fails: an item the analysis cannot bound widens to
/// [`FirstSet::universal`], which is what every slice before this one produced
/// for the whole atom.
pub(super) fn analyze_composite_class(
    an: &mut Analyzer,
    positive: &[ClassItem],
    negative: &[ClassItem],
    ctx: Ctx,
) -> Info {
    let Some(mut set) = positive_first_set(an, positive, ctx) else {
        return Info::consuming(FirstSet::universal(), ctx);
    };
    narrow_by_negatives(&mut set, negative, ctx.ignore_case);
    // `\r\n` is one grapheme cluster, and the arm resolves the `\r` that starts
    // one to `\n` before testing — so a set holding `\n` must offer that `\r`
    // as a start position. Applied after the narrowing on purpose: a negative
    // item matching `\r` says nothing about the position, because the arm
    // never tests `\r` there.
    if set.contains('\n') {
        set.insert('\r');
    }
    Info::consuming(set, ctx)
}

/// The union over the positive items, or `None` when one of them cannot be
/// bounded.
fn positive_first_set(an: &mut Analyzer, positive: &[ClassItem], ctx: Ctx) -> Option<FirstSet> {
    if positive.is_empty() {
        // The engine reads an empty positive list as "any character".
        return Some(FirstSet::universal());
    }
    let mut set = FirstSet::empty();
    for item in positive {
        // One unbounded item sinks the union: the others say nothing about
        // what it could match.
        set.union(&positive_item_first_set(an, item, ctx)?);
    }
    Some(set)
}

/// The characters one positive item can match, or `None` when it may dispatch
/// a grammar token whose body this analysis does not walk.
fn positive_item_first_set(an: &mut Analyzer, item: &ClassItem, ctx: Ctx) -> Option<FirstSet> {
    if let ClassItem::NamedBuiltin(name) = item {
        let interp = an.interp.as_deref_mut()?;
        // Recorded before the answer is known, exactly as `analyze_subrule`
        // does: a derivation that consulted the registry at all is not a pure
        // function of the pattern, whichever way the consultation came out.
        an.resolved_subrule = true;
        if interp.composite_item_may_dispatch_token(name, ctx.pkg) {
            return None;
        }
    }
    let mut set = if item_is_ascii_only(item, ctx.ignore_case) {
        FirstSet::empty()
    } else {
        FirstSet::ascii_none_rest_all()
    };
    for cp in 0u8..128 {
        let c = cp as char;
        if composite_item_matches(item, &composite_probe_chars(c, ctx.ignore_case)) {
            set.insert(c);
        }
    }
    Some(set)
}

/// Remove every ASCII character a negative item provably rejects.
///
/// Sound with no resolution because [`composite_item_matches`] answering
/// `true` is the same `true` the engine's arm short-circuits on, and the
/// grammar-token fallback it does not run can only make `neg_match` *more*
/// often true — which would reject more characters, not fewer.
fn narrow_by_negatives(set: &mut FirstSet, negative: &[ClassItem], ignore_case: bool) {
    if negative.is_empty() {
        return;
    }
    for cp in 0u8..128 {
        let c = cp as char;
        if !set.contains(c) {
            continue;
        }
        let probe = composite_probe_chars(c, ignore_case);
        if negative
            .iter()
            .any(|item| composite_item_matches(item, &probe))
        {
            set.remove_ascii(c);
        }
    }
}

/// Whether an item provably matches no non-ASCII character, which is what lets
/// its set be exact rather than "ASCII plus everything else".
///
/// A built-in name and a Unicode property are both evaluated over the whole of
/// Unicode by predicates this module only samples over ASCII, so neither
/// qualifies; everything else is asked through the plain class rule that
/// already answers this question for `<[a..z]>`.
fn item_is_ascii_only(item: &ClassItem, ignore_case: bool) -> bool {
    match item {
        ClassItem::NamedBuiltin(_) | ClassItem::UnicodePropItem { .. } => false,
        other => class_is_ascii_only(
            &CharClass {
                items: vec![other.clone()],
                negated: false,
            },
            ignore_case,
        ),
    }
}

impl Interpreter {
    /// Whether a `NamedBuiltin` item's grammar-token fallback could engage in
    /// `pkg` — in which case the item matches whatever that token's body
    /// matches, and this analysis declines rather than guessing.
    ///
    /// The test is deliberately an *existence* question asked of the same two
    /// resolvers the engine's fallback itself consults, in the same order, and
    /// not an attempt to re-derive which body would win: the second of them
    /// (`resolve_token_patterns_with_args_in_pkg`) binds parameters and can
    /// read the caller's scope, which is neither free nor keyed by anything
    /// this memo carries. Asking only whether any definition of the name is
    /// visible needs `resolve_token_defs_in_pkg`, which that resolver derives
    /// its own candidates from, and reads nothing.
    fn composite_item_may_dispatch_token(&mut self, name: &str, pkg: Symbol) -> bool {
        // The engine's own guard: with no invocant package there is no rule
        // registry to fall back to, so the built-in predicate is the whole test.
        if pkg.is_empty() {
            return false;
        }
        !self
            .resolve_token_patterns_static_in_pkg(name, pkg)
            .is_empty()
            || !self.resolve_token_defs_in_pkg(name, pkg).is_empty()
    }
}
