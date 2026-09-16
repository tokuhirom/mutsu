//! The two memos the ADR-0099 Stage 1 prefilter is cached in, and the test
//! that picks between them.
//!
//! A [`Prefilter`] costs a walk of the whole token tree with every character
//! class evaluated over the ASCII range, so it must be derived once per
//! pattern rather than once per scan (a `:g` loop makes one
//! `regex_scan_positions` call per match). *What* it may be keyed by is the
//! question this module answers, and there are two answers:
//!
//! - a pattern that names no rule derives a pure function of itself, so one
//!   `OnceLock` slot on the pattern holds it forever;
//! - a pattern that names one does not. The same `<x>` resolves to different
//!   bodies in different packages, and to different bodies after any
//!   (re)definition, so its entries are keyed by invocant package **and**
//!   `TOKEN_DEFS_GEN` — which is exactly the condition ADR-0099 §4 constraint
//!   3 attaches to looking through a rule name at all.
//!
//! Getting that split wrong is not a slow scan but a wrong answer: one frozen
//! first-set answering for two packages silently drops every match in the
//! second. [`mentions_subrule`] is therefore deliberately coarse — it says yes
//! for any `<…>` anywhere in the pattern, including places the analysis never
//! looks — because over-reporting costs a lock and under-reporting costs
//! correctness.
//!
//! "Names a rule" is not the same as "is spelled `<x>`": a `<+a -b>` composite
//! class reads the registry too, because a positive `NamedBuiltin` item whose
//! built-in predicate rejects falls back to a grammar token of that name. That
//! condition has exactly one statement of it,
//! [`super::regex_prefilter_composite::composite_class_reads_registry`], which
//! both this test and the derivation consult — two readings of it would be
//! free to disagree, and the disagreement is the wrong answer above.

use super::super::*;
use super::regex_prefilter::Prefilter;
use super::regex_prefilter_analysis::Analyzer;
use crate::symbol::Symbol;
use std::sync::Arc;

/// This pattern's memoized [`Prefilter`], for a pattern that mentions no rule
/// name — the derivation is then a pure function of the pattern.
pub(super) fn pattern_prefilter(pattern: &RegexPattern) -> &Arc<Prefilter> {
    pattern.derived.prefilter.get_or_init(|| {
        Arc::new(Prefilter::build(
            &mut Analyzer::pattern_only(),
            pattern,
            Symbol::intern(""),
        ))
    })
}

/// This pattern's memoized [`Prefilter`] as seen from `pkg`, for a pattern
/// that does mention a rule name.
///
/// Keyed by the package **and** the `TOKEN_DEFS_GEN` the derivation ran under,
/// because neither alone pins what `<x>` resolves to: `grammar H is G` may
/// override `G`'s `token x` for one invocant and not another, and any
/// (re)definition anywhere moves the generation. This is ADR-0099 §4
/// constraint 3's condition, and [`super::regex_prefilter_subrule`] declines
/// everything the two together still fail to pin.
pub(super) fn pattern_prefilter_in_pkg(
    interp: &mut Interpreter,
    pattern: &RegexPattern,
    pkg: Symbol,
) -> Arc<Prefilter> {
    let generation =
        crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
    if let Ok(slots) = pattern.derived.prefilter_in_pkg.lock()
        && let Some(hit) = slots
            .iter()
            .find(|e| e.pkg == pkg && e.token_defs_gen == generation)
    {
        return Arc::clone(&hit.prefilter);
    }
    let mut analyzer = Analyzer::with_interpreter(interp);
    let built = Arc::new(Prefilter::build(&mut analyzer, pattern, pkg));
    let resolved_subrule = analyzer.resolved_subrule();
    crate::vm::vm_stats::record_regex_prefilter_subrule(resolved_subrule);
    if let Ok(mut slots) = pattern.derived.prefilter_in_pkg.lock() {
        // A generation bump invalidates every entry at once, so the stale ones
        // are dropped rather than searched past forever.
        slots.retain(|e| e.token_defs_gen == generation);
        // Bounded, because the vector is searched linearly on every scan and a
        // pattern used from more packages than this is not the shape the memo
        // exists for.
        const MAX_SLOTS: usize = 8;
        if slots.len() >= MAX_SLOTS {
            slots.clear();
        }
        slots.push(crate::runtime::regex_types::PkgPrefilter {
            pkg,
            token_defs_gen: generation,
            prefilter: Arc::clone(&built),
        });
    }
    built
}

/// Whether `pattern` mentions a `<subrule>` anywhere — the test that picks
/// between the two memos above. Memoized because it is asked once per scan.
pub(super) fn mentions_subrule(pattern: &RegexPattern) -> bool {
    *pattern
        .derived
        .mentions_subrule
        .get_or_init(|| pattern_mentions_subrule(pattern, 0))
}

fn pattern_mentions_subrule(pattern: &RegexPattern, depth: u32) -> bool {
    /// Deeper than the analysis itself will walk, so an over-deep pattern
    /// answers "yes" and takes the package-keyed path, where the analysis
    /// declines on depth anyway.
    const MAX_DEPTH: u32 = 48;
    if depth > MAX_DEPTH {
        return true;
    }
    pattern.tokens.iter().any(|token| {
        atom_mentions_subrule(&token.atom, depth)
            || token
                .separator
                .as_ref()
                .is_some_and(|s| pattern_mentions_subrule(&s.pattern, depth + 1))
    })
}

fn atom_mentions_subrule(atom: &RegexAtom, depth: u32) -> bool {
    match atom {
        RegexAtom::Named(_) => true,
        // A composite class is a rule reference in disguise whenever one of
        // its POSITIVE items is a `NamedBuiltin`: the built-in predicate
        // rejecting falls back to a grammar token of that name, so deriving
        // the atom's first-set reads the rule registry. The condition is
        // stated once, in the module that does the reading, so the two cannot
        // disagree — and disagreeing here is a wrong answer, not a slow scan.
        RegexAtom::CompositeClass { positive, .. } => {
            super::regex_prefilter_composite::composite_class_reads_registry(positive)
        }
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) | RegexAtom::CaptureIsolatedGroup(p) => {
            pattern_mentions_subrule(p, depth + 1)
        }
        RegexAtom::Alternation(v)
        | RegexAtom::SequentialAlternation(v)
        | RegexAtom::Conjunction(v) => v.iter().any(|p| pattern_mentions_subrule(p, depth + 1)),
        RegexAtom::Lookaround { pattern, .. } => pattern_mentions_subrule(pattern, depth + 1),
        RegexAtom::GoalMatch { goal, inner, .. } => {
            pattern_mentions_subrule(goal, depth + 1) || pattern_mentions_subrule(inner, depth + 1)
        }
        // Spelled out rather than wildcarded ON PURPOSE. Missing a container
        // here does not make a scan slower, it makes the answer wrong: the
        // pattern would take the pattern-keyed memo and one package's
        // first-set would answer for every package. An added atom must fail to
        // compile here, exactly as it does in `analyze_atom`.
        RegexAtom::Literal(_)
        | RegexAtom::LiteralGrapheme(_)
        | RegexAtom::CharClass(_)
        | RegexAtom::Newline
        | RegexAtom::Any
        | RegexAtom::NotNewline
        | RegexAtom::UnicodeProp { .. }
        | RegexAtom::WsRule
        | RegexAtom::ZeroWidth
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
        | RegexAtom::CodeAssertion { .. }
        | RegexAtom::ClosureInterpolation { .. }
        | RegexAtom::VarDecl { .. }
        | RegexAtom::Backref(_)
        | RegexAtom::NamedBackref(_)
        | RegexAtom::VarInterp(_)
        | RegexAtom::RecurseSelf(_)
        | RegexAtom::TildeMarker => false,
    }
}
