//! Resolving a `<subrule>` for the ADR-0099 Stage 1 first-character set —
//! the one place the prefilter is allowed to look *through* a rule name.
//!
//! ADR-0099 §4 constraint 3 puts a condition on this rather than a ban: "a
//! prefix derived through `<subrule>` must be keyed by invocant package and
//! `TOKEN_DEFS_GEN` (dynamic override via `H is G` is legal), **or decline**".
//! The first three slices of Stage 1 (#8285, #8446, #8457) took the decline;
//! this module takes the condition, because declining is a cliff rather than a
//! shortfall — a `<subrule>` anywhere in a pattern's leading run makes the
//! whole derivation decline, so `/ <kw> /` with `regex kw { 'zzzq' | 'kkkz' }`
//! entered the engine at every one of a 135,000-character subject's positions
//! while the same alternation written inline was filtered down to two
//! characters.
//!
//! # What "keyed by package and generation" buys, and why it is not enough
//!
//! A rule name is not a stable reference to a body. `TOKEN_DEFS_GEN` moves
//! whenever any rule is (re)defined, and the same name resolves to different
//! bodies in different packages — `grammar H is G { token x { ... } }`
//! overriding `G`'s `x` is ordinary Raku. The memo this feeds
//! ([`super::regex_prefilter_memo::pattern_prefilter_in_pkg`]) therefore carries
//! both, and a pattern that mentions no rule name at all never reaches it.
//!
//! But two more things can change a rule's *parse* without moving the
//! generation, and both are declined here rather than keyed:
//!
//! - a body whose text interpolates a runtime value (`token t { $sep }`) is
//!   re-parsed per call, so no generation-keyed memo can hold it. The test is
//!   the same one the call-graph analysis uses,
//!   [`Interpreter::rule_body_edges_are_generation_stable`], which correctly
//!   ignores a sigil that only appears inside a `{ ... }` block — the common
//!   `token part { \w+ { $n++ } }` shape stays analyzable;
//! - a reference that names a *lexical* `Regex` (`<&$re>`, `<&re>`) resolves
//!   against the caller's scope, which the key does not carry at all
//!   ([`Interpreter::may_name_lexical_regex`]).
//!
//! # Declines, and why each is unsound rather than merely imprecise
//!
//! - **arguments** (`<expr(3)>`) and **`<::(EXPR)>`** resolve per call;
//! - **a name no rule answers to** — an empty candidate list is either a
//!   builtin assertion (`<alpha>`, `<sym>`, which the parser has already
//!   lowered to a character class where it can) or a grammar *method*, i.e.
//!   arbitrary user code whose result no static walk can bound;
//! - **a name a grammar method also answers to**, even when rules exist: which
//!   one the engine picks is not this module's decision to re-derive;
//! - **recursion**. A rule reached from itself is answered "unknown" rather
//!   than by unrolling, which is the only sound direction: the first-set must
//!   be a *superset* of what can match, and a partially-walked recursive rule
//!   yields a subset.
//!
//! Every one of those returns `None`, which widens to the status quo — the
//! scan enumerates every position, exactly as it did before Stage 1 existed.

use super::super::*;
use super::regex_prefilter_analysis::{Analyzer, Ctx, Info, MAX_DEPTH, Seq, walk_pattern};
use super::regex_prefilter_firstset::FirstSet;
use super::regex_token_resolve::ParsedTokenCandidate;
use crate::symbol::Symbol;

/// A resolved subrule: its candidate bodies, plus the `(pkg, name)` node that
/// identifies it for the analysis's recursion guard.
pub(super) struct ResolvedSubrule {
    pub(super) candidates: std::sync::Arc<Vec<ParsedTokenCandidate>>,
    pub(super) node: (Symbol, Symbol),
}

impl Interpreter {
    /// The bodies `<atom_text>` dispatches to in `pkg`, or `None` when the
    /// prefilter may not look through this reference at all.
    ///
    /// Resolution goes through [`Interpreter::resolve_parsed_token_candidates_in_pkg`],
    /// the same memoized `(pkg, name)` + `TOKEN_DEFS_GEN` table the matcher
    /// itself resolves through — so this is not a second reading of the rule
    /// registry, and a redefinition invalidates both at once.
    pub(super) fn prefilter_subrule_candidates(
        &mut self,
        atom_text: &str,
        pkg: Symbol,
    ) -> Option<ResolvedSubrule> {
        let spec = Self::parse_named_regex_lookup_spec(atom_text);
        // Both resolve per call against something the memo key does not carry:
        // the argument values, and the symbol the indirection evaluates to.
        if !spec.arg_exprs.is_empty() || spec.lookup_name == "::" {
            return None;
        }
        // `<&re>` / `<&$re>` read the caller's lexical scope.
        if Self::may_name_lexical_regex(&spec) {
            return None;
        }
        // A body re-parsed per call cannot be held by a generation-keyed memo.
        if !self.rule_body_edges_are_generation_stable(&spec.lookup_name, pkg) {
            return None;
        }
        let candidates =
            self.resolve_parsed_token_candidates_in_pkg(&spec.lookup_name, spec.lookup_sym, pkg)?;
        // Empty means no token/regex/rule answers to this name here: a builtin
        // assertion, or a grammar method. Neither is a body to walk.
        if candidates.is_empty() {
            return None;
        }
        // A method of the same name is user code the walk cannot bound, so its
        // mere existence declines even when rules answer too — deciding which
        // one the engine would pick is exactly the second definition
        // constraint 1 forbids.
        if self
            .registry()
            .user_method_overloads(pkg.as_str(), &spec.lookup_name)
            .is_some()
        {
            return None;
        }
        Some(ResolvedSubrule {
            node: (pkg, spec.lookup_sym),
            candidates,
        })
    }
}

/// The first-set of a `<subrule>` call: the union over every body the name
/// dispatches to, which is the same shape as an alternation because that is
/// what a multi-candidate rule is (LTM picks between them, but any of them may
/// be the one that matches, so the set has to admit all).
///
/// Declines — and so widens to "every position" — whenever the reference
/// cannot be resolved against `(pkg, TOKEN_DEFS_GEN)` alone
/// ([`super::regex_prefilter_subrule`]), whenever the walk is already inside
/// this rule (recursion: a partially-walked recursive rule yields a *subset*
/// of what can match, the one direction that is unsound), and whenever the
/// resolution budget is spent.
pub(super) fn analyze_subrule(an: &mut Analyzer, atom_text: &str, ctx: Ctx) -> Option<Info> {
    if ctx.depth > MAX_DEPTH || an.budget == 0 {
        return None;
    }
    let interp = an.interp.as_deref_mut()?;
    an.budget -= 1;
    let resolved = interp.prefilter_subrule_candidates(atom_text, ctx.pkg)?;
    if an.active.contains(&resolved.node) {
        return None;
    }
    // Recorded before the walk rather than after: a derivation that resolved a
    // rule name and then declined on its body is still not a pure function of
    // the pattern, so it must not reach the pattern-keyed memo either.
    an.resolved_subrule = true;
    an.active.push(resolved.node);
    let seq = subrule_candidates_seq(an, &resolved.candidates, ctx);
    an.active.pop();
    let seq = seq?;
    Some(Info {
        first: seq.first,
        nullable: seq.nullable,
        min_len: seq.min_len,
    })
}

/// The union of a resolved rule's candidate bodies, each walked in the package
/// that defined it.
fn subrule_candidates_seq(
    an: &mut Analyzer,
    candidates: &[super::regex_token_resolve::ParsedTokenCandidate],
    ctx: Ctx,
) -> Option<Seq> {
    let mut first = FirstSet::empty();
    let mut nullable = false;
    let mut min_len = usize::MAX;
    for (parsed, sub_pkg, _) in candidates {
        // One unanalyzable candidate sinks the rule, exactly as one
        // unanalyzable branch sinks an alternation: the others say nothing
        // about what that one could match.
        let seq = walk_pattern(an, parsed, *sub_pkg, ctx.depth + 1)?;
        first.union(&seq.first);
        nullable |= seq.nullable;
        min_len = min_len.min(seq.min_len);
    }
    Some(Seq {
        first,
        nullable,
        min_len: if candidates.is_empty() { 0 } else { min_len },
    })
}
