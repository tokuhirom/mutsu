//! Resolving a `<subrule>` call for the compiled engine (ADR-0135 D3).
//!
//! A call is a frame in the run's own loop when the callee is a plain rule, or
//! a proto whose candidates all have programs, and a *bridge* to the walk's
//! producer otherwise (D5). The frame shapes are the ones the walk resolves
//! with one end per candidate, with the one difference the compiled engine
//! allows: a rule that calls itself is fine as long as it cannot re-enter *at
//! the same position* (`subrule_cannot_left_reenter`), because a frame, unlike
//! the walk's stream, needs no left-recursion activation to stay sound.

use std::sync::Arc;

use super::super::regex_lr_state::lr_name_active;
use super::super::regex_token_resolve::ParsedTokenCandidate;
use super::RxProgram;
use super::rx_entry::program_for;
use crate::runtime::Interpreter;
use crate::runtime::regex_types::NamedAtom;
use crate::symbol::Symbol;

thread_local! {
    /// The verdict for a call, per (rule, caller package, caller `:i`),
    /// stamped with the token generation it was reached under — the inline
    /// cache of ADR-0135 D3. Kept only for a rule whose candidates the
    /// argument-less memo holds (a fully static one); anything else is
    /// resolved afresh at every call.
    static TARGETS: std::cell::RefCell<
        rustc_hash::FxHashMap<(Symbol, Symbol, bool), (u64, Option<CallTarget>)>,
    > = std::cell::RefCell::new(rustc_hash::FxHashMap::default());
}

/// What a `<subrule>` call runs as a frame.
#[derive(Clone)]
pub(super) enum CallTarget {
    /// One plain rule: its program and the package its body matches in (a rule
    /// is matched in the package that defines it).
    Plain(Arc<RxProgram>, Symbol),
    /// A proto: the `:sym<…>` candidates, ranked at the call by the walk's own
    /// LTM measurement (ADR-0046), of which the first that matches wins.
    Proto(Arc<Vec<ParsedTokenCandidate>>),
}

impl Interpreter {
    /// The frame `<name>` called from `pkg` runs as, or `None` when the call
    /// must take the bridge. `ic` is the caller's `:i`, which the walk scopes
    /// over the callee's body.
    // Cost: O(1) expected: one memoized candidate probe, plus the memoized
    // call-graph verdicts for the rule, per call; O(c) more for a proto of c
    // candidates (a program probe each).
    pub(super) fn rx_call_target(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
    ) -> Option<CallTarget> {
        let spec = name.spec();
        // Shapes that resolve per call, never to a fixed body.
        if !spec.arg_exprs.is_empty()
            || spec.lookup_name == "::"
            || Self::may_name_lexical_regex(spec)
        {
            return None;
        }
        // Dispatch the compiled engine does not model: a `$*` rule parameter
        // that has to be installed around the call, a wrapped token, a custom
        // HOW.
        if crate::runtime::regex::regex_dynparams::ANY_DYNAMIC_TOKEN_PARAM
            .load(std::sync::atomic::Ordering::Relaxed)
            || self.has_any_wrap_chains()
            || !self.registry().grammar_custom_how.is_empty()
            || !self.grammar_rule_dynvar_decls.is_empty()
        {
            return None;
        }
        // An enclosing call of this name is being evaluated by the walk's
        // growing-seed loop: this call may be its re-entry, which only the
        // walk's bookkeeping answers.
        if lr_name_active(spec.lookup_sym) {
            return None;
        }
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        let key = (spec.lookup_sym, pkg, ic);
        if let Some(hit) = TARGETS.with(|c| {
            c.borrow()
                .get(&key)
                .filter(|(cached, _)| *cached == generation)
                .map(|(_, target)| target.clone())
        }) {
            return hit;
        }
        let target = self.resolve_call_target(name, pkg, ic);
        if Self::parsed_candidates_are_memoized(spec.lookup_sym, pkg) {
            TARGETS.with(|c| {
                c.borrow_mut().insert(key, (generation, target.clone()));
            });
        }
        target
    }

    /// [`Self::rx_call_target`]'s cache miss: resolve the rule's candidates and
    /// decide the shape of the call.
    fn resolve_call_target(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
    ) -> Option<CallTarget> {
        let spec = name.spec();
        let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, &[]);
        if raw_empty || candidates.is_empty() {
            return None;
        }
        // `:m` remaps positions across the whole result set; an inherited `:i`
        // needs the body compiled under it.
        if candidates
            .iter()
            .any(|(parsed, _, _)| parsed.ignore_mark || (ic && !parsed.ignore_case))
        {
            return None;
        }
        // Several candidates without a proto dedup their ends across each
        // other; a mix of both is not a shape the walk's proto dispatch names.
        let proto = candidates.iter().all(|(_, _, sym)| sym.is_some());
        if !proto && (candidates.len() != 1 || candidates[0].2.is_some()) {
            return None;
        }
        if self.subrule_has_qq_thunks(&spec.lookup_name, pkg) {
            return None;
        }
        if !self.subrule_cannot_left_reenter(spec.lookup_sym, pkg) {
            return None;
        }
        if candidates
            .iter()
            .any(|(parsed, _, _)| program_for(parsed).is_none())
        {
            return None;
        }
        if proto {
            return Some(CallTarget::Proto(candidates));
        }
        let (parsed, sub_pkg, _) = &candidates[0];
        Some(CallTarget::Plain(
            Arc::clone(program_for(parsed)?),
            *sub_pkg,
        ))
    }

    /// The indexes of a proto's candidates in the order the call tries them:
    /// the walk's rank-then-match dispatch (ADR-0046). Ranking measures each
    /// candidate's declarative prefix, so it runs nothing (ADR-0009); a
    /// candidate that cannot match here is left out, and ties keep declaration
    /// order.
    // Cost: O(c·m + c log c), c = the candidates, m = one LTM measurement.
    pub(super) fn rx_rank_proto(
        &mut self,
        candidates: &[ParsedTokenCandidate],
        chars: &[char],
        pos: usize,
    ) -> Vec<usize> {
        let mut ranked: Vec<(usize, (usize, usize))> = Vec::with_capacity(candidates.len());
        for (idx, (parsed, sub_pkg, _)) in candidates.iter().enumerate() {
            let measured = self.ltm_measure(parsed, chars, pos, *sub_pkg);
            let (plen, stopped) = (measured.len, measured.stopped);
            // ADR-0022 §4.1's contract: `(None, false)` is a sound "this
            // candidate cannot match here" verdict and may filter; `(None,
            // true)` only means the measurement was cut short, so the
            // candidate is kept, ranked at 0.
            if plen.is_none() && !stopped {
                continue;
            }
            ranked.push((idx, (plen.unwrap_or(0), measured.litlen)));
        }
        ranked.sort_by_key(|(_, rank)| std::cmp::Reverse(*rank));
        ranked.into_iter().map(|(idx, _)| idx).collect()
    }
}
