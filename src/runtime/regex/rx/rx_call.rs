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

/// (rule, caller package, caller `:i`) → (token generation, the call's target).
type TargetCache = rustc_hash::FxHashMap<(Symbol, Symbol, bool), (u64, CallVerdict)>;

/// A call's frame, or why it takes the bridge (`MUTSU_VM_STATS`'s
/// `regex-walk:` line, `bridged=`).
pub(super) type CallVerdict = Result<CallTarget, &'static str>;

thread_local! {
    /// The verdict for a call, per (rule, caller package, caller `:i`),
    /// stamped with the token generation it was reached under — the inline
    /// cache of ADR-0135 D3. Kept only for a rule whose candidates the
    /// argument-less memo holds (a fully static one); anything else is
    /// resolved afresh at every call.
    static TARGETS: std::cell::RefCell<TargetCache> =
        std::cell::RefCell::new(rustc_hash::FxHashMap::default());
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
    /// No rule of that name: a builtin (`<.ws>`, `<wb>`, `<alpha>`, …) the walk's
    /// single-candidate arm decides, with at most one end.
    Single,
}

impl Interpreter {
    /// The frame `<name>` called from `pkg` runs as, or `Err(why)` when the
    /// call must take the bridge. `ic` is the caller's `:i`, which the walk scopes
    /// over the callee's body.
    // Cost: O(1) expected: one memoized candidate probe, plus the memoized
    // call-graph verdicts for the rule, per call; O(c) more for a proto of c
    // candidates (a program probe each).
    pub(super) fn rx_call_target(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
    ) -> CallVerdict {
        let spec = name.spec();
        // A call with arguments resolves per call (`rx_call_target_args`).
        if !spec.arg_exprs.is_empty() {
            return Err("args");
        }
        self.rx_call_blockers(name)?;
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

    /// What keeps any call of `<name>` off the compiled engine, whatever its
    /// arguments: a name resolved per call, or dispatch the engine does not
    /// model.
    // Cost: O(1).
    fn rx_call_blockers(&self, name: &NamedAtom) -> Result<(), &'static str> {
        let spec = name.spec();
        if spec.lookup_name == "::" || Self::may_name_lexical_regex(spec) {
            return Err("lexical-regex");
        }
        // Dispatch the compiled engine does not model: a `$*` rule parameter
        // that has to be installed around the call, a wrapped token, a custom
        // HOW.
        if crate::runtime::regex::regex_dynparams::ANY_DYNAMIC_TOKEN_PARAM
            .load(std::sync::atomic::Ordering::Relaxed)
        {
            return Err("dynamic-param");
        }
        if self.has_any_wrap_chains() {
            return Err("wrapped");
        }
        if !self.registry().grammar_custom_how.is_empty() {
            return Err("custom-how");
        }
        if !self.grammar_rule_dynvar_decls.is_empty() {
            return Err("rule-dynvar-decls");
        }
        // An enclosing call of this name is being evaluated by the walk's
        // growing-seed loop: this call may be its re-entry, which only the
        // walk's bookkeeping answers.
        if lr_name_active(spec.lookup_sym) {
            return Err("left-recursion-active");
        }
        Ok(())
    }

    /// The frame a `<name(…)>` call with arguments runs as. The arguments are
    /// evaluated here, once, against the caller's captures `caps`, and handed
    /// back with the verdict: a frame's callee was parsed for those values
    /// (`parsed_subrule_candidates`, memoized per rendered argument list), and
    /// a bridged call passes them to the walk's producer so user code in an
    /// argument never runs twice. `None` when an argument fails to evaluate:
    /// the call does not match, as in the walk.
    // Cost: the arguments' evaluation, then the candidate resolution
    // (memoized per argument list) and O(c) program probes for c candidates.
    pub(super) fn rx_call_target_args(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
        caps: &crate::runtime::regex_types::RegexCaptures,
    ) -> Option<(CallVerdict, Option<Vec<crate::value::Value>>)> {
        let spec = name.spec();
        if let Err(why) = self.rx_call_blockers(name) {
            // Bridged without evaluating: the producer evaluates them itself.
            return Some((Err(why), None));
        }
        let args = self.eval_regex_arg_list(&spec.arg_exprs, caps)?;
        // An object or closure argument is bound in the env for the callee's
        // match window (`install_subrule_dynamic_params`), which a frame the
        // run can backtrack into does not keep live: the walk's producer
        // binds it around the callee's whole match.
        // TODO: compile to bytecode with a binding op pair that backtracking
        // re-installs and removes, as `isolated-group-scoped` needs too.
        if crate::runtime::regex::regex_dynparams::regex_args_have_opaque(&args) {
            return Some((Err("args-opaque"), Some(args)));
        }
        let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, &args);
        // No rule of that name: a grammar method or a builtin, which the
        // walk's producer dispatches with these arguments.
        let verdict = if raw_empty {
            Err("args-method")
        } else {
            self.call_target_from_candidates(name, pkg, ic, candidates)
        };
        Some((verdict, Some(args)))
    }

    /// [`Self::rx_call_target`] with the one verdict a method definition can
    /// change: a plain grammar METHOD named like the rule is invoked by the
    /// walk's producer (`try_regex_subrule_as_method`), so such a call bridges.
    // Cost: O(1) expected.
    pub(super) fn rx_call_target_checked(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
    ) -> CallVerdict {
        let target = self.rx_call_target(name, pkg, ic)?;
        if matches!(target, CallTarget::Single)
            && self
                .registry()
                .method_overloads_present_sym(pkg, name.spec().lookup_sym)
        {
            return Err("grammar-method");
        }
        Ok(target)
    }

    /// [`Self::rx_call_target`]'s cache miss: resolve the rule's candidates and
    /// decide the shape of the call.
    fn resolve_call_target(&mut self, name: &NamedAtom, pkg: Symbol, ic: bool) -> CallVerdict {
        let spec = name.spec();
        let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, &[]);
        if raw_empty {
            return Ok(CallTarget::Single);
        }
        self.call_target_from_candidates(name, pkg, ic, candidates)
    }

    /// The shape of a call to the resolved `candidates`: a plain rule, a proto,
    /// or why it bridges.
    fn call_target_from_candidates(
        &mut self,
        name: &NamedAtom,
        pkg: Symbol,
        ic: bool,
        candidates: Arc<Vec<ParsedTokenCandidate>>,
    ) -> CallVerdict {
        let spec = name.spec();
        if candidates.is_empty() {
            return Err("no-candidates");
        }
        // `:m` remaps positions across the whole result set.
        if candidates.iter().any(|(parsed, _, _)| parsed.ignore_mark) {
            return Err("ignoremark");
        }
        // Several candidates without a proto dedup their ends across each
        // other; a mix of both is not a shape the walk's proto dispatch names.
        let proto = candidates.iter().all(|(_, _, sym)| sym.is_some());
        if !proto && (candidates.len() != 1 || candidates[0].2.is_some()) {
            return Err("multi-candidate");
        }
        // The walk's eager arm scopes the caller's `:i` over a proto candidate's
        // body (`subrule_candidate_ends`), which needs the body compiled under it:
        // that call bridges. A plain call is the walk's streamed shape, which
        // does not inherit `:i` — and neither does rakudo.
        if proto && ic && candidates.iter().any(|(parsed, _, _)| !parsed.ignore_case) {
            return Err("proto-inherited-i");
        }
        if self.subrule_has_qq_thunks(&spec.lookup_name, pkg) {
            return Err("qq-thunks");
        }
        if !self.subrule_cannot_left_reenter(spec.lookup_sym, pkg) {
            return Err("left-reenter");
        }
        if candidates
            .iter()
            .any(|(parsed, _, _)| program_for(parsed).is_none())
        {
            return Err("callee-declined");
        }
        if proto {
            return Ok(CallTarget::Proto(candidates));
        }
        let (parsed, sub_pkg, _) = &candidates[0];
        Ok(CallTarget::Plain(
            Arc::clone(program_for(parsed).ok_or("callee-declined")?),
            *sub_pkg,
        ))
    }

    /// The indexes of a proto's candidates in the order the call tries them:
    /// the walk's rank-then-match dispatch (ADR-0046), written to `out`.
    /// Ranking measures each candidate's declarative prefix, so it runs nothing
    /// (ADR-0009); a candidate that cannot match here is left out, and ties
    /// keep declaration order. `keys` is scratch (the run's, reused per call).
    // Cost: O(c·m + c log c), c = the candidates, m = one LTM measurement.
    pub(super) fn rx_rank_proto(
        &mut self,
        candidates: &[ParsedTokenCandidate],
        chars: &[char],
        pos: usize,
        keys: &mut Vec<(usize, (usize, usize))>,
        out: &mut Vec<usize>,
    ) {
        keys.clear();
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
            keys.push((idx, (plen.unwrap_or(0), measured.litlen)));
        }
        keys.sort_by_key(|(_, rank)| std::cmp::Reverse(*rank));
        out.clear();
        out.extend(keys.iter().map(|(idx, _)| *idx));
    }
}
