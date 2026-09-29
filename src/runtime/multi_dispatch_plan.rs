//! Type-keyed dispatch plans for bare-name multi-sub calls
//! ([#9967](https://github.com/tokuhirom/mutsu/issues/9967)).
//!
//! `func_multi_resolve_cache` caches a multi call's *winner*, which is sound
//! only when the winner is a pure function of the argument types. One
//! value-dependent candidate (`where`, a literal, a `subset` — `UInt` is one)
//! withholds that cache from every call of the family, and each call then
//! re-ran the whole resolver: gather the candidates from the registry under up
//! to four registry-key patterns, sort them, compute every candidate's rank
//! key, and only then try to bind. FiniteField's `multi infix:<*>(UInt $a,
//! UInt $b)` paid that on every arithmetic operator — ~55,000 instructions of
//! gathering and ranking for one `*`.
//!
//! Rakudo splits the same work the same way this module does: the candidate
//! list and its narrowness order are a property of the *types* at the call, so
//! they are computed once per type tuple, and only the bind-time checks that
//! can run user code — `where` clauses and subset predicates — run per call.
//! A [`BareMultiPlan`] is that type-keyed half: every gather pass the resolver
//! would try, in order, each already ranked. [`Interpreter::run_bare_multi_plan`]
//! is the per-call half.
//!
//! Everything a plan holds is computed from `(name, current package,
//! innermost lexical package, proto generation, registered subsets, argument
//! type keys)` and the functions map, whose generation tags the entry. The two
//! things that depend on the executing *frame* are handled apart: an imported
//! operator's visibility (#9944) is decided by the executing units, which join
//! the key for such a name, and a compunit-private name is never planned at
//! all.

use super::dispatch_candidates::CandidateRankKey;
use super::*;

/// One candidate of a cached dispatch-plan stage
/// ([`crate::runtime::multi_dispatch_plan`]).
pub(crate) struct PlanEntry {
    /// The rank key against the argument types the plan was built for.
    key: CandidateRankKey,
    /// The key also depends on the argument values, so it is recomputed per
    /// call (see `Interpreter::candidate_rank_reads_value`).
    reads_value: bool,
    def: Arc<FunctionDef>,
}

/// One gather pass of the bare-name resolver, ranked against the call's
/// argument types, in gather order (duplicates included).
pub(crate) type PlanStage = Vec<PlanEntry>;

/// The type-keyed half of a bare-name multi resolution. See the module docs.
pub(crate) struct BareMultiPlan {
    /// The passes to try, in order; the first that yields a winner decides.
    stages: Vec<PlanStage>,
    /// Whether any pass gathered a candidate — decides whether the resolver
    /// may fall back to the arity-only lookup when nothing binds.
    found_multi_candidates: bool,
}

/// What a [`BareMultiPlan`] is computed from, besides the functions map
/// (whose generation tags the entry).
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct BareMultiPlanKey {
    name: Symbol,
    /// The two inputs of the package search (`bare_name_packages`).
    package: Symbol,
    lexical_package: Option<Symbol>,
    proto_generation: u64,
    /// A subset declared later re-ranks a candidate constrained by it.
    subsets: usize,
    /// For an operator some `use` imported: `(current unit, executing unit,
    /// import-table generation)`, the inputs of `operator_candidate_visible`.
    visibility: Option<(Symbol, Symbol, u64)>,
    arg_keys: Vec<Symbol>,
}

impl Interpreter {
    /// Resolve a bare-name multi call through its (cached) dispatch plan.
    /// Behaviorally identical to the gather-rank-bind walk it replaced in
    /// `resolve_function_with_types`.
    // Cost: O(c log c) plus the bind attempts on a plan hit, c = candidates;
    // O(r) on a miss, r = registry keys sharing the base name.
    pub(crate) fn run_bare_multi_plan(
        &mut self,
        name: &str,
        arg_values: &[Value],
        search_pkgs: &[String],
        arity: usize,
    ) -> Option<Arc<FunctionDef>> {
        let plan = self.bare_multi_plan(name, arg_values, search_pkgs, arity);
        // Fingerprints of candidates an earlier pass already tried and that
        // did not bind; each wider pass re-gathers them, so skip them there.
        let mut rejected = std::collections::HashSet::new();
        for stage in &plan.stages {
            if let Some(def) = self.choose_from_plan_stage(name, arg_values, stage, &mut rejected) {
                return Some(def);
            }
        }
        // Fall back to arity-only if no proto declared and no multi candidates were found.
        // When multi candidates exist but none matched (e.g., sub-signature arity mismatch),
        // falling back would bypass the sub-signature check.
        if self.has_proto(name) || plan.found_multi_candidates {
            None
        } else {
            self.resolve_function_with_arity(name, arity)
                .and_then(|def| self.visible_operator_def(name, def))
        }
    }

    /// The plan for this call, from the cache when the arguments can be keyed.
    fn bare_multi_plan(
        &mut self,
        name: &str,
        arg_values: &[Value],
        search_pkgs: &[String],
        arity: usize,
    ) -> Arc<BareMultiPlan> {
        let key = self.bare_multi_plan_key(name, arg_values);
        let generation = self.fn_resolve_gen;
        if let Some(key) = &key
            && let Some(plan) = self.bare_multi_plan_cache.get(generation, key)
        {
            return plan.clone();
        }
        let plan = Arc::new(self.build_bare_multi_plan(name, arg_values, search_pkgs, arity));
        if let Some(key) = key {
            debug_assert_eq!(generation, self.fn_resolve_gen);
            self.bare_multi_plan_cache
                .insert(generation, key, plan.clone());
        }
        plan
    }

    /// The cache key for this call, or `None` when the plan must be built
    /// fresh: an argument whose dispatch is not captured by its type key, or a
    /// name that resolves differently per compilation unit.
    fn bare_multi_plan_key(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<BareMultiPlanKey> {
        if self.is_unit_scoped_routine_name(name) {
            return None;
        }
        let arg_keys = self.multi_arg_type_keys(arg_values)?;
        let visibility = self.operator_has_import_scope(name).then(|| {
            (
                self.current_unit,
                self.executing_unit_sym(),
                self.operator_import_gen,
            )
        });
        Some(BareMultiPlanKey {
            name: Symbol::intern(name),
            package: self.current_package_sym(),
            lexical_package: self
                .routine_stack()
                .last()
                .and_then(|frame| frame.lexical_package),
            proto_generation: self.registry().proto_generation(),
            subsets: self.registry().subsets.len(),
            visibility,
            arg_keys,
        })
    }

    /// Gather and rank every pass the bare-name resolver tries, without
    /// binding anything.
    fn build_bare_multi_plan(
        &mut self,
        name: &str,
        arg_values: &[Value],
        search_pkgs: &[String],
        arity: usize,
    ) -> BareMultiPlan {
        let mut stages: Vec<PlanStage> = Vec::new();
        let typed_prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/{}:", pkg, name, arity))
            .collect();
        let generic_keys: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/{}", pkg, name, arity))
            .collect();
        let mut found_multi_candidates = false;
        let base_keys = self.fn_keys_for_base(name);
        let mut candidates: Vec<(String, Arc<FunctionDef>)> = {
            let registry = self.registry();
            base_keys
                .iter()
                .filter_map(|key| registry.functions.get(key).map(|def| (key, def)))
                .filter(|(key, _)| {
                    let ks = key.as_str();
                    typed_prefixes.iter().any(|p| ks.starts_with(p))
                })
                .map(|(key, def)| (key.resolve(), def.clone()))
                .collect()
        };
        for key in &generic_keys {
            let key_sym = Symbol::intern(key);
            let m_prefix = format!("{}__m", key);
            let more: Vec<(String, Arc<FunctionDef>)> = {
                let registry = self.registry();
                base_keys
                    .iter()
                    .filter_map(|k| registry.functions.get(k).map(|def| (k, def)))
                    .filter(|(k, _)| **k == key_sym || k.as_str().starts_with(&m_prefix))
                    .map(|(k, def)| (k.resolve(), def.clone()))
                    .collect()
            };
            if !more.is_empty() {
                found_multi_candidates = true;
            }
            candidates.extend(more);
        }
        self.sort_candidates_by_specificity(&mut candidates);
        let exact_candidate_consumes_optional = candidates.iter().any(|(_, def)| {
            def.param_defs
                .iter()
                .any(|p| !p.named && (p.optional_marker || p.default.is_some()))
        });
        // An exact-arity candidate with no positional type constraint at all
        // (a bare `($x)`/`($unknown-type)` catch-all) is the WIDEST possible
        // signature, not a narrow one — it must still compete with a
        // different-arity candidate whose optional/default trailing
        // parameter lets it apply to this call too (`multi f($x) {...}` vs
        // `multi f(Int $x, Int $y = 10) {...}` called as `f(1)`: real Raku
        // picks the typed candidate). Without this check the fast path below
        // returned the catch-all unconditionally whenever it happened to be
        // the only candidate registered at the call's exact arity, silently
        // preferring it over a strictly narrower flexible-arity candidate
        // (found while making ASN::BER's enum/Int `serialize` dispatch pick
        // its catch-all `NYI` candidate over the real `Int`/enum ones).
        //
        // Only a candidate that actually HAS a non-named positional parameter
        // can be "untyped" this way — a zero-arity candidate (`multi f() {}`)
        // has no such parameter at all and is already the narrowest possible
        // match for a zero-argument call, not a catch-all competing with
        // wider optional-arity siblings (`multi f(Int $a?) {}`): flagging it
        // here regressed `roast/S06-multi/syntax.t`'s "exact arity match wins
        // over candidates with optionals" (`multi rt74900() {}` losing to
        // `multi rt74900(Int $a?) {}` for `rt74900()`).
        let exact_candidate_untyped = candidates.iter().any(|(_, def)| {
            def.param_defs
                .iter()
                .any(|p| !p.named && !p.slurpy && !p.double_slurpy)
                && self.candidate_specificity_rank_for_args(def, arg_values).1 == 0
        });
        // An exact-arity group made only of named parameters must compete with
        // flexible candidates that also carry a named parameter.  Rakudo
        // treats `multi f($x = 1, :$a)` and `multi f(:$b)` as an equal-
        // narrowness tie for `f()`, so declaration order decides. Returning
        // the exact named-only candidate here would skip that comparison and
        // make the later flexible candidate unreachable. A zero-arity
        // positional candidate (`multi f()`) still gets the fast path: unlike
        // the named-only case, it is the exact-arity winner over an optional
        // positional candidate.
        let exact_candidate_has_unnamed = candidates.iter().any(|(_, def)| {
            !def.param_defs
                .iter()
                .any(|p| p.named && !p.slurpy && !p.double_slurpy)
        });
        // An exact-arity candidate with no optional positional parameter is
        // already narrower than every default-arity fallback. Preserve that
        // fast path; otherwise `multi f(Int $x)` would lose to
        // `multi f(Int $x, Int $y = 7)` for `f(1)`. When an exact candidate
        // does consume an optional argument, however, it must compete with
        // longer signatures whose required parameters may describe the call
        // more precisely (the `is-approx` tolerance overloads).
        if !exact_candidate_consumes_optional
            && !exact_candidate_untyped
            && exact_candidate_has_unnamed
        {
            stages.push(self.rank_candidates_for_plan(name, arg_values, candidates.clone()));
        }
        // Include optional/default candidates with different registered arities
        // before choosing a winner. A candidate such as `(Numeric, Numeric,
        // Numeric, $desc = '')` is applicable to a three-argument call even
        // though its registration arity is four, and must compete with the
        // exact-arity `(Numeric, Numeric, $desc = '')` candidate rather than
        // being considered only after that wider candidate has already won.
        let optional_prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        let optional_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                optional_prefixes
                    .iter()
                    .any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| !p.named && (p.optional_marker || p.default.is_some()))
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !optional_candidates.is_empty() {
            found_multi_candidates = true;
        }
        candidates.extend(optional_candidates);
        self.sort_candidates_by_specificity(&mut candidates);
        stages.push(self.rank_candidates_for_plan(name, arg_values, candidates));
        // Try slurpy candidates with different arities (slurpy params accept
        // variable number of args, so the registered arity may differ from call arity).
        let mut slurpy_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, def)| {
                let ks = k.resolve();
                optional_prefixes
                    .iter()
                    .any(|prefix| ks.starts_with(prefix))
                    && def
                        .param_defs
                        .iter()
                        .any(|p| p.is_variadic() || p.is_capture_subsignature())
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !slurpy_candidates.is_empty() {
            found_multi_candidates = true;
        }
        slurpy_candidates.sort_by(|a, b| a.0.cmp(&b.0));
        stages.push(self.rank_candidates_for_plan(name, arg_values, slurpy_candidates));
        // Try candidates from other arities (e.g., optional/default positional params).
        // This allows calls with fewer args to match signatures like `$x = ...`.
        let mut any_arity_candidates: Vec<(String, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter(|(k, _)| {
                let ks = k.resolve();
                optional_prefixes
                    .iter()
                    .any(|prefix| ks.starts_with(prefix))
            })
            .map(|(k, def)| (k.resolve(), def.clone()))
            .collect();
        if !any_arity_candidates.is_empty() {
            found_multi_candidates = true;
        }
        self.sort_candidates_by_specificity(&mut any_arity_candidates);
        stages.push(self.rank_candidates_for_plan(name, arg_values, any_arity_candidates));
        stages.retain(|stage| !stage.is_empty());
        BareMultiPlan {
            stages,
            found_multi_candidates,
        }
    }

    /// Rank every candidate of a gathered list against `args`, in the
    /// gather's order, dropping only the operator candidates the running code
    /// cannot see. Nothing is bound. The result is a pure function of the
    /// candidates, the argument *types* (see [`Self::candidate_rank_key`]) and
    /// the executing units, which is what lets a
    /// [`crate::runtime::multi_dispatch_plan::BareMultiPlan`] keep it across
    /// calls.
    // Cost: O(c * p), c = candidates, p = parameters per candidate.
    pub(super) fn rank_candidates_for_plan(
        &mut self,
        name: &str,
        args: &[Value],
        mut candidates: Vec<(String, Arc<FunctionDef>)>,
    ) -> Vec<PlanEntry> {
        self.retain_visible_operator_candidates(name, &mut candidates);
        candidates
            .into_iter()
            .map(|(_, def)| PlanEntry {
                key: self.candidate_rank_key(&def, args),
                reads_value: self.candidate_rank_reads_value(&def),
                def,
            })
            .collect()
    }

    /// Whether [`Self::candidate_rank_key`] reads an argument's *value* for
    /// `def`, not only its type: a positional parameter whose "type" is `Inf`,
    /// `NaN` or a `constant` bound to a value (`multi f(Int $n, G)`), which
    /// [`Self::type_hierarchy_distance`] scores 0 only for the argument equal
    /// to it. Such a candidate's rank key cannot be kept across calls with the
    /// same argument types (`foo(Inf)` then `foo(NaN)`, roast
    /// S06-multi/type-based.t).
    // Cost: O(p), p = parameters of `def`.
    fn candidate_rank_reads_value(&self, def: &FunctionDef) -> bool {
        Self::dispatch_visible_params(def)
            .iter()
            .filter(|p| !p.named)
            .filter_map(|p| p.type_constraint.as_deref())
            .any(|tc| {
                let base = Self::constraint_base_name(tc);
                base == "Inf"
                    || base == "NaN"
                    || (!crate::runtime::utils::is_known_type_constraint(base)
                        && self
                            .type_name_binding(base)
                            .is_some_and(|v| !matches!(v.view(), ValueView::Package(_))))
            })
    }

    /// [`Self::choose_best_matching_candidate_excluding`] over one stage of a
    /// cached dispatch plan: `stage` is the gathered list, already filtered
    /// for visibility and ranked by [`Self::rank_candidates_for_plan`]. Only
    /// the per-call parts run here — the duplicate/`rejected` filter and the
    /// bind attempts, which are the only place a `where` clause or subset
    /// predicate is evaluated.
    ///
    /// Filtering after ranking selects exactly the list the unplanned path
    /// ranks: a rank key is computed per candidate, and the sort below is
    /// stable, so dropping an element before or after ranking leaves the
    /// others in the same order.
    // Cost: O(c log c) plus the bind attempts, c = candidates in the stage.
    pub(super) fn choose_from_plan_stage(
        &mut self,
        name: &str,
        args: &[Value],
        stage: &[PlanEntry],
        rejected: &mut std::collections::HashSet<u64>,
    ) -> Option<Arc<FunctionDef>> {
        let mut seen: Vec<u64> = Vec::with_capacity(stage.len());
        let mut ranked: Vec<(CandidateRankKey, Arc<FunctionDef>)> = Vec::with_capacity(stage.len());
        for entry in stage {
            let fp = entry.def.body_fingerprint();
            if rejected.contains(&fp) || seen.contains(&fp) {
                continue;
            }
            seen.push(fp);
            let key = if entry.reads_value {
                self.candidate_rank_key(&entry.def, args)
            } else {
                entry.key
            };
            ranked.push((key, entry.def.clone()));
        }
        if ranked.is_empty() {
            return None;
        }
        ranked.sort_by(|a, b| Self::candidate_rank_cmp(a.0, b.0));
        self.bind_ranked_candidates(name, args, ranked, Some(rejected))
    }
}
