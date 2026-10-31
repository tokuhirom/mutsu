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
//! A [`BareMultiPlan`] is that type-keyed half: every candidate of the name,
//! in every arity, ranked and sorted into rakudo's topological tiers over the
//! whole set (#12795). [`Interpreter::run_bare_multi_plan`] is the per-call
//! half.
//!
//! Everything a plan holds is computed from `(name, current package,
//! innermost lexical package, proto generation, registered subsets, argument
//! type keys)` and the functions map, whose generation tags the entry. The two
//! things that depend on the executing *frame* are handled apart: an imported
//! operator's visibility (#9944) is decided by the executing units, which join
//! the key for such a name, and so do those of a compunit-private name, which
//! is planned per asking unit (as the plain-sub memo keys it, ADR-12529).

use super::multi_rank::{self, MultiRank};
use super::*;

/// One candidate of a cached dispatch plan
/// ([`crate::runtime::multi_dispatch_plan`]).
pub(crate) struct PlanEntry {
    /// The rank key against the argument types the plan was built for.
    pub(super) key: MultiRank,
    /// The key also depends on the argument values, so it is recomputed per
    /// call (see `Interpreter::candidate_rank_reads_value`).
    pub(super) reads_value: bool,
    pub(super) def: Arc<FunctionDef>,
}

/// Every candidate of a bare-name multi, ranked and tiered against the
/// call's argument types, unique by fingerprint, in gather order.
pub(crate) type PlanStage = Vec<PlanEntry>;

/// The type-keyed half of a bare-name multi resolution. See the module docs.
pub(crate) struct BareMultiPlan {
    stage: PlanStage,
    /// The stage's dispatch program (`multi_dispatch_program.rs`), built from
    /// the first call that runs it; `None` inside when the stage needs the
    /// general walk.
    program: std::sync::OnceLock<Option<super::multi_dispatch_program::StageProgram>>,
    /// Whether the gather found a candidate — decides whether the resolver
    /// may fall back to the arity-only lookup when nothing binds.
    found_multi_candidates: bool,
    /// Whether the plan was built for a cacheable argument-type key.
    keyed: bool,
    /// For a captured-dispatcher plan: the candidates in deferral order (the
    /// `callsame` / `nextsame` chain), which do not depend on the arguments.
    captured_remaining: Vec<Arc<FunctionDef>>,
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
    /// For a compunit-scoped name (`my multi sub`, a module's own `our`
    /// routines): `(current unit, executing unit, executing unit for a module
    /// load)`, the same inputs the plain-sub memo (`PlainFnResolveKey`) keys such a name by. The
    /// plan answers per asking unit; without them in the key the name was
    /// never planned and every call re-gathered and re-ranked all its
    /// candidates (ADR-15737; #15786).
    units: Option<(Symbol, Symbol, Symbol)>,
    /// For a captured multi dispatcher (`my proto sub f(|) {*}` called through
    /// its value): the ids of the captured candidate `Sub`s, which fix the
    /// candidate set the plan was built over. `None` for a by-name plan.
    captured: Option<Vec<u64>>,
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
        arg_keys: Option<&[Symbol]>,
        search_pkgs: &[Symbol],
        arity: usize,
    ) -> Resolved {
        let plan = self.bare_multi_plan(name, arg_values, arg_keys, search_pkgs);
        // A program is specific to the argument-type key, so only a plan the
        // cache keyed may build one (an unkeyed plan is per call).
        let program = if plan.keyed && !plan.stage.is_empty() {
            plan.program
                .get_or_init(|| self.build_stage_program(arg_values, &plan.stage))
                .as_ref()
        } else {
            None
        };
        let winner = match program {
            Some(program) => self.run_stage_program(name, arg_values, program)?,
            None if plan.stage.is_empty() => None,
            None => self.choose_from_plan_stage(name, arg_values, &plan.stage)?,
        };
        if winner.is_some() {
            return Ok(winner);
        }
        // Fall back to arity-only if no proto declared and no multi candidates were found.
        // When multi candidates exist but none matched (e.g., sub-signature arity mismatch),
        // falling back would bypass the sub-signature check.
        if self.has_proto(name) || plan.found_multi_candidates {
            Ok(None)
        } else {
            Ok(self
                .resolve_function_with_arity(name, arity)
                .and_then(|def| self.visible_operator_def(name, def)))
        }
    }

    /// The plan for this call, from the cache when the arguments can be keyed.
    fn bare_multi_plan(
        &mut self,
        name: &str,
        arg_values: &[Value],
        arg_keys: Option<&[Symbol]>,
        search_pkgs: &[Symbol],
    ) -> Arc<BareMultiPlan> {
        let key = self.bare_multi_plan_key(name, arg_values, arg_keys);
        let generation = self.caches.fn_resolve_gen;
        if let Some(key) = &key
            && let Some(plan) = self
                .caches
                .bare_multi_plan_cache
                .get(generation, key, || self.dispatch.fn_family_gens.of_name(key.name))
        {
            return plan.clone();
        }
        let mut plan = self.build_bare_multi_plan(name, arg_values, search_pkgs);
        plan.keyed = key.is_some();
        let plan = Arc::new(plan);
        if let Some(key) = key {
            debug_assert_eq!(generation, self.caches.fn_resolve_gen);
            let name = key.name;
            self.caches.bare_multi_plan_cache.insert(
                generation,
                self.caches.fn_resolve_gen,
                || self.dispatch.fn_family_gens.of_name(name),
                key,
                plan.clone(),
            );
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
        arg_keys: Option<&[Symbol]>,
    ) -> Option<BareMultiPlanKey> {
        let units = self.is_unit_scoped_routine_name(name).then(|| {
            (
                self.current_unit,
                self.executing_unit_sym(),
                self.executing_unit_sym_for_module_load(),
            )
        });
        let arg_keys = match arg_keys {
            Some(keys) => keys.to_vec(),
            // Named arguments are keyed by name and value type, the keys the
            // keyed callers already pass (`resolve_function_with_types_cached`):
            // refusing them left every `f(..., :force)` call unplanned.
            None => self.multi_arg_type_keys_with_named(arg_values)?,
        };
        let visibility = self.operator_has_import_scope(name).then(|| {
            (
                self.current_unit,
                self.executing_unit_sym(),
                self.module.operator_import_gen,
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
            units,
            captured: None,
            arg_keys,
        })
    }

    /// Gather every candidate of the name, in every arity, and rank them
    /// together, without binding anything.
    ///
    /// Rakudo sorts a multi's whole candidate list into topological tiers
    /// once and then walks it tier by tier, skipping the candidates the call
    /// cannot bind (#12795). The plan does the same: one list, deduplicated
    /// by fingerprint (one declaration is registered under several keys) and
    /// tiered over all of it, so a candidate of another arity still sits
    /// between two others whenever rakudo puts it there.
    // Cost: O(k + c^2), k = registry keys sharing the base name,
    // c = candidates.
    fn build_bare_multi_plan(
        &mut self,
        name: &str,
        arg_values: &[Value],
        search_pkgs: &[Symbol],
    ) -> BareMultiPlan {
        let prefixes: Vec<String> = search_pkgs
            .iter()
            .map(|pkg| format!("{}::{}/", pkg, name))
            .collect();
        let base_keys = self.fn_keys_for_base(name);
        let mut candidates: Vec<(String, Arc<FunctionDef>)> = {
            let registry = self.registry();
            base_keys
                .iter()
                .filter_map(|key| registry.functions.get(key).map(|def| (key, def)))
                .filter(|(key, _)| {
                    let ks = key.as_str();
                    prefixes.iter().any(|p| ks.starts_with(p))
                })
                .map(|(key, def)| (key.resolve(), def.clone()))
                .collect()
        };
        let found_multi_candidates = !candidates.is_empty();
        self.sort_candidates_by_specificity(&mut candidates);
        let mut seen = std::collections::HashSet::new();
        candidates.retain(|(_, def)| seen.insert(def.body_fingerprint()));
        BareMultiPlan {
            stage: self.rank_candidates_for_plan(name, arg_values, candidates),
            program: std::sync::OnceLock::new(),
            found_multi_candidates,
            keyed: false,
            captured_remaining: Vec::new(),
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
        self.rank_candidates_unfiltered(args, candidates)
    }

    /// [`Self::rank_candidates_for_plan`] over candidates the caller already
    /// holds in scope (a captured dispatcher's own), so no visibility filter.
    // Cost: O(c * p), c = candidates, p = parameters per candidate.
    fn rank_candidates_unfiltered(
        &mut self,
        args: &[Value],
        candidates: Vec<(String, Arc<FunctionDef>)>,
    ) -> Vec<PlanEntry> {
        let mut entries: Vec<PlanEntry> = candidates
            .into_iter()
            .map(|(_, def)| PlanEntry {
                // The candidate's parameter types are named in the scope that
                // declared it: a module-private `my constant` type is not
                // visible from the caller (#15087).
                key: self.with_candidate_scope(&def, |this| this.candidate_rank_key(&def, args)),
                reads_value: self.candidate_rank_reads_value(&def),
                def,
            })
            .collect();
        {
            let _scope = self.conformance_scope();
            multi_rank::assign_tiers(&mut entries, |entry| &mut entry.key, &|x, y| {
                self.nominal_type_conforms(x, y)
            });
        }
        entries
    }

    /// Whether [`Self::candidate_rank_key`] reads an argument's *value* for
    /// `def`, not only its type: a positional parameter whose "type" is `Inf`,
    /// `NaN` or a `constant` bound to a value (`multi f(Int $n, G)`), which
    /// [`Self::type_hierarchy_distance`] scores 0 only for the argument equal
    /// to it. Such a candidate's rank key cannot be kept across calls with the
    /// same argument types (`foo(Inf)` then `foo(NaN)`, roast
    /// S06-multi/type-based.t).
    // Cost: O(p), p = parameters of `def`.
    pub(super) fn candidate_rank_reads_value(&self, def: &FunctionDef) -> bool {
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

    /// [`Self::choose_best_matching_candidate_excluding`] over a cached
    /// dispatch plan: `stage` is the gathered list, already unique by
    /// fingerprint, filtered for visibility and ranked by
    /// [`Self::rank_candidates_for_plan`]. Only the per-call parts run here —
    /// re-ranking a value-reading key and the bind attempts, which are the
    /// only place a `where` clause or subset predicate is evaluated.
    // Cost: O(c log c) plus the bind attempts, c = candidates in the stage.
    pub(super) fn choose_from_plan_stage(
        &mut self,
        name: &str,
        args: &[Value],
        stage: &[PlanEntry],
    ) -> Resolved {
        let core_outranks = Self::core_io_path_candidate_applies(name, args);
        let mut ranked: Vec<(MultiRank, Arc<FunctionDef>)> = Vec::with_capacity(stage.len());
        for entry in stage {
            if core_outranks && Self::candidate_is_untyped_where(&entry.def) {
                continue;
            }
            let key = if entry.reads_value {
                self.with_candidate_scope(&entry.def, |this| {
                    this.candidate_rank_key(&entry.def, args)
                })
            } else {
                entry.key
            };
            ranked.push((key, entry.def.clone()));
        }
        if ranked.is_empty() {
            return Ok(None);
        }
        // A key re-ranked against this call's values needs its tier again.
        if stage.iter().any(|entry| entry.reads_value) {
            let _scope = self.conformance_scope();
            multi_rank::assign_tiers(&mut ranked, |(key, _)| key, &|x, y| {
                self.nominal_type_conforms(x, y)
            });
        }
        ranked.sort_by(|a, b| a.0.total_cmp(&b.0));
        self.bind_ranked_candidates(name, args, None, ranked, None)
    }

    /// The cached, keyed plan of a bare multi call with argument type keys
    /// `arg_keys`, when its stage has an inline-checks program
    /// (`StageProgram::checks_are_inline_wheres`): what the multi light call
    /// of a value-dependent family runs (ADR-12529 §2.4). `None` for an
    /// imported operator, whose visibility depends on the executing unit.
    // Cost: O(a) for the key, a = arguments, plus one cache probe.
    pub(crate) fn light_bare_multi_plan(
        &mut self,
        name: &str,
        args: &[Value],
        arg_keys: &[Symbol],
    ) -> Option<(BareMultiPlanKey, Arc<BareMultiPlan>)> {
        let key = self.bare_multi_plan_key(name, args, Some(arg_keys))?;
        if key.visibility.is_some() {
            return None;
        }
        let plan = self.caches.bare_multi_plan_cache.get(
            self.caches.fn_resolve_gen,
            &key,
            || self.dispatch.fn_family_gens.of_name(key.name),
        )?;
        let usable = plan.keyed
            && plan
                .program
                .get()
                .is_some_and(|p| p.as_ref().is_some_and(|p| p.checks_are_inline_wheres()));
        usable.then_some((key, plan))
    }

    /// Whether `key` (from [`Self::light_bare_multi_plan`]) still describes a
    /// call made here: the same package search inputs, proto generation and
    /// subsets. Its name and argument keys are the caller's to match.
    // Cost: O(1), one registry read.
    pub(crate) fn light_plan_key_current(&self, key: &BareMultiPlanKey) -> bool {
        key.units.is_none_or(|units| {
            units
                == (
                    self.current_unit,
                    self.executing_unit_sym(),
                    self.executing_unit_sym_for_module_load(),
                )
        }) && key.package == self.current_package_sym()
            && key.lexical_package
                == self
                    .routine_stack()
                    .last()
                    .and_then(|frame| frame.lexical_package)
            && {
                let registry = self.registry();
                key.proto_generation == registry.proto_generation()
                    && key.subsets == registry.subsets.len()
            }
    }

    /// Run `plan`'s stage program against `args`: the winner, if any.
    // Cost: O(c) plus the value checks, c = candidates up to the best rank.
    pub(crate) fn run_light_bare_multi_plan(
        &mut self,
        name: &str,
        args: &[Value],
        plan: &BareMultiPlan,
    ) -> Resolved {
        match plan.program.get() {
            Some(Some(program)) => self.run_stage_program(name, args, program),
            _ => Ok(None),
        }
    }

    /// The candidates `plan`'s stage program can pick (empty without one).
    // Cost: O(s), s = program steps.
    pub(crate) fn plan_program_candidates(plan: &BareMultiPlan) -> Vec<Arc<FunctionDef>> {
        match plan.program.get() {
            Some(Some(program)) => program.candidates().cloned().collect(),
            _ => Vec::new(),
        }
    }
}

impl Interpreter {
    /// The winner of a captured multi dispatcher's `family` candidates for
    /// `args`, through a plan cached per (candidate set, argument types):
    /// the winner and the deferral chain `callsame` / `nextsame` walk. `None`
    /// when the call cannot be keyed (a Proxy, a Junction, an imported
    /// operator, ...), which leaves it to the by-value walk. Behaviorally the
    /// walk [`Self::pick_captured_multi_candidate`] does per call: build a
    /// `FunctionDef` per candidate, fingerprint, rank, sort and bind.
    // Cost: O(c) for the candidate-set key plus the bind attempts on a hit,
    // c = captured candidates; O(c * p + c log c) on a miss.
    pub(super) fn captured_multi_plan_winner(
        &mut self,
        family: &str,
        candidates: &[Value],
        args: &[Value],
    ) -> Option<Result<Option<(Arc<FunctionDef>, Vec<Arc<FunctionDef>>)>, RuntimeError>> {
        let arg_keys = self.multi_arg_type_keys_with_named(args)?;
        // `visibility` (an imported name) stays in the key; the captured
        // candidates need no visibility filter, they are the dispatcher's own.
        let mut key = self.bare_multi_plan_key(family, args, Some(&arg_keys))?;
        let mut ids = Vec::with_capacity(candidates.len());
        for code in candidates {
            let ValueView::Sub(data) = code.view() else {
                return None;
            };
            ids.push(data.id);
        }
        key.captured = Some(ids);
        let generation = self.caches.fn_resolve_gen;
        let plan = match self
            .caches
            .bare_multi_plan_cache
            .get(generation, &key, || self.dispatch.fn_family_gens.of_name(key.name))
        {
            Some(plan) => plan.clone(),
            None => {
                let plan = Arc::new(self.build_captured_multi_plan(family, candidates, args));
                debug_assert_eq!(generation, self.caches.fn_resolve_gen);
                let name = key.name;
                self.caches.bare_multi_plan_cache.insert(
                    generation,
                    self.caches.fn_resolve_gen,
                    || self.dispatch.fn_family_gens.of_name(name),
                    key,
                    plan.clone(),
                );
                plan
            }
        };
        Some(
            self.choose_from_plan_stage(family, args, &plan.stage)
                .map(|winner| winner.map(|w| (w, plan.captured_remaining.clone()))),
        )
    }

    /// The plan of a captured dispatcher's candidates against `args`' types.
    // Cost: O(c * p + c log c), c = candidates, p = parameters per candidate.
    fn build_captured_multi_plan(
        &mut self,
        family: &str,
        candidates: &[Value],
        args: &[Value],
    ) -> BareMultiPlan {
        // One declared `multi` can be captured more than once (it sits under
        // several registry keys), each copy a distinct Sub value: keep the
        // first copy of each declaration.
        let mut seen = std::collections::HashSet::new();
        let rows: Vec<(String, Arc<FunctionDef>)> = candidates
            .iter()
            .filter(|code| match code.view() {
                ValueView::Sub(data) => seen.insert(crate::ast::function_body_fingerprint(
                    &data.params,
                    &data.param_defs,
                    &data.body,
                )),
                _ => false,
            })
            .filter_map(|code| {
                let mut def = Self::dispatchee_function_def(code, family)?;
                // Its `where` clauses and defaults run in its declaring unit.
                if let ValueView::Sub(data) = code.view() {
                    def.source_file = data.source_file.clone();
                }
                Some((family.to_string(), Arc::new(def)))
            })
            .collect();
        let mut ordered = rows.clone();
        self.sort_candidates_by_specificity(&mut ordered);
        BareMultiPlan {
            stage: self.rank_candidates_unfiltered(args, rows),
            program: std::sync::OnceLock::new(),
            found_multi_candidates: true,
            keyed: true,
            captured_remaining: ordered.into_iter().map(|(_, def)| def).collect(),
        }
    }
}
