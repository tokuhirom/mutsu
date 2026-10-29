use super::multi_rank::{self, MultiRank, RankProfile, prune_to_maximal};
use super::multi_type_walk::WalkOwner;
use super::*;
use crate::runtime::types::{is_coercion_constraint, parse_coercion_type};
use crate::value::ValueView;

/// The distance reported for a constraint that is not an ancestor of the
/// argument's type at all (including types whose hierarchy mutsu does not
/// model).  Any real ancestor scores below this.
pub(super) const UNRELATED_DISTANCE: usize = 500;

/// A builtin type's classes and roles in narrowness order (ADR-0051 P2): the
/// index of a constraint in it is that constraint's distance from a value of
/// the type. Read from the builtin type catalog, so a type the catalog knows
/// ranks its roles as Rakudo does (`Real` before `Numeric` for an `Int`) and
/// carries no ancestor Rakudo denies (`Pair` is not `Cool`).
/// The ancestors of `Parameter` and `Signature`, which have no catalog row (a
/// row changes how their `ACCEPTS` and method dispatch resolve) but are plain
/// `Any` subclasses. Without them a `Mu`/`Any` candidate scored "unrelated"
/// for such a value and outranked a nominally exact candidate.
fn implicit_builtin_mro(type_name: &str) -> &'static [&'static str] {
    match type_name {
        "Parameter" => &["Parameter", "Any", "Mu"],
        "Signature" => &["Signature", "Any", "Mu"],
        _ => &[],
    }
}

fn builtin_type_mro(type_name: &str) -> &'static [&'static str] {
    crate::builtin_types::ancestry::builtin_type_narrowness_chain(type_name).unwrap_or(&[])
}

/// The type a coercion parameter accepts, i.e. the type it is as *wide* as.
/// `Str()` is short for `Str(Any)`, so it accepts anything.
pub(super) fn coercion_accepted_constraint(constraint: &str) -> Option<&str> {
    if !is_coercion_constraint(constraint) {
        return None;
    }
    match parse_coercion_type(constraint) {
        Some((_, Some(source))) => Some(source),
        Some((_, None)) => Some("Any"),
        None => None,
    }
}

impl Interpreter {
    fn ambiguous_multi_dispatch_error(
        &self,
        name: &str,
        args: &[Value],
        defs: &[Arc<FunctionDef>],
    ) -> RuntimeError {
        let arg_types = args
            .iter()
            .filter(|v| !v.is_string_pair_value())
            .map(super::value_type_name)
            .collect::<Vec<_>>()
            .join(", ");
        let mut err = RuntimeError::new(format!("Ambiguous call to '{}({})'", name, arg_types));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Ambiguous call to {}({}); these signatures all match: {}",
                name,
                arg_types,
                defs.iter()
                    .map(|def| {
                        crate::value::signature::make_signature_value(
                            crate::value::signature::param_defs_to_sig_info(
                                &def.param_defs,
                                def.return_type.clone(),
                            ),
                            Some(self),
                        )
                        .to_string_value()
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        );
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Multi::Ambiguous"),
            attrs,
        )));
        err
    }

    /// The [`MultiRank`] a multi candidate is ranked by for `args`: the
    /// nominal tier, the type-hierarchy distance, the refinement tier,
    /// slurpiness, whether it needs a named bind check, optional-positional
    /// count, declaration order. See [`MultiRank`] for why nominal narrowness
    /// outranks a refinement rather than the other way round.
    ///
    /// There is deliberately no required-named step: rakudo leaves
    /// `multi f(:$x!)` and `multi f(:$x)` (or `multi f(*%m where ...)`) tied
    /// and declaration order decides. The one case where a required named
    /// wins — `multi j(*%m)` vs `multi j(:$x!)` — is already decided by the
    /// bind-check step.
    // Cost: O(p) plus one hierarchy walk per typed parameter, p = parameters.
    pub(super) fn candidate_rank_key(
        &mut self,
        def: &Arc<FunctionDef>,
        args: &[Value],
    ) -> MultiRank {
        let (literal, typed, constrained, subsig, writable) =
            self.candidate_specificity_rank_for_args(def, args);
        let (distance, profile) = self.candidate_type_distance_profile(args, def);
        let params = Self::dispatch_visible_params(def);
        // A capture with a sub-signature (`multi f(|c($x))`) ranks by the
        // flattened sub-signature, but to rakudo it is still a slurpy capture
        // whose sub-signature is a bind check and which declares no
        // positional: `multi f($n)` is narrower
        // (`roast/S06-multi/subsignature.t`), `multi f(*@a)` is wider.
        let capture = def.param_defs.iter().any(|p| p.is_capture_subsignature());
        let arity_fits = || {
            let given = args
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                .count();
            let (count, required) = multi_rank::positional_arity(&params);
            capture
                || (given >= required
                    && (given <= count || multi_rank::has_slurpy_positional(&params)))
        };
        MultiRank {
            nominal: (literal, typed),
            distance,
            refinement: [constrained, subsig, writable, 0],
            constrained_slot: 0,
            non_slurpy: !capture && !multi_rank::has_slurpy_positional(&params),
            arity: if capture {
                (0, 0)
            } else {
                multi_rank::positional_arity(&params)
            },
            bind_check: capture || constrained > 0 || multi_rank::needs_named_bind_check(&params),
            // A refined candidate (`where`, `subset`) carries rakudo's bind-check
            // flag, so its omitted optional positional no longer ranks it wider
            // (`multi f(UInt $s = 1, :$a)` ties `multi f(:$a)` for `f()`).
            optional: constrained == 0 && multi_rank::has_optional_positional(&params),
            decl_order: def.decl_order,
            profile,
            invocant: None,
            tier: 0,
            tiered: false,
            tier_bind_check: false,
            arity_fits: arity_fits(),
        }
    }

    /// Whether the named type `sub` is `sup` or conforms to it: the
    /// directional relation [`MultiRank`] compares declared nominal types by.
    // Cost: O(h), h = the hierarchy walk of `type_hierarchy_distance`.
    pub(super) fn nominal_type_conforms(&self, sub: Symbol, sup: Symbol) -> bool {
        if sub == sup {
            return true;
        }
        // Inside a ranking scope the relation between two plain classes is a
        // memoised set lookup (see `type_conformance`); debug builds check it
        // against the general walk.
        if let Some(fast) = self.conformance_fast(sub, sup) {
            debug_assert_eq!(
                fast,
                self.nominal_type_conforms_general(sub, sup),
                "type_conformance disagrees with type_hierarchy_distance for {sub:?} / {sup:?}"
            );
            return fast;
        }
        self.nominal_type_conforms_general(sub, sup)
    }

    // Cost: O(h), h = the hierarchy walk of `type_hierarchy_distance`.
    fn nominal_type_conforms_general(&self, sub: Symbol, sup: Symbol) -> bool {
        if sub == sup {
            return true;
        }
        // `Any` and `Mu` are ancestors of every type, including the ones the
        // hierarchy walk below cannot place from a type object.
        if sup.with_str(|n| n == "Mu") || (sup.with_str(|n| n == "Any") && !sub.with_str(|n| n == "Mu")) {
            return true;
        }
        if sub.with_str(|n| matches!(n, "Any" | "Mu")) {
            return false;
        }
        if sub.with_str(|sub| {
            sup.with_str(|sup| crate::builtin_types::ancestry::builtin_role_ancestors(sub).contains(&sup))
        }) {
            return true;
        }
        sup.with_str(|sup| {
            self.type_hierarchy_distance(sup, &Value::package(sub)) < UNRELATED_DISTANCE
        })
    }

    /// [`prune_to_maximal`] over the candidates of one sub dispatch that
    /// bound, in narrowest-first scan order; a pure tie is returned unchanged
    /// so the ambiguity logic in [`Self::settle_ranked_matches`] still sees it.
    // Cost: O(m^2 * h), m = candidates that bound, h = one `nominal_type_conforms` walk.
    ///
    /// An arity-incomparable split that rakudo reports as ambiguous (see
    /// `multi_rank::prune_to_maximal`) is raised here as
    /// `X::Multi::Ambiguous`.
    pub(super) fn prune_incomparable_matches(
        &self,
        name: &str,
        args: &[Value],
        matches: Vec<(MultiRank, Arc<FunctionDef>)>,
    ) -> Result<Vec<Arc<FunctionDef>>, RuntimeError> {
        let (matches, ambiguous) = {
            let _scope = self.conformance_scope();
            prune_to_maximal(matches, &|x, y| self.nominal_type_conforms(x, y))
        };
        let defs: Vec<Arc<FunctionDef>> = matches.into_iter().map(|(_, d)| d).collect();
        if ambiguous {
            return Err(self.ambiguous_multi_dispatch_error(name, args, &defs));
        }
        Ok(defs)
    }

    pub(super) fn choose_best_matching_candidate(
        &mut self,
        name: &str,
        args: &[Value],
        candidates: Vec<(String, Arc<FunctionDef>)>,
    ) -> Resolved {
        self.choose_best_matching_candidate_excluding(name, args, candidates, None)
    }

    /// [`Self::choose_best_matching_candidate`] for a resolver that retries
    /// with successively wider candidate sets. `rejected` collects the
    /// fingerprints of candidates that were tried against these `args` and did
    /// not bind, and candidates already in it are skipped: the answer for the
    /// same arguments cannot change, and trying again re-ran their `where`
    /// clauses once per retry -- three times per call of a user operator that
    /// no candidate accepts (`resolve_function_with_types`' exact-arity,
    /// optional-arity and any-arity passes), where rakudo runs it once.
    /// A candidate whose `where` threw is not recorded, so its exception is
    /// still weighed by the later pass exactly as before.
    pub(super) fn choose_best_matching_candidate_excluding(
        &mut self,
        name: &str,
        args: &[Value],
        candidates: Vec<(String, Arc<FunctionDef>)>,
        rejected: Option<&mut std::collections::HashSet<u64>>,
    ) -> Resolved {
        // Rank every candidate BEFORE trying to bind any of them.
        //
        // [`Self::candidate_rank_key`] reads only the declared signature and
        // the argument *types* — it never runs user code, and in particular
        // never evaluates a `where` clause (it merely counts how many params
        // carry one). So the whole candidate list can be put in
        // narrowest-first order up front, and the bind attempts below can then
        // stop as soon as the remaining candidates are strictly wider than the
        // best match found so far: a wider candidate can neither win nor join
        // the tie set that decides `X::Multi::Ambiguous`, so testing it cannot
        // change the answer.
        //
        // Raku dispatches this way too — narrowest-first, stopping at the
        // first candidate that binds — and the difference matters because a
        // bind attempt is not cheap here: every `where` clause is a compiled
        // block run against a snapshot of the env. `Crane`'s protos are the
        // pathological case, with 17 (`in`) and 7 (`exists-key`) candidates
        // whose `where` clauses recurse back into `Crane`; running all of them
        // on every call is what made `Config::TOML` 12x slower than rakudo
        // ([#7858](https://github.com/tokuhirom/mutsu/issues/7858)).
        //
        // The sort is stable, so candidates the caller already ordered by
        // `sort_candidates_by_specificity` keep that order within a rank group
        // — which is what makes the final "first declared wins" tie-break come
        // out the same as it did when every candidate was tried.
        // One `multi` is registered under SEVERAL registry keys — the arity key
        // `Pkg::f/1`, the typed key `Pkg::f/1:Int`, the `__m<n>` multi suffixes
        // — and the gathers above collect by key, so the same
        // `Arc<FunctionDef>` arrives two or three times over. Drop the repeats
        // BEFORE ranking: identical fingerprints mean identical
        // `params`/`param_defs`/`body`, and the repeats are the very same
        // `Arc`, so they would each produce a byte-identical rank key --
        // ranking them was `candidate_rank_key` (and the whole signature walk
        // under it) run two or three times over per resolution for no effect
        // on the outcome (#8696).
        //
        // The incoming order is the caller's `sort_candidates_by_specificity`
        // order, and `retain` keeps the first of each fingerprint, so the
        // survivor is the same copy that won the caller's declaration-order
        // tie-break -- which is what the stable sort below then preserves
        // within a rank group. This is where the dedup used to sit, after the
        // sort; moving it earlier cannot change which copy survives, because
        // every copy of one fingerprint is the same `Arc` and therefore ties
        // on every component of the key, `decl_order` included.
        let mut candidates = candidates;
        // An operator candidate only another compunit imported is not in
        // scope here (#9944, `runtime/operator_scope.rs`).
        self.retain_visible_operator_candidates(name, &mut candidates);
        self.choose_best_in_scope_candidate(name, args, candidates, rejected)
    }

    /// [`Self::choose_best_matching_candidate_excluding`] over candidates the
    /// caller already holds in scope, so no visibility filter applies: the
    /// candidates a multi dispatcher value captured are that value's,
    /// wherever it is called from (an imported family's dispatcher called
    /// after its importing block exited, ADR-8751).
    // Cost: O(c log c) ranking plus the bind attempts, c = candidates.
    pub(super) fn choose_best_in_scope_candidate(
        &mut self,
        name: &str,
        args: &[Value],
        candidates: Vec<(String, Arc<FunctionDef>)>,
        rejected: Option<&mut std::collections::HashSet<u64>>,
    ) -> Resolved {
        let mut candidates = candidates;
        self.retain_candidates_not_outranked_by_core(name, args, &mut candidates);
        {
            let mut seen_keys = std::collections::HashSet::new();
            candidates.retain(|(_, def)| {
                let fp = def.body_fingerprint();
                !rejected.as_ref().is_some_and(|r| r.contains(&fp)) && seen_keys.insert(fp)
            });
        }

        // A `Proxy` argument (a `:=`-bound variable, a sigilless parameter, a
        // callable multi's call) ranks by what its FETCH answers, as the binder
        // type-checks it: `multi f(%h)` must beat `multi f(Mu)` for a Proxy over
        // a Hash. The ranking walk cannot run a FETCH, so the fetched values
        // stand in for the arguments there; binding still gets the originals.
        let fetched = self.args_with_proxies_fetched(args)?;
        let rank_args = fetched.as_deref().unwrap_or(args);
        let mut ranked: Vec<(MultiRank, Arc<FunctionDef>)> = Vec::with_capacity(candidates.len());
        for (_, def) in candidates {
            let key = self.candidate_rank_key(&def, rank_args);
            ranked.push((key, def));
        }
        {
            let _scope = self.conformance_scope();
            multi_rank::assign_tiers(&mut ranked, |(key, _)| key, &|x, y| {
                self.nominal_type_conforms(x, y)
            });
        }
        ranked.sort_by(|a, b| a.0.total_cmp(&b.0));
        self.bind_ranked_candidates(name, args, fetched.as_deref(), ranked, rejected)
    }

    /// `args` with every `Proxy` argument (also under a `VarRef`) replaced by
    /// its FETCHed value, or `None` when there is no `Proxy` among them.
    // Cost: O(a) plus one FETCH body per Proxy, a = arguments.
    fn args_with_proxies_fetched(
        &mut self,
        args: &[Value],
    ) -> Result<Option<Vec<Value>>, RuntimeError> {
        if !args.iter().any(|v| v.unwrap_varref().is_proxy_value()) {
            return Ok(None);
        }
        let mut fetched = Vec::with_capacity(args.len());
        for v in args {
            if v.unwrap_varref().is_proxy_value() {
                fetched.push(self.auto_fetch_proxy(v.unwrap_varref())?);
            } else {
                fetched.push(v.clone());
            }
        }
        Ok(Some(fetched))
    }

    /// The bind half of [`Self::choose_best_matching_candidate_excluding`]:
    /// walk `ranked` (narrowest first, already filtered) and pick the winner,
    /// raising `X::Multi::Ambiguous` or a parked `where` exception exactly as
    /// the full resolver does.
    pub(super) fn bind_ranked_candidates(
        &mut self,
        name: &str,
        args: &[Value],
        fetched: Option<&[Value]>,
        ranked: Vec<(MultiRank, Arc<FunctionDef>)>,
        mut rejected: Option<&mut std::collections::HashSet<u64>>,
    ) -> Resolved {
        // The duplicate-registry-key dedup that used to sit here now runs
        // before the ranking loop above -- see the comment there. It stops one
        // candidate's `where` clause from being RUN once per key it happens to
        // be registered under (`multi f(Int:D $x where {...})` evaluated its
        // constraint 3x per resolution before the dedup existed), and now also
        // stops it being RANKED that many times.
        let mut matches: Vec<(MultiRank, Arc<FunctionDef>)> = Vec::new();
        // The rank of `matches[0]`, i.e. of the narrowest candidate that
        // actually bound. `None` until the first match.
        let mut best_key: Option<MultiRank> = None;
        // The first candidate whose `where` constraint THREW, together with
        // the exception. Raku walks the candidates narrowest-first and stops at
        // the first that binds, so such an exception escapes only when that
        // candidate is one raku would actually have reached — i.e. when it is
        // not out-ranked by a candidate that did match. Deciding that needs the
        // winner, so the exception is parked here until the ranking below.
        let mut threw: Option<(Arc<FunctionDef>, RuntimeError)> = None;
        // A `where` exception recorded OUTSIDE this dispatch (by a method match
        // earlier in the same instruction, say) must not be mistaken for one of
        // these candidates': park it and put it back below.
        let outer_where_exception = self.pending_where_exception.take();
        for (key, def) in ranked {
            // Strictly wider than the narrowest candidate that already bound:
            // every candidate from here on is too, since the list is sorted.
            // Not for one that is incomparable with it (#11943): that one may
            // still win by declaration order, so it is tried and the scan goes on.
            if let Some(best) = best_key
                && key.scan_may_skip(&best, &|x, y| self.nominal_type_conforms(x, y))
            {
                continue;
            }
            // For auto-param subs ($^a, $^b) with empty param_defs but
            // non-empty params, check arity against params.len() since
            // args_match_param_types cannot handle this case.
            let type_ok = if def.param_defs.is_empty()
                && !def.params.is_empty()
                && def.params.iter().all(|p| p.starts_with('^'))
            {
                let positional_arg_count = args
                    .iter()
                    .filter(|arg| {
                        !matches!(arg.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
                    })
                    .count();
                positional_arg_count == def.params.len()
            } else {
                // A candidate that binds the caller's container (`is rw`,
                // `is raw`, `\x`, `|c`) is matched on the Proxy itself; any
                // other sees what the Proxy's FETCH answered, as the binder
                // type-checks it (`multi f(%h)` for a Proxy over a Hash).
                let match_args = match fetched {
                    Some(fetched) if !def.param_defs.iter().any(|p| p.binds_caller_container()) => {
                        fetched
                    }
                    _ => args,
                };
                self.args_match_multi_candidate_in_scope(match_args, &def)
            };
            if let Some(e) = self.take_where_exception() {
                if threw.is_none() {
                    threw = Some((def.clone(), e));
                }
                // A candidate whose `where` died did not match; whether the
                // exception escapes is settled after the winner is known.
                continue;
            }
            if !type_ok && let Some(rejected) = rejected.as_deref_mut() {
                rejected.insert(def.body_fingerprint());
            }
            if type_ok {
                // No fingerprint filter here: `ranked` is already unique by
                // fingerprint (see the `retain` above), so every match is a
                // distinct declaration.
                best_key.get_or_insert(key);
                matches.push((key, def));
            }
        }
        let tier_bind_check = matches.first().is_some_and(|(rank, _)| rank.tier_bind_check);
        let matches = match self.prune_incomparable_matches(name, args, matches) {
            Ok(matches) => matches,
            Err(err) => {
                self.pending_where_exception = outer_where_exception;
                return Err(err);
            }
        };
        self.settle_ranked_matches(
            name,
            args,
            matches,
            threw,
            outer_where_exception,
            tier_bind_check,
        )
    }

    /// The second half of [`Self::bind_ranked_candidates`]: given the
    /// candidates that matched at the best rank (narrowest first) and the
    /// first `where` exception a tried candidate threw, pick the winner —
    /// raising the parked exception when it was reached, and
    /// `X::Multi::Ambiguous` for a genuine tie. Shared with the per-type
    /// dispatch program (`multi_dispatch_program.rs`), which finds the
    /// matches its own way.
    pub(super) fn settle_ranked_matches(
        &mut self,
        name: &str,
        args: &[Value],
        mut matches: Vec<Arc<FunctionDef>>,
        threw: Option<(Arc<FunctionDef>, RuntimeError)>,
        outer_where_exception: Option<Box<RuntimeError>>,
        tier_bind_check: bool,
    ) -> Resolved {
        if let Some((thrower, e)) = threw {
            // No candidate matched at all, or the thrower sorts at least as
            // narrow as (or was declared before) the best match: raku would
            // have reached the `where` and died there, so re-raise.
            let reached = match matches.first() {
                None => true,
                Some(best) => {
                    // Compare narrowness with the `where`-constraint component
                    // ZEROED on both sides. Rakudo orders candidates by their
                    // NOMINAL types first and only lets a `where` break a tie
                    // between equally-nominal candidates, so a nominally
                    // narrower candidate that matched (`multi bar(| (A $x))`)
                    // is reached before a `where`-only one
                    // (`multi bar(| where { ... })`) and that `where` never
                    // runs. The rank key now models that tier split itself, so
                    // this mostly duplicates it; it still matters for a
                    // thrower that is nominally TIED with the best match, where
                    // rakudo runs neither `where` ahead of the plain candidate
                    // it cannot out-narrow.
                    let mut a = self.candidate_rank_key(&thrower, args);
                    let mut b = self.candidate_rank_key(best, args);
                    a.refinement[0] = 0;
                    b.refinement[0] = 0;
                    a.total_cmp(&b) != std::cmp::Ordering::Greater
                }
            };
            if reached {
                // Raised the way an ambiguous dispatch is: as the resolution's
                // error, which a caller that dispatches the call re-raises.
                self.pending_where_exception = outer_where_exception;
                return Err(e);
            }
        }
        self.pending_where_exception = outer_where_exception;
        if matches.len() <= 1 {
            return Ok(matches.into_iter().next());
        }

        // `matches` is ALREADY in narrowest-first order — the scan walked the
        // pre-ranked list and broke at the first candidate strictly wider than
        // `matches[0]`. So `matches[0]` is the winner, and every entry ties
        // with it on all four narrowness components (specificity rank, type
        // distance, named bind check, optional-positional count) by
        // construction: the loop's break condition is exactly "that
        // four-component key compares Greater". The separate re-sort and the
        // `tied` re-filter this used to do — both of which recomputed
        // `candidate_specificity_rank_for_args` and `candidate_type_distance`
        // per match — are therefore redundant.
        let positional_args = args
            .iter()
            .filter(|arg| !matches!(arg.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            .count();
        let best_shape = self.candidate_dispatch_shape(&matches[0], positional_args);
        let tied: Vec<Arc<FunctionDef>> = matches.clone();
        // Compare dispatch shapes as sorted multisets so that candidates with
        // the same set of typed parameters in a different order are still
        // detected as tied (e.g. `foo(S $a, T $b)` vs `foo(T $a, S $b)`).
        let mut best_shape_sorted = best_shape;
        best_shape_sorted.sort();
        if tied.len() > 1
            && !tied.iter().any(Self::candidate_has_typed_named_param)
            && tied
                .iter()
                .all(|def| !self.candidate_uses_order_sensitive_dispatch(def))
            && tied.iter().all(|def| {
                let mut shape = self.candidate_dispatch_shape(def, positional_args);
                shape.sort();
                shape == best_shape_sorted
                    || self.shapes_tie_on_incomparable_types(&shape, &best_shape_sorted)
            })
        {
            // `is default` trait tie-breaking: if exactly one tied candidate
            // has `is_default`, prefer it over the others.
            let default_candidates: Vec<&Arc<FunctionDef>> =
                tied.iter().filter(|def| def.is_default).collect();
            if default_candidates.len() == 1 {
                return Ok(Some(default_candidates[0].clone()));
            }
            // Candidates that tie while needing a named bind check (an
            // explicit named parameter, or a `where` on a slurpy) are never
            // ambiguous: rakudo routes them through its trial-bind path, where
            // the first one declared that binds wins.
            //
            // ```raku
            // multi sub f(Int $x, :$bar!) {"first"}; multi sub f(Int $x, :$bar!) {"second"};
            // f(1, :bar);      # "first", not ambiguous
            // multi sub h(:$a) {"a"};                multi sub h(:$b) {"b"};
            // h();             # "a"
            // multi sub g($x) {"first"};             multi sub g($x) {"second"};
            // g(1);            # X::Multi::Ambiguous
            // ```
            //
            // Two purely *positional* ties stay ambiguous —
            // `roast/integration/advent2011-day24.t` pins two verbatim
            // duplicate `multi sub Slurp($filename) {...}` with no named param.
            //
            // A refined candidate (`where`, `subset`) is trial-bound the same
            // way: `multi g(UInt $x)` twice is not ambiguous either.
            //
            // The same holds when any candidate of the winner's tier, bound or
            // not, carries a literal or a `where`: rakudo trial-binds that
            // tier in declaration order (`multi r($a, Additive, 0)` next to
            // `multi r(Numeric:D, $, Numeric:D)` and `multi r(Numeric, $,
            // Numeric)` resolves `r(2, '+', 3)` to the first, #13280).
            if tier_bind_check
                || multi_rank::needs_named_bind_check(&Self::dispatch_visible_params(&matches[0]))
                || self.candidate_rank_key(&matches[0], args).refinement[0] > 0
            {
                return Ok(Some(matches.remove(0)));
            }
            return Err(self.ambiguous_multi_dispatch_error(name, args, &tied));
        }

        Ok(Some(matches.remove(0)))
    }

    /// Whether any *named* parameter of `def` carries a type constraint.
    ///
    /// Narrowness in Raku is computed from the POSITIONAL parameters only —
    /// named parameters decide whether a candidate is *applicable*, never how
    /// narrow it is. So two candidates that differ only in the types of their
    /// named parameters land in the same narrowness group, and rakudo does not
    /// call that ambiguous: a candidate with type-constrained nameds needs a
    /// trial bind, and the FIRST one that binds wins.
    ///
    /// ```text
    /// multi f(Str :$a,     :$b) { "A" }
    /// multi f(    :$a, Str :$b) { "B" }
    /// f(a => "x", b => "y")   # "A" -- declaration order, not ambiguous
    /// ```
    ///
    /// (Both are applicable; A is simply declared first. Reversing the two
    /// declarations makes the call return "B".) The positional analogue
    /// `multi g(Str $a, $b)` / `multi g($a, Str $b)` really is ambiguous, in
    /// rakudo and here alike, which is why this only suppresses the report when
    /// a named type constraint is what tied the candidates.
    fn candidate_has_typed_named_param(def: &Arc<FunctionDef>) -> bool {
        Self::dispatch_visible_params(def)
            .into_iter()
            .any(|p| p.named && p.type_constraint.is_some())
    }

    /// The narrowness tuple of `def` with no call in flight, in
    /// `(literal, typed, constrained, subsig, writable)` order — the first
    /// two are the nominal tier, the rest the refinement tier.
    pub(super) fn candidate_specificity_rank(
        &self,
        def: &FunctionDef,
    ) -> (usize, usize, usize, usize, usize) {
        self.candidate_specificity_rank_for_args(def, &[])
    }

    /// Like [`Self::candidate_specificity_rank`], but ranks coercion parameters
    /// against the arguments that would bind to them — see
    /// [`Self::effective_dispatch_constraint`].  Pass an empty `args` slice when no
    /// call is in flight (ordering a `callsame` chain, say); coercion params
    /// then rank by the type they accept.
    /// Whether a type-constraint base name is a `subset` — a user-declared one
    /// in the registry, or one of the *core* subsets the setting declares.
    ///
    /// `UInt` is `subset UInt of Int where * >= 0` in rakudo, so
    /// `multi f(UInt $x)` is narrower than `multi f(Int $x)` for `f(10)`.
    /// mutsu implements `UInt` as a type-matching special case rather than a
    /// registry entry, so without this it ranked as a plain nominal type and
    /// lost every tie against `Int` (`t/inline-module-check-import.t`'s
    /// `multi infix:<+>(UInt $a, UInt $b)`).
    pub(crate) fn constraint_is_subset(&self, base: &str) -> bool {
        base == "UInt" || self.registry().subsets.contains_key(base)
    }

    /// The type a constraint measures its type-hierarchy distance against.
    ///
    /// A `subset` is a *refinement* of another type, not a type of its own:
    /// rakudo gives `subset Even of Int` the nominal type `Int` plus a
    /// constraint, so `multi f(Even $x)` ties `multi f(Int $x)` on nominal
    /// narrowness and wins on the refinement tier. Measuring `Even` itself
    /// finds it nowhere in the hierarchy and scores the 500 "unrelated"
    /// distance, which — now that distance outranks the refinement tier —
    /// would make a subset lose every tie it exists to win. Resolving it here
    /// keeps that comparison honest without teaching the hierarchy walk about
    /// subsets.
    ///
    /// A subset of a subset (`subset Top of Mid of Int`) resolves through the
    /// chain; the iteration bound keeps a cyclic declaration from spinning.
    pub(super) fn dispatch_nominal_base<'a>(
        &self,
        base: &'a str,
        arg: Option<&Value>,
    ) -> std::borrow::Cow<'a, str> {
        use std::borrow::Cow;
        let resolved = self.dispatch_subset_chain_base(base);
        // A subset of a coercion type (`subset CC of Str() where ...`) is as
        // narrow as that coercion parameter would be (see
        // [`Self::effective_dispatch_constraint`]); left as `Str()` the
        // hierarchy walk finds it nowhere and ranks it unrelated.
        if is_coercion_constraint(&resolved) {
            return Cow::Owned(self.effective_dispatch_constraint(&resolved, arg).to_string());
        }
        resolved
    }

    fn dispatch_subset_chain_base<'a>(&self, base: &'a str) -> std::borrow::Cow<'a, str> {
        use std::borrow::Cow;
        // `UInt` is `subset UInt of Int where * >= 0` in rakudo, but mutsu
        // implements it as a type-matching special case rather than a registry
        // entry (see `constraint_is_subset`).
        if base == "UInt" {
            return Cow::Borrowed("Int");
        }
        let Some(mut current) = self.registry().subsets.get(base).map(|d| d.base.clone()) else {
            return Cow::Borrowed(base);
        };
        for _ in 0..16 {
            if current == "UInt" {
                return Cow::Owned("Int".to_string());
            }
            let Some(next) = self
                .registry()
                .subsets
                .get(&current)
                .map(|d| d.base.clone())
            else {
                break;
            };
            if next.is_empty() || next == current {
                break;
            }
            current = next;
        }
        if current.is_empty() {
            return Cow::Borrowed(base);
        }
        Cow::Owned(current)
    }

    /// [`Self::params_specificity_rank`] of a multi sub candidate's
    /// dispatch-visible parameters.
    // Cost: O(p) plus one hierarchy walk per coercion parameter, p = parameters.
    pub(super) fn candidate_specificity_rank_for_args(
        &self,
        def: &FunctionDef,
        args: &[Value],
    ) -> (usize, usize, usize, usize, usize) {
        self.params_specificity_rank(&Self::dispatch_visible_params(def), args)
    }

    /// The constraint a parameter effectively dispatches on when `arg` binds
    /// to it.
    ///
    /// Raku compiles a coercion type `T(F)` into *two* candidates — see
    /// `roast/S06-multi/type-based.t`, "Coercion types introduce two
    /// candidates": one taking `T` directly, and one taking `F` (`Any` when
    /// omitted) and coercing.  Collapsed back into the single candidate mutsu
    /// keeps, that means the parameter is as narrow as `T` when the argument
    /// already IS a `T`, and only as wide-open as `F` otherwise.  So `Int() $`
    /// beats `Cool $` for `42` but loses to it for `"42"`, and `Str() $` loses
    /// to `Blob:D $` for `"x".encode`.
    pub(super) fn effective_dispatch_constraint<'a>(
        &self,
        constraint: &'a str,
        arg: Option<&Value>,
    ) -> &'a str {
        let Some(accepted) = coercion_accepted_constraint(constraint) else {
            return constraint;
        };
        let Some(open) = constraint.find('(') else {
            return constraint;
        };
        let target = &constraint[..open];
        match arg {
            Some(value) if self.type_hierarchy_distance(target, value) < UNRELATED_DISTANCE => {
                target
            }
            _ => accepted,
        }
    }

    /// Pair each dispatch-visible parameter with the argument that would bind
    /// to it (`None` when the call supplies none), so ranking can ask type
    /// questions about the actual argument.
    pub(super) fn dispatch_param_arguments(&self, params: &[&ParamDef], args: &[Value]) -> Vec<Option<Value>> {
        let mut out = Vec::with_capacity(params.len());
        let mut pos_idx = 0usize;
        for pd in params {
            if pd.named {
                let bare_name = pd.name.trim_start_matches(|c: char| "$@%&".contains(c));
                out.push(args.iter().find_map(|a| match a.view() {
                    ValueView::Pair(key, val) if key == bare_name => Some(val.clone()),
                    _ => None,
                }));
                continue;
            }
            while pos_idx < args.len() && matches!(args[pos_idx].view(), ValueView::Pair(..)) {
                pos_idx += 1;
            }
            if pos_idx < args.len() {
                out.push(Some(self.unwrap_varref_for_dispatch(&args[pos_idx]).0));
                pos_idx += 1;
            } else {
                out.push(None);
            }
        }
        out
    }

    /// Compute the total type hierarchy distance between a candidate's type
    /// constraints and the actual argument types.  Lower means more specific.
    /// Each parameter contributes the number of MRO levels between the
    /// constraint and the actual type; unconstrained parameters contribute a
    /// large constant so that constrained candidates are always preferred.
    pub(crate) fn candidate_type_distance(&self, args: &[Value], def: &FunctionDef) -> usize {
        self.candidate_type_distance_profile(args, def).0
    }

    /// [`Self::candidate_type_distance`] together with the per-parameter
    /// [`RankProfile`] the same walk produces (see
    /// [`Self::params_type_distance_profile`], shared with multi methods).
    // Cost: O(p) plus one hierarchy walk per positional parameter, p = parameters.
    pub(crate) fn candidate_type_distance_profile(
        &self,
        args: &[Value],
        def: &FunctionDef,
    ) -> (usize, RankProfile) {
        let owner = WalkOwner {
            package: def.package,
        };
        self.params_type_distance_profile(args, &Self::dispatch_visible_params(def), owner)
    }

    /// Unwrap a [`Value::varref`] wrapper to get the inner value and the source
    /// variable's declared type constraint (if any) for dispatch.
    pub(super) fn unwrap_varref_for_dispatch(&self, value: &Value) -> (Value, Option<String>) {
        match value.as_varref() {
            Some((name, inner, _)) => {
                // What the `WrapVarRef` site resolved in the variable's own
                // frame, or the env record for a `VarRef` built without it.
                let var_type = self.varref_type_constraint(value, name);
                (inner.clone(), var_type)
            }
            None => (value.clone(), None),
        }
    }

    /// Return how many MRO levels separate `constraint` from the actual type
    /// of `value`.  0 means exact match; larger means less specific.
    /// The builtin type catalog's spelling of a sized Buf/Blob type name.
    /// Raku source spells these `buf8`/`blob8`; `.^name` and the catalog
    /// render them parameterized (`Buf[uint8]`). Anything else is returned
    /// unchanged.
    fn catalog_buf_type_name(name: &str) -> &str {
        match name {
            "buf8" => "Buf[uint8]",
            "buf16" => "Buf[uint16]",
            "buf32" => "Buf[uint32]",
            "buf64" => "Buf[uint64]",
            "blob8" => "Blob[uint8]",
            "blob16" => "Blob[uint16]",
            "blob32" => "Blob[uint32]",
            "blob64" => "Blob[uint64]",
            other => other,
        }
    }

    pub(crate) fn type_hierarchy_distance(&self, constraint: &str, value: &Value) -> usize {
        let value_type = super::value_type_name(value);
        // Strip :D/:U smiley
        let base = if constraint.ends_with(":D") || constraint.ends_with(":U") {
            &constraint[..constraint.len() - 2]
        } else {
            constraint
        };
        // A RakuAST node or type object ranks by rakudo's measured hierarchy
        // (`src/rakuast/hierarchy.tsv`), answered without allocating: this
        // runs for every candidate of every call into a multi dispatching
        // on node types (rakudo's own RakuAST::Deparse has 267).
        if base.starts_with("RakuAST::") || base == "Any" || base == "Mu" {
            let distance = match value.view() {
                ValueView::RakuAst(node) => {
                    crate::rakuast::type_object_distance(node.class.printed_name(), base)
                }
                ValueView::Package(name) => name.with_str(|n| {
                    n.starts_with("RakuAST::")
                        .then(|| crate::rakuast::type_object_distance(n, base))
                        .flatten()
                }),
                _ => None,
            };
            if let Some(distance) = distance {
                return distance.unwrap_or(UNRELATED_DISTANCE);
            }
        }
        // `base` may be an env-bound alias for a fully-qualified type name —
        // an imported short name (`use Mod; ...` importing `Mod::Foo` and
        // binding it lexically as bare `Foo`) or a `::T` type capture.
        // Follow it the same way `type_matches_value` already does for
        // *applicability* — otherwise this distance (used only for ranking
        // candidates that already matched) compares the unresolved alias
        // against the value's real class name, never finds it in the MRO,
        // and falls through to the 500 "unrelated" distance: an `Any`
        // candidate then wrongly out-ranks the more specific one (issue
        // #8566, cross-module case).
        let resolved_base;
        let base = if !crate::runtime::utils::is_known_type_constraint(base)
            && let Some(bound_val) = self.type_name_binding(base)
            && let ValueView::Package(bound) = bound_val.view()
            && bound.with_str(|b| b != base)
        {
            resolved_base = bound.resolve();
            resolved_base.as_str()
        } else {
            base
        };
        // A `constant` bound to a mixin type object (`constant StrType = Str
        // but Type`) names the composed type `Str+{Type}` itself, which the
        // matching value is an instance of: distance 0, narrower than its
        // base `Str`. Only a candidate that already matched is ranked.
        if !crate::runtime::utils::is_known_type_constraint(base)
            && let Some(bound_val) = self.type_name_binding(base)
            && let ValueView::Mixin(inner, _) = bound_val.view()
            && matches!(inner.view(), ValueView::Package(_))
        {
            return 0;
        }
        if base == "Inf" {
            return match value.view() {
                ValueView::Num(n) if n.is_infinite() && n.is_sign_positive() => 0,
                _ => UNRELATED_DISTANCE,
            };
        }
        if base == "NaN" {
            return match value.view() {
                ValueView::Num(n) if n.is_nan() => 0,
                _ => UNRELATED_DISTANCE,
            };
        }
        // A `constant` bound to a value, used where a type goes
        // (`constant G = Point.new(...); multi f(Int $n, G)`), is a definite
        // value constraint, exactly like a literal parameter: rakudo compiles
        // it to the value's type plus a smartmatch against it, so it is as
        // narrow as the argument that bound to it. Only a candidate that
        // already matched is ranked, so equality here is that match.
        if !crate::runtime::utils::is_known_type_constraint(base)
            && let Some(bound_val) = self.type_name_binding(base)
            && !matches!(bound_val.view(), ValueView::Package(_))
            && crate::runtime::values_identical(&bound_val, value)
        {
            return 0;
        }
        // An enum value narrows in three steps: the value's own name
        // (`multi f(e1)`, a definite-value constraint), then its enum type
        // (`multi f(A $x)`), then the base type its values carry
        // (`multi f(Int $x)`). Measured against rakudo:
        // `enum A <e1 e2>; multi f(e1) {...}` beats `multi f(A $x)` beats
        // `multi f(Int $x)` for `f(e1)`. `value_type_name` answers the base
        // type (`Int`), so without this arm both narrower spellings scored the
        // 500 "unrelated" distance and the base-typed candidate won every time.
        if let ValueView::Enum {
            enum_type,
            key,
            value,
            ..
        } = value.view()
        {
            if key.with_str(|k| k == base) {
                return 0;
            }
            if enum_type.with_str(|n| n == base) {
                return 1;
            }
            let ancestors: &[&str] = match value {
                crate::value::EnumValue::Str(_) => builtin_type_mro("Str"),
                crate::value::EnumValue::Int(_) => builtin_type_mro("Int"),
                crate::value::EnumValue::Generic(_) => &["Any", "Mu"],
            };
            for (i, &ancestor) in ancestors.iter().enumerate() {
                if ancestor == base {
                    return i + 2;
                }
            }
            return UNRELATED_DISTANCE;
        }
        // `value_type_name` answers the generic "Any" for every `Instance`
        // (it does not know the concrete class) — so this shortcut must NOT
        // fire for an Instance constrained by `base == "Any"`, or a class
        // completely unrelated to `Foo` would tie with `Foo` itself at
        // distance 0 for a `multi method f(Any $x)` / `multi method f(Foo
        // $x)` pair (issue #8566). The dedicated Instance/MRO walk below
        // computes the real distance, `Any` included, via the class's own
        // `mro` chain.
        // A value carrying mixed-in roles (`'x' but Markup`, or the coercion
        // into a role that inherits a built-in type) sits one step BELOW its
        // inner type, exactly as rakudo's synthesized `Str+{Markup}` class
        // does: a constraint naming one of the composed roles is the narrowest
        // match there is, and every ancestor of the inner type is one step
        // further away than it would be for a bare inner value.
        //
        // Without this the inner type answered distance 0 and the role the 500
        // "unrelated" distance, so a wider candidate won the tie-break:
        // `Air::Functional`'s `multi render-tag(Str() $)` beat `multi
        // render-tag(Markup $)` for its own `Markup` values and HTML-escaped
        // the markup it exists to pass through. (An `Instance` needs none of
        // this -- `types::role_mixin_class` reblesses it into a real
        // `C+{R}` class whose MRO the walk below already follows.)
        if let ValueView::Mixin(inner, mixins) = value.view() {
            if mixins.contains_key(crate::meta_ns::MetaNs::Role.str_key_for_str(base)) {
                return 0;
            }
            // A role the mixed-in one itself composes is still nearer than the
            // inner type's own ancestors.
            if self.has_role(base)
                && mixins.keys().any(|key| {
                    key.as_str()
                        .strip_prefix(crate::meta_ns::MetaNs::Role.prefix())
                        .is_some_and(|role| self.role_is_descendant_of(role, base))
                })
            {
                return 1;
            }
            return self
                .type_hierarchy_distance(constraint, inner.as_ref())
                .saturating_add(1)
                .min(UNRELATED_DISTANCE);
        }
        if base == value_type && !matches!(value.view(), ValueView::Instance { .. }) {
            return 0;
        }
        // For instances, use the class MRO
        if let ValueView::Instance { class_name, .. } = value.view() {
            let cn = class_name.resolve();
            // `buf8` and `Buf[uint8]` are one type spelled two ways.
            let catalog_base = Self::catalog_buf_type_name(base);
            if base == cn.as_str() || catalog_base == cn.as_str() {
                return 0;
            }
            if let Some(distance) = self.mixin_type_role_distance(cn.as_str(), base) {
                return distance;
            }
            // A builtin type modelled as an instance (`Instant`, `Duration`,
            // `IO::Path`, the Buf/Blob family, ...) ranks its roles too: `now`
            // is narrower as a `Real` than as a `Numeric`, and a `"x".encode`
            // (`utf8`) is narrower as a `blob8` (`Blob[uint8]`) than as a
            // `Blob`, which the class-only MRO below cannot see.
            if let Some(i) = builtin_type_mro(cn.as_str())
                .iter()
                .position(|&ancestor| ancestor == catalog_base)
            {
                return i;
            }
            // A role the class itself composes is as near as a direct parent:
            // rakudo ranks a role and a parent class by their declared types,
            // which are unrelated, so the class MRO (which lists composed
            // roles after `Mu`) must not push the role behind a parent
            // candidate that is no narrower on its declared type.
            if self.has_role(base)
                && let Some(roles) = self.registry().class_composed_roles.get(cn.as_str())
                && roles.iter().any(|r| r == base)
            {
                return self.composed_role_distance(roles, base);
            }
            // `mro_readonly` falls back to a live parents-only walk when the
            // registry's cached `ClassDef::mro` is still empty (this method
            // takes `&self`, so it cannot compute-and-cache like
            // `Registry::class_mro` does) — see the `base == "Any"` case
            // above, which now always reaches this walk instead of the
            // universal-Instance shortcut.
            for (i, ancestor) in self.mro_readonly(cn.as_str()).iter().enumerate() {
                if ancestor == base {
                    return i;
                }
            }
            if let Some(i) = implicit_builtin_mro(cn.as_str())
                .iter()
                .position(|&ancestor| ancestor == catalog_base)
            {
                return i;
            }
            // Check composed roles
            if let Some(roles) = self.registry().class_composed_roles.get(cn.as_str())
                && roles.iter().any(|r| r == base)
            {
                return self.composed_role_distance(roles, base);
            }
        }
        if let ValueView::Package(name) = value.view() {
            let cn = name.resolve();
            if base == cn.as_str() {
                return 0;
            }
            // The type object of an `Int`-based enum is one step below `Int`
            // (`enum E <a b>`: `E.^mro` is `(E) (Int) (Cool) (Any) (Mu)`), so
            // it out-ranks `Int:U` and the ancestors follow at +1 (#13243).
            if self.is_int_enum_type_name(cn.as_str()) {
                // The candidate may spell the same enum by its qualified or
                // its short name (`module Foo { enum Format ...; multi
                // mt(Format) }`): distance 0 either way.
                if self.resolve_enum_type_key(base).is_some()
                    && self.resolve_enum_type_key(base) == self.resolve_enum_type_key(cn.as_str())
                {
                    return 0;
                }
                let enum_mro = builtin_type_mro("Int");
                if let Some(i) = enum_mro.iter().position(|&a| a == base) {
                    return i + 1;
                }
            }
            // A parametric type object (`Positional[Dog]`) ranks by its base
            // name: distance 0 to a `Positional` constraint (JSON::Unmarshal's
            // `_unmarshal($json, @x)` receiving an attribute type).
            let cn_base = cn.split('[').next().unwrap_or(cn.as_str());
            if base == cn_base {
                return 0;
            }
            if let Some(distance) = self.role_type_object_distance(cn_base, base) {
                return distance;
            }
            for lookup in [cn.as_str(), cn_base] {
                // `mro_readonly` walks the parent chain live — the registry's
                // `ClassDef.mro` field is not populated for every class, which
                // made `d(C1, Package(C2))` fall through to the 500 fallback
                // (S14 parameter-subtyping: `(C1 @arr)` lost to a bare
                // `(@arr)` once the bare sigil got a real implicit distance).
                for (i, ancestor) in self.mro_readonly(lookup).iter().enumerate() {
                    if ancestor == base {
                        return i;
                    }
                }
                if let Some(roles) = self.registry().class_composed_roles.get(lookup)
                    && roles.iter().any(|r| r == base)
                {
                    return self.composed_role_distance(roles, base);
                }
            }
        }
        // Built-in type hierarchy (approximation of Raku MRO depths)
        // Bool -> Int -> Cool -> Any -> Mu
        // but also Bool -> Int -> Numeric/Real -> ...
        // A type object is represented as a Package, whose generic value type
        // is `Package`; method dispatch still needs the named type's built-in
        // MRO. For example, Str is both Cool and Mu, and a `multi method` on
        // an Attribute must prefer its Cool candidate over Mu when passed the
        // Str type object (Audio::Hydrogen's XML::Class does this).
        let builtin_mro: &[&str] = match value.view() {
            // Type objects are represented as Package, whose generic value
            // type is `Package`; use the named type's built-in MRO instead.
            ValueView::Package(name) => name.with_str(|type_name| {
                let own = builtin_type_mro(type_name);
                if own.is_empty() {
                    builtin_type_mro(type_name.split('[').next().unwrap_or("Package"))
                } else {
                    own
                }
            }),
            _ => builtin_type_mro(value_type),
        };
        let catalog_base = Self::catalog_buf_type_name(base);
        for (i, &ancestor) in builtin_mro.iter().enumerate() {
            if ancestor == catalog_base {
                return i;
            }
        }
        // Not found in hierarchy; return a large distance
        UNRELATED_DISTANCE
    }

    /// Distance of a class to a role in its composed-role list: 1 when the class
    /// composes `base` directly, 2 when another composed role itself composes it
    /// (`class D does R3`, `role R3 does R2` -- `R3` is the narrower constraint
    /// for a `D`).
    // Cost: O(r * p), r = composed roles, p = role-parent edges walked per check.
    /// The distance of the role `base` from an instance of the mixin type
    /// `class` (`C+{R}`, stacked `C+{A}+{B}`) that `$obj does R` reblessed it
    /// into: the roles a mixin level composes sit one step below it, beside
    /// its base type, as for a `Mixin` value (see `type_hierarchy_distance`).
    /// The class MRO lists them after `Mu`, which ranked `multi f(R $, $)`
    /// behind `multi f(C $, Mu $)` for such an object. `None` when `class`
    /// is no mixin type or composes no such role.
    // Cost: O(l * r), l = stacked mixin levels, r = roles per level.
    fn mixin_type_role_distance(&self, class: &str, base: &str) -> Option<usize> {
        let registry = self.registry();
        let mut level = class;
        let mut depth = 0;
        while self.types.published_mixins.may_hold(level) {
            let roles = registry.class_composed_roles.get(level)?;
            if roles.iter().any(|r| r == base) {
                return Some(depth + 1);
            }
            if self.has_role(base) && roles.iter().any(|r| self.role_is_descendant_of(r, base)) {
                return Some(depth + 2);
            }
            level = registry.classes.get(level)?.parents.first()?;
            depth += 1;
        }
        None
    }

    fn composed_role_distance(&self, roles: &[String], base: &str) -> usize {
        if roles
            .iter()
            .any(|r| r != base && self.role_is_descendant_of(r, base))
        {
            2
        } else {
            1
        }
    }

    /// Like `type_hierarchy_distance`, but also considers the source variable's
    /// declared type constraint for native type dispatch.
    /// When a variable declared as `int` is passed to a multi candidate expecting
    /// `int`, that should be distance 0 (exact native match). The boxed `Int`
    /// candidate should be distance 1 (autoboxing penalty).
    pub(super) fn type_hierarchy_distance_with_var_type(
        &self,
        constraint: &str,
        value: &Value,
        var_type: Option<&str>,
    ) -> usize {
        let base = if constraint.ends_with(":D") || constraint.ends_with(":U") {
            &constraint[..constraint.len() - 2]
        } else {
            constraint
        };
        // If the source variable has a native type constraint, use it for dispatch.
        if let Some(vt) = var_type
            && Self::is_native_type_name(vt)
        {
            // Any native constraint in the SAME FAMILY is equally specific, not
            // just the identically-spelled one: rakudo answers `int8` for a
            // `my int $n` against `int8`/`Int` candidates exactly as it answers
            // `int64` against `int64`/`Int`. Comparing the spellings meant only
            // the one written the same way beat `Int`, so `my int $n` lost the
            // `int64`, `int8` and `int32` candidates to the boxed one.
            if Self::native_family(base).is_some()
                && Self::native_family(base) == Self::native_family(vt)
            {
                return 0;
            }
            // No same-family native candidate for this position: every other
            // constraint is ranked by the ORDINARY hierarchy distance, offset
            // by one so that a same-family native (0 above) still beats all of
            // them. A flat penalty here instead collapsed the distinctions
            // between them -- `Int` and `Numeric` both came out at 1 for an
            // `int`-typed argument, so `multi rt71754(Int)` /
            // `multi rt71754(Numeric)` called with an integer literal tied and
            // picked the wrong one (`roast/S06-advanced/callsame.t`).
            return 1 + self.type_hierarchy_distance(constraint, value);
        }
        self.type_hierarchy_distance(constraint, value)
    }

    /// Check if a type name is a native type.
    fn is_native_type_name(name: &str) -> bool {
        crate::runtime::native_types::native_family(name).is_some()
    }

    /// Map native type names to their boxed equivalents.
    /// The native *family* a native type name belongs to, or `None` when the
    /// name is not native. Two names in one family rank EQUALLY specific for an
    /// argument of that family, which is what rakudo does: `my int $n` picks the
    /// `int8` candidate over `Int` just as it picks `int64` over `Int`, and two
    /// same-family candidates are an "Ambiguous call" there rather than one
    /// winning. The families are exactly the boundaries rakudo enforces —
    /// signedness is one (a `my int $n` against `uint`/`Int` candidates answers
    /// `Int`, and an integer literal against `byte`/`Int` answers `Int`), and so
    /// is int-vs-num (`my int $n` against `num`/`Int` answers `Int`).
    fn native_family(name: &str) -> Option<&'static str> {
        crate::runtime::native_types::native_family(name)
    }
}
