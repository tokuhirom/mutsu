//! The per-argument walk that ranks one multi candidate against one call:
//! its nominal tier (literal and typed positional counts) and its
//! type-hierarchy distance with the per-parameter [`RankProfile`].
//!
//! Multi subs and multi methods both rank through these two functions; the
//! callers differ only in which parameters they pass (a method drops its
//! invocant), the package that resolves a bare type name in the signature,
//! and whether the call site's literal-argument mask applies (#12776). They
//! used to be two walks that drifted apart (a slurpy cost twice as much for a
//! method, a coercion parameter ranked differently, only a method knew `Nil`).

use super::dispatch_candidates::UNRELATED_DISTANCE;
use super::multi_rank::{PROFILE_LEN, RankProfile};
use super::*;
use crate::value::ValueView;

/// The candidate-side context of one ranking walk.
#[derive(Clone, Copy)]
pub(crate) struct WalkOwner {
    /// The package a bare user type name in the signature is resolved from:
    /// the package that declared the candidate.
    pub(crate) package: Symbol,
}

impl Interpreter {
    /// The narrowness tuple of a candidate whose dispatch-visible parameters
    /// are `params` (the invocant excluded), in
    /// `(literal, typed, constrained, subsig, writable)` order — the first two
    /// are the nominal tier, the rest the refinement tier. Coercion
    /// parameters rank against the arguments that would bind to them — see
    /// [`Self::effective_dispatch_constraint`]. Pass an empty `args` slice
    /// when no call is in flight (ordering a `callsame` chain, say); coercion
    /// params then rank by the type they accept.
    // Cost: O(p) plus one hierarchy walk per coercion parameter, p = parameters.
    pub(super) fn params_specificity_rank(
        &self,
        params: &[&ParamDef],
        args: &[Value],
    ) -> (usize, usize, usize, usize, usize) {
        // Type narrowness is computed from the POSITIONAL parameters only.
        // A named parameter's type decides whether the candidate is
        // *applicable*, never how narrow it is — `multi p(Any :$a)` and
        // `multi p(Int :$a)` are equally narrow in rakudo, so `p(a => 1)`
        // picks whichever is declared first (verified against rakudo; the
        // positional pair `multi r(Any $x, Int :$a)` / `multi r(Int $x, Any
        // :$a)` still resolves to the `Int $x` one). See
        // `candidate_has_typed_named_param` for the matching ambiguity rule.
        let params: Vec<&ParamDef> = params.iter().copied().filter(|p| !p.named).collect();
        let param_args = self.dispatch_param_arguments(&params, args);
        let effective: Vec<Option<&str>> = params
            .iter()
            .zip(param_args.iter())
            .map(|(p, arg)| {
                p.type_constraint
                    .as_deref()
                    .map(|tc| self.effective_dispatch_constraint(tc, arg.as_ref()))
            })
            .collect();
        let literal_value_count = params.iter().filter(|p| p.literal_value.is_some()).count();
        // A parameter is constrained by a `where` clause or by a `subset` type,
        // and rakudo ranks the two alike: each is a bind-time check on top of
        // the same nominal type, so `multi f(Small $x)` and `multi f(Int $x
        // where * < 10_000)` tie and declaration order decides. Counting them
        // as two separate components made every `where` beat every subset.
        // A parameter carrying both still counts once, as rakudo's
        // per-parameter "has a constraint" comparison does.
        let constrained_count = params
            .iter()
            .zip(effective.iter())
            .filter(|(p, tc)| {
                // A `where` on a slurpy refines nothing positional; it ranks
                // in the named bind-check step instead (see
                // `multi_rank::needs_named_bind_check`).
                (p.where_constraint.is_some() && !p.is_variadic())
                    || tc
                        .map(Self::constraint_base_name)
                        .is_some_and(|base| self.constraint_is_subset(base))
            })
            .count();
        let typed_param_count = params
            .iter()
            .zip(effective.iter())
            .zip(param_args.iter())
            // An optional positional the call supplies nothing for is not
            // compared: rakudo checks only the positions both candidates
            // declare, so `multi m(Int $x)` ties `multi m(Int $x, Int $y?)`
            // for `m(1)` and the optional-count step picks the former.
            .filter(|((p, _), arg)| {
                arg.is_some() || !(p.optional_marker || p.default.is_some())
            })
            .map(|(pt, _)| pt)
            .filter(|(p, tc)| {
                // A bare `Mu`/`Any` constraint is no narrower than an
                // unconstrained param (raku: unconstrained IS Any) — counting
                // it made `(Any:D $j, Mu)` outrank `($j, @x)` wholesale
                // (JSON::Unmarshal's fallback swallowing the Positional
                // candidate). A smiley (`Any:D`) still counts.
                let meaningful_type = tc.is_some_and(|tc| !matches!(tc, "Mu" | "Any"));
                meaningful_type
                    // An optional param (`&c = &say`, `@a?`) takes no part in
                    // rakudo's narrowness comparison, so its implied
                    // Positional/Associative/Callable type must not either.
                    // A single-argument slurpy (`+@a`) is a slurpy too: its
                    // implied Positional is no narrowness either.
                    || (!p.is_variadic()
                        && !p.optional_marker
                        && p.default.is_none()
                        && (p.name.starts_with('@')
                            || p.name.starts_with('%')
                            || p.name.starts_with('&')))
            })
            .count();
        let subsig_count = params.iter().filter(|p| p.sub_signature.is_some()).count();
        // A slurpy's `is raw` (`*@a is raw`) only keeps its elements' containers;
        // it requires no writable argument, so it must not make the candidate
        // narrower than `()` for a call with no arguments (P5chomp's
        // `multi chomp()` that dies vs `multi chomp(*@a is raw)`).
        let writable_trait_count = params
            .iter()
            .filter(|p| !p.is_variadic() && p.traits.iter().any(|t| matches!(t.as_str(), "rw" | "raw")))
            .count();
        (
            // NOMINAL tier. A literal parameter is not merely a refinement:
            // rakudo compiles it to the literal's own type plus an equality
            // constraint, so it is nominally exactly as narrow as the argument
            // and only then wins the tie on the constraint. It therefore leads
            // the tier, ahead of the plain type count.
            literal_value_count,
            typed_param_count,
            // REFINEMENT tier — consulted only once two candidates are tied on
            // the nominal tier AND on type-hierarchy distance. See
            // [`MultiRank`].
            constrained_count,
            subsig_count,
            // `rw`/`raw` are dispatch-visible because they require a writable
            // argument; `copy` changes only binding and must not outrank an
            // otherwise equal candidate. Keep the slot for the callers that
            // compare the nominal/refinement components separately.
            writable_trait_count,
        )
    }

    /// The total type-hierarchy distance between a candidate's parameters
    /// (`params`, dispatch-visible, the invocant excluded) and the call's
    /// arguments, lower being narrower, together with the per-parameter
    /// [`RankProfile`] the same walk produces.
    ///
    /// Each positional parameter contributes the number of MRO levels between
    /// its constraint and the argument's type. An untyped `$` parameter is an
    /// implicit `Any`, an untyped `@`/`%` an implicit `Positional` /
    /// `Associative`. A positional slurpy that swallows an argument costs a
    /// flat 1000, and so does a required parameter the call supplies nothing
    /// for (that candidate cannot bind at all).
    // Cost: O(p) plus one hierarchy walk per positional parameter, p = parameters.
    pub(crate) fn params_type_distance_profile(
        &self,
        args: &[Value],
        params: &[&ParamDef],
        owner: WalkOwner,
    ) -> (usize, RankProfile) {
        let mut total = 0usize;
        let mut profile = RankProfile::EMPTY;
        // The refinement bit and nominal type of the positional slot being
        // walked; recorded once the next slot (or the end) fixes its distance.
        let mut slot_refined = false;
        let mut slot_nominal: Option<Symbol> = None;
        // The slot's declared type, independent of the argument
        // ([`RankProfile::declared_wins`]).
        let mut slot_declared: Option<Symbol> = None;
        let mut slot_coercive = false;
        // The slot is an optional positional this call does not reach; its
        // `slot_nominal` is then the declared type.
        let mut slot_unreached = false;
        let mut prev_total = 0usize;
        let mut slot = 0usize;
        let mut pos_idx = 0usize;
        // The next positional argument (named-argument Pairs are skipped),
        // unwrapped for dispatch, with the source variable's declared type.
        let next_positional = |pos_idx: &mut usize| -> Option<(Value, Option<String>)> {
            while *pos_idx < args.len() && matches!(args[*pos_idx].view(), ValueView::Pair(..)) {
                *pos_idx += 1;
            }
            let idx = *pos_idx;
            let value = args.get(idx)?;
            *pos_idx += 1;
            Some(self.unwrap_arg_for_dispatch(value))
        };
        for pd in params.iter() {
            // A named parameter does not contribute to narrowness (see
            // `params_specificity_rank`), so it must not move the distance
            // either — neither its type (`multi p(Int :$a)` would otherwise
            // outrank `multi p(Any :$a)`) nor its mere presence (`multi K($in,
            // :$a, :$b)` must not lose to `multi K($in, :$a)` purely for
            // declaring one named more — rakudo picks whichever of the two is
            // declared first). Nameds never consume a positional slot.
            if pd.named {
                continue;
            }
            // A slurpy hash (`*%m`) takes only named arguments, so it is
            // skipped for the same reason: charging it the flat 1000 below
            // made `multi f(*%m where .elems == 1)` lose to `multi f(:$x!)`
            // whatever their declaration order (#10519).
            if pd.slurpy && pd.name.starts_with('%') {
                continue;
            }
            if slot > 0 {
                profile.record(slot - 1, total - prev_total, slot_refined, slot_nominal, slot_declared, slot_coercive);
                if slot_unreached && let Some(declared) = slot_nominal {
                    profile.mark_unreached(slot - 1, declared);
                }
            }
            prev_total = total;
            slot_nominal = None;
            slot_declared = None;
            slot_coercive = pd
                .type_constraint
                .as_deref()
                .is_some_and(crate::runtime::types::is_coercion_constraint);
            slot_unreached = false;
            if slot < PROFILE_LEN {
                slot_refined = (pd.where_constraint.is_some() && !pd.is_variadic())
                    || pd.literal_value.is_some()
                    || pd
                        .type_constraint
                        .as_deref()
                        .map(Self::constraint_base_name)
                        .is_some_and(|base| self.constraint_is_subset(base));
            }
            slot += 1;
            // A positional slurpy that receives no argument is not compared
            // at all. Rakudo's `is_narrower` checks only the positional
            // parameters both candidates declare, and consults slurpiness only
            // once those are tied (the slurpy step of [`MultiRank`]). Charging
            // it made `multi a(Str:D $n, *@p)` lose `a('c')` to
            // `multi a(Any:D $r)` (#11045). A slurpy that does swallow an
            // argument costs the flat 1000: that is rakudo's arity-mismatch
            // rule, under which the non-slurpy candidate that binds the same
            // call is narrower (`multi f($x)` beats `multi f(*@a)` for `f(1)`).
            // A single-argument slurpy (`+@a`) binds one argument and ranks as
            // the ordinary parameter it then is.
            if pd.slurpy || pd.double_slurpy {
                if next_positional(&mut pos_idx).is_some() {
                    total += 1000;
                }
                continue;
            }
            let Some((arg, var_type)) = next_positional(&mut pos_idx) else {
                // An OPTIONAL positional the call supplied no argument for is
                // not a wider parameter — it is one this call does not compare
                // at all. Rakudo caps its narrowness comparison at the shorter
                // signature's positional count whenever the two candidates
                // share a minimum arity, so a surplus optional contributes
                // nothing to either side (`multi pick(Int $seed = 0, $? where
                // { ... })` vs `multi pick(Int $seed = 0)`, `t/routines/
                // signature/optional-param-where-runs-when-omitted.t`). A
                // required parameter with no argument costs 1000: that
                // candidate cannot bind the call at all.
                if pd.optional_marker || pd.default.is_some() {
                    // Rakudo still orders the candidates by this parameter's
                    // DECLARED type (`multi g(Int $a?)` is narrower than
                    // `multi g($a?)` for `g()`), so record it for
                    // `RankProfile::unreached_wins`.
                    slot_unreached = true;
                    slot_nominal = Some(self.declared_positional_nominal(pd, owner.package));
                    slot_declared = slot_nominal;
                } else if !pd.name.starts_with('&') {
                    total += 1000;
                }
                continue;
            };
            if let Some(constraint) = &pd.type_constraint {
                let effective = self.effective_dispatch_constraint(constraint, Some(&arg));
                let nominal =
                    self.dispatch_nominal_base(Self::constraint_base_name(effective), Some(&arg));
                let base = self.resolve_signature_type_name(nominal.as_ref(), owner.package);
                if slot <= PROFILE_LEN {
                    slot_nominal = Some(Symbol::intern(&base));
                    slot_declared = slot_nominal;
                }
                let distance = self.typed_param_distance(pd, &base, &arg, var_type.as_deref());
                if distance >= UNRELATED_DISTANCE {
                    profile.mark_unrelated();
                } else if let ValueView::Mixin(_, mixins) = arg.view()
                    && mixins.contains_key(crate::meta_ns::MetaNs::Role.str_key_for_str(&base))
                {
                    profile.mark_role_mixin();
                }
                total += distance;
                continue;
            }
            // An unconstrained `@`/`%` param carries an implicit
            // Positional/Associative constraint: it must out-narrow a `Mu`
            // candidate for a Positional/Associative argument
            // (JSON::Unmarshal's `_unmarshal($json, @x)` vs `($json, Mu)`
            // dispatching a `Positional[Dog]` attribute type).
            if pd.name.starts_with('@') || pd.name.starts_with('%') {
                let implicit = if pd.name.starts_with('@') {
                    "Positional"
                } else {
                    "Associative"
                };
                slot_declared = Some(Symbol::intern(implicit));
                // A `Seq` binds to `@` through `PositionalBindFailover` (it is
                // cached into a `List`), so it ranks as that List: `multi
                // h(@raw)` stays narrower than `multi h(Any:D $x)` for
                // `h("a b".words)`, as in rakudo, instead of scoring as
                // unrelated to `Positional`.
                let distance = match self.seq_as_positional_distance(implicit, &arg) {
                    Some(distance) => distance,
                    None => self.arg_type_distance(implicit, &arg, var_type.as_deref()),
                };
                if distance >= UNRELATED_DISTANCE {
                    profile.mark_unrelated();
                }
                total += distance;
                continue;
            }
            // A LITERAL parameter (`multi f(42)`) carries no type constraint
            // here, but it is not an unconstrained parameter: rakudo compiles
            // it to the literal's own type plus an equality constraint, so it
            // is nominally exactly as narrow as any argument that can bind to
            // it. It scores distance 0 rather than the implicit-`Any` distance
            // below — otherwise the type distance, which outranks the
            // refinement tier, would hand every literal candidate's tie to a
            // plainly-typed sibling (`multi f(42)` losing to `multi f(Int $x)`).
            if pd.literal_value.is_some() {
                continue;
            }
            slot_declared = Some(Symbol::intern(if pd.name.starts_with('&') {
                "Callable"
            } else {
                "Any"
            }));
            // An untyped `&` parameter has no ranked type.
            if pd.name.starts_with('&') {
                total += 1000;
                continue;
            }
            // An unconstrained positional `$`-param is an implicit `Any`
            // constraint (raku): rank it at the Any distance instead of a flat
            // charge, so `($j, @x)` can out-narrow `(Any:D $j, Mu)` on the
            // SECOND param (JSON::Unmarshal's `_unmarshal($json, @x)` vs its
            // `(Any:D $json, Mu)` fallback), and `($n where ..., $v)` ties
            // `($n, Any $v)` on the second position.
            total += self.arg_type_distance("Any", &arg, var_type.as_deref());
        }
        if slot > 0 {
            profile.record(slot - 1, total - prev_total, slot_refined, slot_nominal, slot_declared, slot_coercive);
            if slot_unreached && let Some(declared) = slot_nominal {
                profile.mark_unreached(slot - 1, declared);
            }
        }
        (total, profile)
    }

    /// The nominal type a positional parameter declares, with no argument to
    /// measure it against: its type constraint (a coercion by the type it
    /// accepts, a `subset` by its base type), else the type its sigil
    /// implies — `Positional` for `@`, `Associative` for `%`, `Callable` for
    /// `&` and `Any` for `$`.
    // Cost: O(1) for a known type; one registry probe otherwise.
    fn declared_positional_nominal(&self, pd: &ParamDef, package: Symbol) -> Symbol {
        if let Some(constraint) = &pd.type_constraint {
            let effective = self.effective_dispatch_constraint(constraint, None);
            let nominal = self.dispatch_nominal_base(Self::constraint_base_name(effective), None);
            return Symbol::intern(&self.resolve_signature_type_name(nominal.as_ref(), package));
        }
        Symbol::intern(match pd.name.as_bytes().first() {
            Some(b'@') => "Positional",
            Some(b'%') => "Associative",
            Some(b'&') => "Callable",
            _ => "Any",
        })
    }

    /// A bare user type name in a candidate's signature names the type
    /// visible from the package that DECLARED it (`my role ContentX` inside
    /// `role XML::Class` registers as `XML::Class::ContentX`); measuring the
    /// bare spelling against the argument's composed roles found nothing and
    /// scored the 500 "unrelated" distance, so a wider candidate out-ranked
    /// the role-typed one (XML::Class t/065).
    // Cost: O(1) for a known type; one registry probe otherwise.
    fn resolve_signature_type_name(&self, base: &str, package: Symbol) -> String {
        if crate::runtime::utils::is_known_type_constraint(base) || self.has_type_direct(base) {
            return base.to_string();
        }
        let by_owner = self.resolve_type_name_for_owner(&package.resolve(), base.to_string());
        // A lexical type (`my class VL`) the candidate's module imported is
        // stored as `VL\u{0}<decl-id>` and the caller's scope may not name it
        // at all (#12710). Applicability already resolves the bare spelling
        // through the registry's unique lexical key (`type_matches_value`);
        // rank against the same type, or `Positional` out-narrows `VL` for a
        // `VL` argument.
        if by_owner == base
            && self.registry().has_lexical_type_key_for(base)
            && let Some(key) = self.unique_lexical_type_key(base)
        {
            return key;
        }
        by_owner
    }

    /// The distance of a typed parameter whose resolved nominal type is
    /// `base` from the argument `arg`.
    // Cost: one hierarchy walk.
    fn typed_param_distance(
        &self,
        pd: &ParamDef,
        base: &str,
        arg: &Value,
        var_type: Option<&str>,
    ) -> usize {
        if pd.name.starts_with('&')
            && let Some(return_type) = self.callable_return_type(arg)
        {
            return self.type_hierarchy_distance(base, &Value::package(Symbol::intern(&return_type)));
        }
        if pd.name.starts_with('@') || pd.name.starts_with('%') {
            if let Some(info) = self.container_type_metadata(arg)
                && !info.value_type.is_empty()
            {
                return self
                    .type_hierarchy_distance(base, &Value::package(Symbol::intern(&info.value_type)));
            }
            if let Some(var_type) = var_type {
                return self.type_hierarchy_distance(base, &Value::package(Symbol::intern(var_type)));
            }
        }
        self.arg_type_distance(base, arg, var_type)
    }

    /// [`Self::type_hierarchy_distance`] of `arg` from `constraint`, through
    /// the source variable's native type (see
    /// `type_hierarchy_distance_with_var_type`) and with `Nil`'s own MRO.
    // Cost: one hierarchy walk.
    fn arg_type_distance(&self, constraint: &str, arg: &Value, var_type: Option<&str>) -> usize {
        if var_type.is_none() && matches!(arg.view(), ValueView::Nil) {
            return Self::nil_type_distance(constraint);
        }
        self.type_hierarchy_distance_with_var_type(constraint, arg, var_type)
    }

    /// `value_type_name(Nil)` reports "Any", so a `Nil` argument needs its own
    /// MRO: a `(Nil)` candidate must out-narrow e.g. `(Str() $s)` for a
    /// literal Nil argument (URI's `authority(Nil)`).
    // Cost: O(d), d = depth of `Nil`'s type hierarchy.
    fn nil_type_distance(constraint: &str) -> usize {
        let base = constraint
            .strip_suffix(":D")
            .or_else(|| constraint.strip_suffix(":U"))
            .unwrap_or(constraint);
        let nil_mro =
            crate::builtin_types::ancestry::builtin_type_narrowness_chain("Nil").unwrap_or(&[]);
        nil_mro
            .iter()
            .position(|&ancestor| ancestor == base)
            .unwrap_or(UNRELATED_DISTANCE)
    }

    /// Unwrap one argument for ranking: a [`Value::varref`] wrapper yields its
    /// value and the source variable's declared type constraint, and a
    /// container argument (a hash element, a Pair's value) ranks by its
    /// contents, as it type-checks.
    ///
    /// A literal argument carries no native type: it is a boxed `Int` / `Num`
    /// / `Str`, so `multi d(int)` / `multi d(Int)` called as `d(5)` ranks the
    /// boxed candidate first, as rakudo 2026.09 does (#13065). Only a source
    /// variable declared `int` / `num` / `str` ranks as native.
    // Cost: O(1) plus one variable-type lookup for a varref.
    fn unwrap_arg_for_dispatch(&self, value: &Value) -> (Value, Option<String>) {
        let (mut arg, mut var_type) = self.unwrap_varref_for_dispatch(value);
        if arg.is_container_ref() {
            // A native cell ranks as the native variable it is the container
            // of (ADR-15309).
            if var_type.is_none() {
                var_type = arg.native_container_type();
            }
            arg = arg.deref_container();
        }
        (arg, var_type)
    }
}
