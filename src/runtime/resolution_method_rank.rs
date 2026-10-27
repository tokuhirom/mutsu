use super::multi_rank::{self, MultiRank, prune_to_maximal};
use super::*;
use std::cmp::Ordering;

impl Interpreter {
    /// The parameters a method candidate is ranked on: every one but the
    /// invocant (and the synthetic type-capture slots), for the walks shared
    /// with multi-sub dispatch.
    // Cost: O(p), p = parameters.
    pub(super) fn method_rank_params(def: &MethodDef) -> Vec<&ParamDef> {
        def.param_defs
            .iter()
            .filter(|p| !p.is_invocant && !p.is_bare_type_capture())
            .collect()
    }

    /// Number of positional parameters that participate in method specificity.
    /// Named and variadic parameters bind differently from ordinary positions.
    pub(super) fn method_specificity_positions(def: &MethodDef) -> usize {
        def.param_defs
            .iter()
            .filter(|p| {
                !p.is_invocant
                    && !p.named
                    && !p.is_variadic()
                    && !p.is_bare_type_capture()
            })
            .count()
    }

    /// Rank constraints only at positions shared by all competing owners.
    /// The invocant is compared before a position that another candidate does
    /// not declare; an optional parent position must not outrank a child's
    /// named-only method merely because its unbound type is a subset.
    pub(super) fn method_candidate_narrowness(
        &self,
        def: &MethodDef,
        shared_positions: usize,
    ) -> (usize, usize, usize, usize) {
        let mut rank = (0, 0, 0, 0);
        let mut positional_index = 0;
        for p in &def.param_defs {
            if !p.is_invocant && !p.named && !p.is_variadic() {
                let shared = positional_index < shared_positions;
                positional_index += 1;
                if !shared {
                    continue;
                }
            }
            if p.literal_value.is_some() {
                rank.0 += 1;
            }
            if !p.named
                && ((p.where_constraint.is_some() && !p.is_variadic())
                    || p.type_constraint.as_deref().is_some_and(|tc| {
                        self.constraint_is_subset(Self::constraint_base_for_distance(tc))
                    }))
            {
                rank.1 += 1;
            }
            if !p.is_invocant
                && !p.named
                && !p.slurpy
                && !p.double_slurpy
                && !p.onearg
                && (p.name.starts_with('@') || p.name.starts_with('%') || p.name.starts_with('&'))
            {
                rank.2 += 1;
            }
            if !p.is_invocant && p.traits.iter().any(|t| t == "rw" || t == "raw") {
                rank.3 += 1;
            }
        }
        rank
    }

    /// The [`MultiRank`] of one matched method candidate: the nominal tier
    /// (literal and typed positional counts) of
    /// [`Self::params_specificity_rank`], the type distance and profile of
    /// `method_candidate_type_distance_profile` — both the walks multi-SUB
    /// dispatch ranks by —, the refinement tier of
    /// [`Self::method_candidate_narrowness`] at `shared_positions`, and
    /// `decl_order` (see [`Self::method_decl_order`]).
    // Cost: O(p) plus one hierarchy walk per positional parameter, p = parameters.
    pub(super) fn method_rank_key(
        &self,
        args: &[Value],
        def: &MethodDef,
        shared_positions: usize,
        decl_order: u64,
        owner: Symbol,
    ) -> MultiRank {
        let params = Self::method_rank_params(def);
        // A literal is nominally as narrow as the argument it equals, so
        // `multi method m($f, 'khz')` beats `multi method m(Numeric $n, Str:D
        // $u)` for `m(0.1, 'khz')` although its first parameter is untyped
        // (CSS::Writer's unit conversion).
        let (literals, typed, ..) = self.params_specificity_rank(&params, args);
        let (distance, profile) = self.method_candidate_type_distance_profile(args, def);
        let n = self.method_candidate_narrowness(def, shared_positions);
        MultiRank {
            nominal: (literals, typed),
            distance,
            refinement: [n.0, n.1, n.2, n.3],
            constrained_slot: 1,
            non_slurpy: !multi_rank::has_slurpy_positional(&params),
            arity: multi_rank::positional_arity(&params),
            // A refined candidate (`where`, `subset`) carries rakudo's
            // bind-check flag, as for subs (see `candidate_rank_key`).
            bind_check: n.1 > 0 || multi_rank::needs_named_bind_check(&params),
            optional: n.1 == 0 && multi_rank::has_optional_positional(&params),
            decl_order,
            profile,
            // An explicit invocant type (`multi method m(RM:D:)`) is the
            // parameter rakudo ranks, not the class that owns the candidate: a
            // role-typed invocant is narrower than `Mu:D:` in the same class.
            invocant: Some(Self::explicit_invocant_type(def).unwrap_or(owner)),
            tier: 0,
            tiered: false,
            tier_bind_check: false,
            arity_fits: true,
        }
    }

    /// The nominal type an explicit invocant constraint (`(RM:D:)`) names, if
    /// the candidate writes one that is a plain type name (no type capture,
    /// no parameterization).
    // Cost: O(p), p = parameters.
    fn explicit_invocant_type(def: &MethodDef) -> Option<Symbol> {
        let constraint = def
            .param_defs
            .iter()
            .find(|p| p.is_invocant)?
            .type_constraint
            .as_deref()?;
        let constraint = ["D", "U", "_"]
            .iter()
            .find_map(|smiley| constraint.strip_suffix(&format!(":{smiley}")))
            .unwrap_or(constraint);
        let plain = !constraint.is_empty()
            && constraint
                .chars()
                .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | ':' | '\''))
            && !constraint.starts_with(':');
        plain.then(|| Symbol::intern(constraint))
    }

    /// The MRO index of the most-derived level with a built-in method
    /// `method_name`, or `None`.
    // Cost: O(m), m = MRO depth.
    pub(super) fn builtin_method_level(mro: &[Symbol], method_name: &str) -> Option<usize> {
        let name: &'static str = Symbol::intern(method_name).as_str();
        mro.iter().position(|owner| {
            let folded =
                crate::builtins::builtin_type_methods::canonical_builtin_owner(owner.as_str());
            let owner = if folded.is_empty() {
                owner.as_str()
            } else {
                folded
            };
            crate::builtins::method_introspection::native_method_row_exists(owner, name)
        })
    }

    /// Whether `class_name` has user candidates for `method_name` and every one
    /// is a `multi` declared above the level that declares the built-in (never
    /// a candidate of its dispatcher), or of only a capture / slurpies, while the receiver has a
    /// built-in of that name declared at the user candidates' level or below
    /// (`Any.map` against an augment of `Any`; not `Mu.say`, whose invocant is
    /// wider than the augmented `Any`): no user candidate can win a call,
    /// whatever its arguments (#12751).
    // Cost: O(m * c), m = MRO depth, c = candidates per level.
        pub(crate) fn user_candidates_all_outranked_by_builtin(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> bool {
        let mro = self.class_mro(class_name);
        let Some(builtin_level) = Self::builtin_method_level(&mro, method_name) else {
            return false;
        };
        // Rakudo derives a built-in class's multi dispatcher from its parent's
        // when the class is composed, so a `multi` an `augment` later adds to
        // an ANCESTOR never joins the dispatcher of a more derived class that
        // declares the method itself: `List` declares `head`, so an augmented
        // `Any.head(Int $x)` is not a candidate of `(1, 2, 3).head(2)`, whatever
        // its signature. Only a level at or above the declaring one can win.
        let declared_level = mro.iter().position(|owner| {
            let folded =
                crate::builtins::builtin_type_methods::canonical_builtin_owner(owner.as_str());
            let owner = if folded.is_empty() {
                owner.as_str()
            } else {
                folded
            };
            crate::builtins::method_introspection::native_method_declared(owner, method_name)
        });
        let mut any = false;
        for (level, owner) in mro.iter().enumerate() {
            let overloads = self
                .registry()
                .get_method_overloads(owner.as_str(), method_name);
            for def in overloads.iter().flatten() {
                if def.is_private {
                    continue;
                }
                if !def.is_multi {
                    return false;
                }
                if declared_level.is_some_and(|declared| level > declared) {
                    any = true;
                    continue;
                }
                if !Self::candidate_is_slurpy_only(def) || level < builtin_level {
                    return false;
                }
                any = true;
            }
        }
        any
    }

    /// Whether `def` takes nothing but a capture or slurpies (`|c`, `*@a`,
    /// `*%h`, and the implicit `*%_`): the candidates Rakudo ranks after every
    /// one with a plain positional parameter.
    // Cost: O(p), p = parameters of the candidate.
    pub(crate) fn candidate_is_slurpy_only(def: &MethodDef) -> bool {
        // The implicit `*%_` every method carries, like an explicit named
        // slurpy, does not affect the positional ranking: a candidate whose
        // only parameter is the explicit invocant (`multi method
        // FatRat(Str:D:)`) ranks like any other candidate.
        def.param_defs
            .iter()
            .any(|pd| pd.is_variadic() && !pd.name.starts_with('%'))
            && def
                .param_defs
                .iter()
                .all(|pd| pd.is_invocant || pd.is_variadic())
    }

    /// Pick the winning candidate from an MRO walk's collected multi-method
    /// matches, raising `X::Multi::Ambiguous` (`self.dispatch.dispatch_ambiguous`)
    /// when nothing breaks the tie. Extracted from
    /// `resolve_method_with_owner_impl` (ADR-0019 E4a) so a candidate list
    /// collected a different way — e.g. `resolution_sequence::resolve_sequence`'s
    /// TypeId-chain walk — is ranked with the exact same rules. `mro` is
    /// consulted only for [`Self::method_owner_tiebreak`].
    ///
    /// The candidates are ranked by [`MultiRank`], the relation multi-sub
    /// dispatch uses: rakudo's `is_narrower` partial order
    /// ([`prune_to_maximal`]), then the tier order, then `is default` among
    /// the candidates tied on it, and finally the owner tie-break among the
    /// candidates still tied.
    ///
    /// `rivals` are the visible multi candidates that did not bind. With the
    /// matches they form the candidate set rakudo sorts into topological
    /// tiers (`multi_rank::assign_tiers`, #12795), so a match in a later tier
    /// loses to one in an earlier tier even when the pair alone is
    /// incomparable.
    // Cost: O((m + r)^2 * h + (m + r) * p), m = matches, r = rivals, h = one
    // nominal-type relation walk, p = parameters per candidate.
    pub(crate) fn pick_method_winner(
        &mut self,
        mro: &[Symbol],
        arg_values: &[Value],
        invocant: Option<&Value>,
        mut all_matches: Vec<(Symbol, MethodDef)>,
        rivals: Vec<(Symbol, MethodDef)>,
    ) -> Option<(Symbol, MethodDef)> {
        if all_matches.len() <= 1 {
            return all_matches.into_iter().next();
        }
        // A method call boxes every argument, and a native parameter (`int`,
        // `uint`, `num`, `str`) takes a boxed argument only as a fallback:
        // rakudo's dispatcher tries the candidates a boxed argument fits
        // first (`multi method m(int $v)` against `multi method m(Int:D $v)`
        // answers the `Int` one for `m(5)`), so a native-parameter candidate
        // is only a rival of the others when nothing else matched.
        if all_matches
            .iter()
            .any(|(_, def)| !Self::has_native_positional_param(def))
        {
            all_matches.retain(|(_, def)| !Self::has_native_positional_param(def));
            if all_matches.len() <= 1 {
                return all_matches.into_iter().next();
            }
        }
        let mut ranks: Vec<MultiRank> = all_matches
            .iter()
            .enumerate()
            .map(|(i, (owner, def))| {
                let order = Self::method_decl_order(mro, *owner, i);
                self.method_rank_key(arg_values, def, usize::MAX, order, *owner)
            })
            .collect();
        // The candidates tied on the leading tiers (literal count, type
        // distance), in match-list order.
        let lead = |r: &MultiRank| (std::cmp::Reverse(r.nominal), r.distance);
        let best_lead = ranks.iter().map(lead).min()?;
        let lead_tied: Vec<usize> = (0..ranks.len())
            .filter(|&i| lead(&ranks[i]) == best_lead)
            .collect();
        // Rank refinements only at positions shared by all competing owners
        // (see `method_candidate_narrowness`).
        if lead_tied
            .iter()
            .any(|&i| all_matches[i].0 != all_matches[lead_tied[0]].0)
        {
            let shared = lead_tied
                .iter()
                .map(|&i| Self::method_specificity_positions(&all_matches[i].1))
                .min()
                .unwrap_or(0);
            for (rank, (_, def)) in ranks.iter_mut().zip(all_matches.iter()) {
                let n = self.method_candidate_narrowness(def, shared);
                rank.refinement = [n.0, n.1, n.2, n.3];
            }
        }
        // Rakudo's tiers over every candidate, the ones that did not bind
        // included.
        let matched = ranks.len();
        let mut tiered: Vec<MultiRank> = ranks.clone();
        tiered.extend(rivals.iter().enumerate().map(|(j, (owner, def))| {
            let order = Self::method_decl_order(mro, *owner, matched + j);
            self.method_rank_key(arg_values, def, usize::MAX, order, *owner)
        }));
        {
            let _scope = self.conformance_scope();
            multi_rank::assign_tiers(&mut tiered, |rank| rank, &|x, y| {
                self.nominal_type_conforms(x, y)
            });
        }
        for (rank, tiered) in ranks.iter_mut().zip(&tiered) {
            rank.tier = tiered.tier;
            rank.tiered = true;
        }
        // Across owners too: an ancestor's candidate and a subclass's that
        // are incomparable resolve by declaration order (`method_decl_order`).
        let (mut ranked, ambiguous) = {
            let _scope = self.conformance_scope();
            prune_to_maximal(
                ranks
                    .into_iter()
                    .zip(0..)
                    .collect::<Vec<(MultiRank, usize)>>(),
                &|x, y| self.nominal_type_conforms(x, y),
            )
        };
        ranked.sort_by(|a, b| a.0.total_cmp(&b.0));
        // An arity-incomparable split in one tier with no bind-time check:
        // rakudo's `X::Multi::Ambiguous`, unless `is default` picks one.
        if ambiguous {
            if let Some(&(_, i)) = ranked.iter().find(|(_, i)| all_matches[*i].1.is_default) {
                return Some(all_matches.swap_remove(i));
            }
            if invocant.is_some() {
                self.dispatch.dispatch_ambiguous = true;
            }
            let i = ranked[0].1;
            return Some(all_matches.swap_remove(i));
        }
        let best = ranked[0].0;
        let tied: Vec<usize> = ranked
            .iter()
            .take_while(|(r, _)| r.tier_cmp(&best) == Ordering::Equal)
            .map(|(_, i)| *i)
            .collect();
        // `is default` breaks a tie on the full tier order, as in sub
        // dispatch: a `where` refinement that makes a candidate narrower is
        // decided before it (#12775).
        if tied.len() > 1
            && let Some(&i) = tied.iter().find(|&&i| all_matches[i].1.is_default)
        {
            return Some(all_matches.swap_remove(i));
        }
        let (winner, still_tied) = Self::method_owner_tiebreak(&tied, &all_matches, mro);
        // Ambiguous dispatch: two or more candidates remain equally specific
        // (same tiers and the same most-derived owner class) and none is
        // marked `is default`. Raku raises X::Multi::Ambiguous here.
        // Named parameters never make a dispatch ambiguous (rakudo): tied
        // candidates that all declare explicit named params (the bind-check
        // tier, which every tied candidate shares) resolve by declaration
        // order (`(Any :$file!)` vs `(Str :$file!)` picks the first).
        // Only a resolution that HAS the invocant can legitimately declare an
        // ambiguity: without it, `method_args_match_for_invocant` skips every
        // invocant type/definedness constraint, so a `:U:`/`:D:` multi pair
        // (or any invocant-narrowed candidates) all "match" and tie spuriously.
        // Such invocant-less resolutions come from introspection/speculative
        // callers (`has_user_method`, `Bool`/`Str` existence probes) that never
        // consume `dispatch_ambiguous`; leaving the flag set here instead leaks
        // a false ambiguity into the NEXT real dispatch (a defined-instance
        // `.gist`/`.Str` call poisoning a following `self.pairs`). The real
        // dispatch always re-resolves WITH the invocant, so deferring the
        // decision to it loses nothing.
        // A bind-check winner (a `where`, a subset, a literal or a
        // destructuring sub-signature) is never ambiguous either: rakudo
        // trial-binds tied candidates and the first declared that binds
        // wins (#11027), as the sub dispatch's
        // `candidate_uses_order_sensitive_dispatch` does.
        if still_tied > 1
            && !best.bind_check
            && invocant.is_some()
            && !self.params_use_order_sensitive_dispatch(
                all_matches[winner]
                    .1
                    .param_defs
                    .iter()
                    .filter(|p| !p.is_invocant),
            )
        {
            self.dispatch.dispatch_ambiguous = true;
        }
        Some(all_matches.swap_remove(winner))
    }

    /// Whether `def` declares a positional parameter of a native type
    /// (`int`, `uint32`, `num`, `str`, ...).
    // Cost: O(p), p = parameters of the candidate.
    fn has_native_positional_param(def: &MethodDef) -> bool {
        def.param_defs.iter().any(|p| {
            !p.is_invocant
                && !p.named
                && p.type_constraint.as_deref().is_some_and(|tc| {
                    let base = tc.strip_suffix(":D").or_else(|| tc.strip_suffix(":U")).unwrap_or(tc);
                    crate::native_types::native_family(base).is_some()
                })
        })
    }

    /// The declaration order of match `index` (the MRO-ordered match list),
    /// owned by `owner`: rakudo derives a subclass's multi dispatcher from its
    /// parent's and appends the subclass's own candidates, so an ancestor's
    /// candidates are declared first and, within one owner, the list order
    /// is the declaration order. This is what decides between incomparable
    /// candidates of different owners: rakudo answers `P` for
    /// `class P { multi method m(Int $x, Any $y) }` and
    /// `class C is P { multi method m(Any $x where True, Int $y) }` called as
    /// `C.new.m(1, 2)`. An owner missing from `mro` ranks as the receiver's
    /// own class.
    // Cost: O(r), r = MRO length.
    fn method_decl_order(mro: &[Symbol], owner: Symbol, index: usize) -> u64 {
        let depth = mro.iter().position(|cn| *cn == owner).unwrap_or(0);
        (((mro.len() - depth) as u64) << 32) | index as u64
    }

    /// Break a tie between equally narrow method candidates (`tied`, indices
    /// into `matches` in declaration order) by their owners: a `multi method`
    /// defined on the receiver's own class is more specific than one
    /// inherited from an ancestor, so the most-derived owner (earliest in
    /// `mro`) wins; then a candidate the class declares itself outranks an
    /// equally narrow one composed into it from a role (rakudo: the class's
    /// own `multi method new(A:U: :$raw-error)` beats the role's
    /// `new(::?CLASS: :$raw-error = False, *%in)`), whatever order the
    /// registry happened to push them in; then the first declared. Returns
    /// the winner and how many candidates were still tied with it before
    /// declaration order, for the ambiguity check.
    // Cost: O(t * r), t = tied candidates, r = MRO length.
    fn method_owner_tiebreak(
        tied: &[usize],
        matches: &[(Symbol, MethodDef)],
        mro: &[Symbol],
    ) -> (usize, usize) {
        let mro_index =
            |owner: Symbol| mro.iter().position(|cn| *cn == owner).unwrap_or(usize::MAX);
        let best_mro = tied
            .iter()
            .map(|&i| mro_index(matches[i].0))
            .min()
            .unwrap_or(usize::MAX);
        let mut narrowed: Vec<usize> = tied
            .iter()
            .copied()
            .filter(|&i| mro_index(matches[i].0) == best_mro)
            .collect();
        if narrowed.iter().any(|&i| matches[i].1.role_origin.is_none()) {
            narrowed.retain(|&i| matches[i].1.role_origin.is_none());
        }
        (narrowed[0], narrowed.len())
    }

    /// The nominal distance of an argument from the implicit constraint of an
    /// unconstrained `@`/`%` parameter (`Positional`/`Associative`), shared by
    /// multi-sub and multi-method dispatch. A `Seq` binds to `@` through
    /// `PositionalBindFailover` (it is cached into a `List`), so it ranks as
    /// that List: `(@arr)` stays narrower than `(Str() $k)` or `(Any $x)` for
    /// `"abc".comb`, as in rakudo (Trie's `delete(@arr)` / `delete(Str() $key)`).
    /// `None` when the Seq rule does not apply.
    // Cost: O(d), d = depth of `List`'s type hierarchy.
    pub(super) fn seq_as_positional_distance(&self, implicit: &str, arg: &Value) -> Option<usize> {
        (implicit == "Positional" && arg.is_seq_value()).then(|| {
            self.type_hierarchy_distance(implicit, &Value::package(Symbol::intern("List")))
        })
    }
}
