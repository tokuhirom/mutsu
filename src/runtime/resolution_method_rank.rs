use super::multi_rank::{self, MultiRank, prune_to_maximal};
use super::*;
use std::cmp::Ordering;

impl Interpreter {
    /// The parameters a method candidate is ranked on: every one but the
    /// invocant, for the shared helpers in `multi_rank`.
    // Cost: O(p), p = parameters.
    pub(super) fn method_rank_params(def: &MethodDef) -> Vec<&ParamDef> {
        def.param_defs.iter().filter(|p| !p.is_invocant).collect()
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
                    && !p.name.starts_with("__type_capture__")
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

    /// The [`MultiRank`] of one matched method candidate: the literal count as
    /// its nominal tier, the type distance and profile of
    /// `method_candidate_type_distance_profile`, the refinement tier of
    /// [`Self::method_candidate_narrowness`] at `shared_positions`, and
    /// `decl_order` (see [`Self::method_decl_order`]).
    // Cost: O(p) plus one hierarchy walk per positional parameter, p = parameters.
    pub(super) fn method_rank_key(
        &self,
        args: &[Value],
        def: &MethodDef,
        shared_positions: usize,
        decl_order: u64,
    ) -> MultiRank {
        let params = Self::method_rank_params(def);
        // The literal-parameter count leads the nominal tier, ahead of type
        // distance, exactly as in multi-SUB dispatch (`candidate_rank_key`): a
        // literal is nominally as narrow as the argument it equals, so
        // `multi method m($f, 'khz')` beats `multi method m(Numeric $n, Str:D
        // $u)` for `m(0.1, 'khz')` although its first parameter is untyped
        // (CSS::Writer's unit conversion).
        let literals = params
            .iter()
            .filter(|p| !p.named && p.literal_value.is_some())
            .count();
        let (distance, profile) = self.method_candidate_type_distance_profile(args, def);
        let n = self.method_candidate_narrowness(def, shared_positions);
        MultiRank {
            nominal: (literals, 0),
            distance,
            refinement: [n.0, n.1, n.2, n.3],
            non_slurpy: !multi_rank::has_slurpy_positional(&params),
            bind_check: multi_rank::needs_named_bind_check(&params),
            optional: multi_rank::optional_positional_count(&params),
            decl_order,
            profile,
        }
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
    /// dispatch uses: `is default` first (among candidates tied on literal
    /// count and type distance), then rakudo's `is_narrower` partial order
    /// ([`prune_to_maximal`]), then the tier order, and finally the owner
    /// tie-break among the candidates still tied.
    // Cost: O(m^2 * h + m * p), m = matches, h = one nominal-type relation
    // walk, p = parameters per candidate.
    pub(crate) fn pick_method_winner(
        &mut self,
        mro: &[Symbol],
        arg_values: &[Value],
        invocant: Option<&Value>,
        mut all_matches: Vec<(Symbol, MethodDef)>,
    ) -> Option<(Symbol, MethodDef)> {
        if all_matches.len() <= 1 {
            return all_matches.into_iter().next();
        }
        let mut ranks: Vec<MultiRank> = all_matches
            .iter()
            .enumerate()
            .map(|(i, (owner, def))| {
                let order = Self::method_decl_order(mro, *owner, i);
                self.method_rank_key(arg_values, def, usize::MAX, order)
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
        // `is default` breaks a tie on the leading tiers outright.
        if lead_tied.len() > 1
            && let Some(&i) = lead_tied.iter().find(|&&i| all_matches[i].1.is_default)
        {
            return Some(all_matches.swap_remove(i));
        }
        // Across owners too: an ancestor's candidate and a subclass's that
        // are incomparable resolve by declaration order (`method_decl_order`).
        let mut ranked = prune_to_maximal(
            ranks
                .into_iter()
                .zip(0..)
                .collect::<Vec<(MultiRank, usize)>>(),
            &|x, y| self.nominal_types_related(x, y),
        );
        ranked.sort_by(|a, b| a.0.total_cmp(&b.0));
        let best = ranked[0].0;
        let tied: Vec<usize> = ranked
            .iter()
            .take_while(|(r, _)| r.tier_cmp(&best) == Ordering::Equal)
            .map(|(_, i)| *i)
            .collect();
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
