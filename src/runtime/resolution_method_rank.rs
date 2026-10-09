use super::*;
use super::dispatch_candidates::RankProfile;

impl Interpreter {
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

    /// Apply rakudo's `is_narrower` partial order to the matched candidates of
    /// one owner: drop every candidate another one out-narrows and, when what
    /// is left is split by INCOMPARABLE candidates (one wins a parameter by a
    /// refinement -- `where`, literal, subset -- the other by a nominal type),
    /// keep only the first declared, which is what rakudo's tier walk runs.
    /// The method half of `prune_incomparable_matches`; both rest on
    /// [`RankProfile::incomparable`] ([#11175](https://github.com/tokuhirom/mutsu/issues/11175)).
    /// Candidates of different owners, or sets with no incomparable pair, are
    /// left to the tie-breaks in `pick_method_winner`.
    // Cost: O(m^2 * p), m = matched candidates, p = positional parameters.
    pub(super) fn prune_incomparable_method_matches(
        &self,
        args: &[Value],
        all_matches: &mut Vec<(Symbol, MethodDef)>,
    ) {
        if all_matches.len() < 2 || all_matches.iter().any(|(o, _)| *o != all_matches[0].0) {
            return;
        }
        let related = |a: Symbol, b: Symbol| self.nominal_types_related(a, b);
        // (literals, distance, narrowness) -- the order `pick_method_winner`
        // applies its filters in; the first is higher-is-narrower.
        let keys: Vec<(usize, usize, (usize, usize, usize, usize), RankProfile)> = all_matches
            .iter()
            .map(|(_, def)| {
                let literals = def
                    .param_defs
                    .iter()
                    .filter(|p| !p.is_invocant && !p.named && p.literal_value.is_some())
                    .count();
                let (dist, profile) = self.method_candidate_type_distance_profile(args, def);
                (
                    literals,
                    dist,
                    self.method_candidate_narrowness(def, usize::MAX),
                    profile,
                )
            })
            .collect();
        let n = keys.len();
        let incomparable = |i: usize, j: usize| keys[i].3.incomparable(&keys[j].3, &related);
        if !(0..n).any(|i| (i + 1..n).any(|j| incomparable(i, j))) {
            return;
        }
        let narrower = |o: usize, k: usize| {
            use std::cmp::Ordering::*;
            if incomparable(o, k) {
                return false;
            }
            keys[k]
                .0
                .cmp(&keys[o].0)
                .then(keys[o].1.cmp(&keys[k].1))
                .then(keys[k].2.cmp(&keys[o].2))
                == Less
        };
        let maximal: Vec<usize> = (0..n)
            .filter(|&k| !(0..n).any(|o| o != k && narrower(o, k)))
            .collect();
        let same_rank = |i: usize, j: usize| {
            (keys[i].0, keys[i].1, keys[i].2) == (keys[j].0, keys[j].1, keys[j].2)
        };
        let keep: Vec<usize> = if maximal.iter().all(|&i| same_rank(i, maximal[0])) {
            maximal
        } else {
            // Incomparable survivors: declaration order decides.
            maximal.into_iter().take(1).collect()
        };
        let mut idx = 0;
        all_matches.retain(|_| {
            let kept = keep.contains(&idx);
            idx += 1;
            kept
        });
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
