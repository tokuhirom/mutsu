//! ADR-0019 Phase E box E9a: the flat deferral expansion.
//!
//! [`Interpreter::resolve_deferral_expansion`] replaces [`Self::resolve_all_methods_with_owner`]
//! as the ordering source for a method dispatch's `nextsame`/`callsame`/`nextwith`/`callwith`
//! "remaining" candidate list. The two functions answer the same question (every candidate a
//! deferral from `class_name.method_name` can still reach) but order it differently:
//! `resolve_all_methods_with_owner` walks the MRO once, per-level, in stored declaration order;
//! this box instead builds the ordering raku itself uses — one block per MRO class, where a
//! class with `multi method` candidates contributes its *governing proto's specificity-ranked
//! candidate block* rather than its own bare declaration order. See "E9 design decision 2 —
//! REDRAWN" in `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` for the raku ground
//! truth (two confirmed predictions, `t/defer-multi-cross-level-proto-block.t`) this
//! reproduces.
//!
//! **The governing block, recursively.** For an MRO class `K` that declares its own `multi
//! method` candidates for `name`:
//! - if `K` declares its OWN `proto method name` (explicit), `K`'s block is exactly `K`'s own
//!   candidates, ranked — ancestor candidates are NOT merged in (an explicit proto isolates the
//!   subtree it roots from whatever the same name means further up the MRO);
//! - otherwise (an implicit proto), `K`'s block is `K`'s own candidates merged with the nearest
//!   ancestor's own governing block (found by walking the MRO past `K`), all ranked together as
//!   one set.
//!
//! Ranking is **nominal** (declared-type narrowness, not a distance to any particular call's
//! argument values — the block is a structural fact of the class hierarchy, consulted the same
//! way regardless of which args a later `nextwith`/`callwith` substitutes), with MRO depth
//! (more-derived first) then declaration order breaking ties. Per-call argument matching still
//! happens exactly where it always has — the caller of [`Self::resolve_deferral_expansion`]
//! filters this list with [`Self::method_args_match_for_invocant`], same as it filtered
//! `resolve_all_methods_with_owner`'s output.
//!
//! The same candidate can legitimately occur more than once in the expansion (once per governing
//! block it is merged into) — this is not a dedup bug. A parent's multi candidate that also
//! covers the child's own governing block re-runs when deferral walks past the child's block
//! into the parent's own block (raku ground truth, `defer-multi-cross-level-proto-block.t`).

use super::*;

impl Interpreter {
    /// The overloads `class_name` declares directly for `method_name` (not inherited) —
    /// identical read to [`Self::resolve_all_methods_with_owner`]'s per-level lookup (class
    /// table, falling back to the role table for an MRO entry that is a punned role rather than
    /// a class): [`crate::runtime::registry::Registry::get_method_overloads_with_role_fallback`].
    /// Deliberately NOT the bare `user_method_overloads`/`get_method_overloads`: that table has
    /// a known gap for a role that was never punned (see `resolution_sequence.rs`'s module doc)
    /// which `resolve_all_methods_with_owner` does not share, and this function must not
    /// regress it.
    fn own_overloads_at_level(&mut self, owner: &str, method_name: &str) -> Option<Vec<MethodDef>> {
        self.registry()
            .get_method_overloads_with_role_fallback(owner, method_name)
    }

    /// Nominal (argument-independent) narrowness of `def`'s positional, non-slurpy parameter
    /// types: the sum of each type constraint's own MRO depth (an `Int` constraint scores
    /// higher than an `Any` one because `Int`'s MRO chain is longer), mirroring how
    /// [`Self::pick_method_winner`]'s distance-based ranking treats a deeper nominal type as
    /// more specific, but without reference to any call's actual argument values. A subset
    /// resolves to its ultimate base type first, same as the arg-distance ranking does.
    fn nominal_positional_narrowness(&mut self, def: &MethodDef) -> usize {
        let mut total = 0usize;
        for pd in &def.param_defs {
            if pd.is_invocant || pd.named || pd.slurpy || pd.double_slurpy {
                continue;
            }
            let Some(tc) = pd.type_constraint.as_deref() else {
                continue;
            };
            let base = Self::constraint_base_for_distance(tc);
            if base == "Any" || base == "Mu" {
                continue;
            }
            let resolved = if self.registry().subsets.contains_key(base) {
                self.resolve_subset_base_type(base)
            } else {
                base.to_string()
            };
            total += self.class_mro(&resolved).len();
        }
        total
    }

    /// Rank `block` in place by nominal narrowness (descending), then MRO depth (ascending —
    /// more-derived first), stably preserving each entry's incoming relative order (declaration
    /// order within one owner, or the already-ranked order of a merged-in ancestor block) as the
    /// final tie-break.
    fn rank_deferral_block(
        &mut self,
        mro_depth: &HashMap<Symbol, usize>,
        block: &mut [(Symbol, MethodDef)],
    ) {
        let scored: Vec<(usize, usize)> = block
            .iter()
            .map(|(owner, def)| {
                (
                    self.nominal_positional_narrowness(def),
                    *mro_depth.get(owner).unwrap_or(&usize::MAX),
                )
            })
            .collect();
        let mut indexed: Vec<usize> = (0..block.len()).collect();
        indexed.sort_by(|&a, &b| {
            scored[b]
                .0
                .cmp(&scored[a].0)
                .then(scored[a].1.cmp(&scored[b].1))
        });
        let reordered: Vec<(Symbol, MethodDef)> =
            indexed.into_iter().map(|i| block[i].clone()).collect();
        block.clone_from_slice(&reordered);
    }

    /// Build the flat deferral expansion for `(class_name, method_name)` — see the module doc.
    /// Structural (no argument filtering): the caller matches this against a specific call's
    /// `arg_values` exactly as it did `resolve_all_methods_with_owner`'s output.
    pub(crate) fn resolve_deferral_expansion(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Vec<(Symbol, MethodDef)> {
        let mro = self.class_mro(class_name);
        let mro_depth: HashMap<Symbol, usize> =
            mro.iter().enumerate().map(|(idx, s)| (*s, idx)).collect();
        // Pass 1: bottom-up (nearest-ancestor-first among those still to visit), so a level's
        // own governing block can merge in an already-computed ancestor block.
        let mut governing: HashMap<Symbol, Vec<(Symbol, MethodDef)>> = HashMap::new();
        for (idx, owner) in mro.iter().enumerate().rev() {
            let is_ancestor = idx > 0;
            let owner_str = owner.as_str();
            let own_multis: Vec<MethodDef> = self
                .own_overloads_at_level(owner_str, method_name)
                .into_iter()
                .flatten()
                .filter(|d| d.is_multi && !d.is_private && !(d.is_my && is_ancestor))
                .collect();
            let explicit_proto = self
                .registry()
                .method_entry_proto(owner_str, method_name)
                .is_some();
            if own_multis.is_empty() && !explicit_proto {
                continue;
            }
            let mut block: Vec<(Symbol, MethodDef)> =
                own_multis.into_iter().map(|d| (*owner, d)).collect();
            if !explicit_proto
                && let Some(ancestor_block) = mro[idx + 1..].iter().find_map(|a| governing.get(a))
            {
                block.extend(ancestor_block.iter().cloned());
            }
            self.rank_deferral_block(&mro_depth, &mut block);
            governing.insert(*owner, block);
        }
        // Pass 2: top-down (receiver first), concatenating each level's contribution.
        let mut expansion = Vec::new();
        for (idx, owner) in mro.iter().enumerate() {
            let is_ancestor = idx > 0;
            let owner_str = owner.as_str();
            let Some(overloads) = self.own_overloads_at_level(owner_str, method_name) else {
                continue;
            };
            if overloads.iter().any(|d| d.is_multi) {
                if let Some(block) = governing.get(owner) {
                    expansion.extend(block.iter().cloned());
                }
            } else {
                for def in overloads {
                    if def.is_private || (def.is_my && is_ancestor) {
                        continue;
                    }
                    expansion.push((*owner, def));
                }
            }
        }
        self.drop_flattened_role_duplicates(&mut expansion);
        expansion
    }
}
