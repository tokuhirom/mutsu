use super::*;

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
}
