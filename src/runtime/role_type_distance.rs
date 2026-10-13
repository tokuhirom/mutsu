//! Multi-dispatch distance from a ROLE type object to a constraint.

use super::*;

impl Interpreter {
    /// The type-hierarchy distance from the role type object `role` (an
    /// argument such as `R` in `f(42, R)`) to the constraint `base`, or
    /// `None` when `role` is not a user role.
    ///
    /// A role type object binds where its pun would: the pun class does
    /// `role` and every role `role` does, and inherits from `Any`. So the
    /// roles it does are at their depth in the role-parent graph (1 for a
    /// direct `does`), and `Any` / `Mu` sit beyond the deepest of them -- a
    /// `P` parameter must stay narrower than an untyped one for `R does P`,
    /// as in rakudo, where the per-parameter comparison of `P` against `Any`
    /// does not depend on the argument at all. Without this, neither `P`
    /// nor `Any` was found in the role's (empty) class MRO, both scored the
    /// "unrelated" distance, and `multi m($o, P $c)` tied
    /// `multi m(Any:D $o, $t)` for `m(42, R)` (PDF's `PDF::COS::Coercer`).
    /// A role parent that is a built-in role (`Positional`) is matched by
    /// name but not walked further.
    // Cost: O(g), g = roles reachable through `role`'s role-parent graph.
    pub(super) fn role_type_object_distance(&self, role: &str, base: &str) -> Option<usize> {
        let registry = self.registry();
        if !registry.roles.contains_key(role) {
            return None;
        }
        let base = Self::role_base_name(base);
        let mut found = None;
        let mut deepest = 0usize;
        let mut layer = vec![role.to_string()];
        let mut seen = std::collections::HashSet::new();
        seen.insert(role.to_string());
        let mut depth = 0usize;
        while !layer.is_empty() {
            depth += 1;
            let mut next = Vec::new();
            for name in &layer {
                let Some(parents) = registry.role_parents.get(name) else {
                    continue;
                };
                for parent in parents {
                    let parent = Self::role_base_name(parent);
                    if !seen.insert(parent.to_string()) {
                        continue;
                    }
                    deepest = depth;
                    if parent == base && found.is_none() {
                        found = Some(depth);
                    }
                    if registry.roles.contains_key(parent) {
                        next.push(parent.to_string());
                    }
                }
            }
            layer = next;
        }
        found.or(match base {
            "Any" => Some(deepest + 1),
            "Mu" => Some(deepest + 2),
            _ => None,
        })
    }
}
