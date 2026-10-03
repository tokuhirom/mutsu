//! Ordering a class's role-provided construction submethods (`BUILD`,
//! `TWEAK`, `DESTROY`): the order the roles were composed in, parents first,
//! taken from the candidate (parametric or plain) each composition used.

use super::*;

impl Interpreter {
    /// For a given class, return the ordered list of (role_name, MethodDef) pairs
    /// for role submethods with the given name (e.g. "BUILD", "TWEAK", "DESTROY").
    /// The order respects role composition: sub-roles come before the role that
    /// composes them. Only submethods (is_my == true) are included; regular methods
    /// in roles are skipped.
    pub(super) fn ordered_role_submethods_for_class(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Vec<(String, MethodDef)> {
        // Initializers run in COMPOSITION order, not in the last-declared-first
        // order `class_composed_roles` records for `.^roles` (rakudo keeps the
        // two apart as `@!roles_to_compose` vs `@!roles`): for
        // `class C1 does R1 does R2`, `R1.BUILD` runs before `R2.BUILD` even
        // though `.^roles` is `(R2, R1)`. `role_closure_segments_reversed` is
        // its own inverse, so re-applying it to the recorded list -- with the
        // recorded (also reversed) direct list as its markers -- hands back the
        // declaration order composition used.
        let registry = self.registry();
        let composed = match registry.class_composed_roles.get(class_name) {
            Some(roles) => {
                crate::runtime::registration_class_compose_record::role_closure_segments_reversed(
                    roles,
                    registry
                        .class_direct_composed_roles
                        .get(class_name)
                        .map_or(&[][..], |d| &d[..]),
                )
            }
            None => return Vec::new(),
        };
        drop(registry);
        // Build the correct order: for each directly composed role (in order),
        // recursively include parent roles (depth-first) before the role itself.
        // Deduplicate to avoid calling the same role submethod twice for the same class.
        let mut ordered = Vec::new();
        let mut seen = std::collections::HashSet::new();
        // Figure out which roles are "direct" (from `does` declarations) vs transitive.
        // Direct roles are those not reachable through another direct role's parents.
        // However, `class_composed_roles` includes both direct and transitive roles in
        // an unspecified order. We need to reconstruct the proper depth-first order.
        //
        // Strategy: for each role in composed list, expand it depth-first (parents first).
        // The composed list may have the order [R1, R0, R2] where R0 is a parent of R1.
        // We want [R0, R1, R2]. We achieve this by expanding each role and skipping
        // already-seen roles.
        for role in &composed {
            let role_base = role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(role.as_str());
            self.expand_role_depth_first(role_base, &mut ordered, &mut seen);
        }
        // A parameterized role and an unparameterized one may share a name
        // (`role Some[::T] {...}` beside `role Some {...}`), and `roles` keeps
        // only one of them under it. A role composed with brackets takes its
        // submethods from the parametric candidate, one composed bare from
        // the plain candidate (Definitely: `Some[Int].new` ran the plain
        // `Some`'s TWEAK).
        let parameterized_bases: std::collections::HashSet<&str> = composed
            .iter()
            .filter_map(|role| role.split_once('[').map(|(base, _)| base))
            .collect();
        let bare_composed: std::collections::HashSet<&str> = composed
            .iter()
            .filter(|role| !role.contains('['))
            .map(String::as_str)
            .collect();
        let candidate_def = |role_name: &str| -> Option<crate::runtime::RoleDef> {
            if parameterized_bases.contains(role_name) {
                self.role_candidate_of_shape(role_name, true)
            } else if bare_composed.contains(role_name) {
                self.role_candidate_of_shape(role_name, false)
            } else {
                None
            }
        };
        // Now filter to only roles that have the requested submethod
        let mut result = Vec::new();
        for role_name in &ordered {
            let role_def =
                candidate_def(role_name).or_else(|| self.registry().roles.get(role_name).cloned());
            if let Some(role_def) = role_def
                && let Some(overloads) = role_def.methods.get(method_name)
            {
                for md in overloads {
                    if md.is_my {
                        result.push((role_name.clone(), md.clone()));
                    }
                }
            }
        }
        // A parameterized role's TWEAK runs before the TWEAKs supplied by a
        // role it composes. This is observable for model-style roles whose
        // derived TWEAK installs the state a parent TWEAK consumes. Direct
        // parameterized roles without a role parent retain declaration order;
        // that distinction matters to the versioned role-constructor roast.
        let has_parameterized_role_parent = composed.iter().any(|role| {
            let base = role.split_once('[').map_or(role.as_str(), |(base, _)| base);
            self.registry()
                .role_parents
                .get(base)
                .is_some_and(|parents| !parents.is_empty())
        });
        if method_name == "TWEAK"
            && composed.iter().any(|role| role.contains('['))
            && has_parameterized_role_parent
        {
            result.reverse();
        }
        result
    }

    /// Recursively expand a role and its parent roles in depth-first order
    /// (parent roles first, then the role itself).
    fn expand_role_depth_first(
        &self,
        role_name: &str,
        ordered: &mut Vec<String>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        if !seen.insert(role_name.to_string()) {
            return;
        }
        // First, expand parent roles
        if let Some(parents) = self.registry().role_parents.get(role_name) {
            for parent in parents {
                let parent_base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.registry().roles.contains_key(parent_base) {
                    self.expand_role_depth_first(parent_base, ordered, seen);
                }
            }
        }
        // Then add the role itself
        ordered.push(role_name.to_string());
    }
}
