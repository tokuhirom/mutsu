//! Named phase of `register_class_decl` (ADR-0019 D0): recording the
//! composed-role lists on the registry and propagating role parent classes
//! and `hides` declarations onto the class. Split out of
//! `registration_class_compose.rs` to keep that file under the repo's
//! 500-line-per-file convention.

use super::*;

/// A class's flattened role closure with the segment of each DIRECT
/// composition moved to the front, in reverse marker order.
///
/// The flattened list is built role by role, so it reads
/// `[d1, tail(d1)..., d2, tail(d2)...]` -- the direct entries are its segment
/// markers. Reordering by segment (rather than reversing the whole list, or
/// re-deriving reachability from `role_parents`) is what keeps a role reached
/// THROUGH another one after it: `role RB does RA; class KN does RB` stays
/// `(RB, RA)`, while `class K does R1 does R2` becomes `(R2, R1)`. Anything
/// ahead of the first marker keeps its place.
///
/// The transform is its own inverse, which is what lets the one consumer that
/// needs *composition* order back -- `ordered_role_submethods_for_class`, the
/// `BUILD`/`TWEAK`/`DESTROY` walk -- recover it from the recorded list by
/// applying the same function again.
pub(crate) fn role_closure_segments_reversed(
    flattened: &[String],
    direct: &[String],
) -> Vec<String> {
    if direct.len() < 2 {
        return flattened.to_vec();
    }
    let mut segments: Vec<Vec<String>> = vec![Vec::new()];
    // A direct role only opens a new segment the first time it is seen, so a
    // duplicate composition (`does R does R`) cannot split the list into more
    // segments than there are markers to reverse.
    let mut opened: HashSet<&str> = HashSet::new();
    for role in flattened {
        if direct.iter().any(|d| d == role) && opened.insert(role.as_str()) {
            segments.push(Vec::new());
        }
        segments.last_mut().expect("never empty").push(role.clone());
    }
    let mut out = segments.remove(0);
    for segment in segments.into_iter().rev() {
        out.extend(segment);
    }
    out
}

impl Interpreter {
    /// Record the composed-role lists on the registry and propagate role
    /// parent classes and `hides` declarations (recursively through
    /// sub-roles) onto the class.
    pub(super) fn record_class_composed_roles(
        &mut self,
        name: &str,
        class_def: &mut ClassDef,
        composed_roles_list: &[String],
        direct_composed_roles: &[String],
    ) {
        // raku reports a class's own compositions LAST-DECLARED-FIRST --
        // `class K does R1 does R2` is `(R2, R1)` for `.^roles`,
        // `.^roles(:!transitive)` and `.^mro(:roles)` alike, the same last-wins
        // rule a `but`-mixed role follows. Rakudo keeps two lists for this:
        // `add_role` UNSHIFTS onto the `@!roles` these registry rows correspond
        // to, while composition walks a separately pushed
        // `@!roles_to_compose`. So the recorded order is flipped here, at the
        // one place both lists are written -- every reader then agrees, and the
        // built-in seeds, which `runtime_init` writes straight into the
        // registry already in report order, are not touched. The one consumer
        // that needs composition order back (`BUILD`/`TWEAK`/`DESTROY`, via
        // `ordered_role_submethods_for_class`) recovers it by applying the same
        // self-inverse transform.
        let composed_roles_list =
            role_closure_segments_reversed(composed_roles_list, direct_composed_roles);
        let direct_composed_roles: Vec<String> =
            direct_composed_roles.iter().rev().cloned().collect();
        let composed_roles_list = &composed_roles_list[..];
        let direct_composed_roles = &direct_composed_roles[..];
        // Clear stale composed roles from previous registration
        self.registry_mut().class_composed_roles.remove(name);
        if !composed_roles_list.is_empty() {
            // Propagate role parent classes to the class (recursively through sub-roles)
            // When a role `R is C1` is composed into a class, C1 becomes a parent
            {
                let mut role_stack: Vec<String> = composed_roles_list
                    .iter()
                    .map(|r| {
                        r.split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(r.as_str())
                            .to_string()
                    })
                    .collect();
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(rparents) = self.registry().role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.is_role_type_name(rp_base) {
                                // It's a sub-role, recurse
                                role_stack.push(rp_base.to_string());
                            } else if self.registry().classes.contains_key(rp_base)
                                && !class_def.parents.contains(&rp)
                            {
                                class_def.parents.push(rp);
                            }
                        }
                    }
                }
            }
            self.registry_mut()
                .class_composed_roles
                .insert(name.to_string(), composed_roles_list.to_vec());
            self.registry_mut()
                .class_direct_composed_roles
                .insert(name.to_string(), direct_composed_roles.to_vec());
            // Propagate `hides` from composed roles (and sub-roles) to the class
            {
                let mut role_stack: Vec<String> = composed_roles_list
                    .iter()
                    .map(|r| {
                        r.split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(r.as_str())
                            .to_string()
                    })
                    .collect();
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    // Hoist the clone to a `let` so the read guard drops before the
                    // registry_mut write below (read->write on the same lock deadlocks).
                    let hides_list = self.registry().role_hides.get(&role_name).cloned();
                    if let Some(hides_list) = hides_list {
                        for hidden in hides_list {
                            self.registry_mut()
                                .hidden_defer_parents
                                .entry(name.to_string())
                                .or_default()
                                .insert(hidden);
                        }
                    }
                    // Recurse into sub-roles
                    if let Some(rparents) = self.registry().role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.is_role_type_name(rp_base) {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
        }
    }
}
