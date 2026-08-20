//! Named phase of `register_class_decl` (ADR-0019 D0): recording the
//! composed-role lists on the registry and propagating role parent classes
//! and `hides` declarations onto the class. Split out of
//! `registration_class_compose.rs` to keep that file under the repo's
//! 500-line-per-file convention.

use super::*;

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
                            if self.registry().roles.contains_key(rp_base) {
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
                            if self.registry().roles.contains_key(rp_base) {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
        }
    }
}
