use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn dispatch_classhow_parents(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let has_flag = |name: &str| -> bool {
            args[1..]
                .iter()
                .any(|a| matches!(a, Value::Pair(k, v) if k == name && v.truthy()))
        };
        let local = has_flag("local");
        let all = has_flag("all");
        let tree = has_flag("tree");
        let class_name = match &args[0] {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            _ => value_type_name(&args[0]).to_string(),
        };
        if tree {
            let result = self.parents_tree(&class_name);
            return Ok(Value::real_array(result));
        }
        let mro = self.classhow_mro_names(&args[0]);
        let parents_iter = mro.into_iter().skip(1);
        let parents: Vec<Value> = if local {
            self.registry()
                .classes
                .get(&class_name)
                .map(|cd| cd.parents.clone())
                .unwrap_or_default()
                .iter()
                .map(|p| Value::Package(Symbol::intern(p)))
                .collect()
        } else if all {
            parents_iter
                .map(|p| Value::Package(Symbol::intern(&p)))
                .collect()
        } else {
            parents_iter
                .filter(|p| p != "Any" && p != "Mu")
                .map(|p| Value::Package(Symbol::intern(&p)))
                .collect()
        };
        Ok(Value::real_array(parents))
    }

    fn parents_tree(&mut self, class_name: &str) -> Vec<Value> {
        let direct = self
            .registry()
            .classes
            .get(class_name)
            .map(|cd| cd.parents.clone())
            .unwrap_or_default();
        if direct.is_empty() {
            return Vec::new();
        }
        direct
            .iter()
            .map(|parent| {
                let subtree = self.parents_tree(parent);
                let mut entry = vec![Value::Package(Symbol::intern(parent))];
                if !subtree.is_empty() {
                    entry.extend(subtree);
                } else if parent != "Mu" {
                    let any_subtree = self.parents_tree("Any");
                    let mut any_entry = vec![Value::Package(Symbol::intern("Any"))];
                    if !any_subtree.is_empty() {
                        any_entry.extend(any_subtree);
                    } else {
                        any_entry.push(Value::real_array(vec![Value::Package(Symbol::intern(
                            "Mu",
                        ))]));
                    }
                    entry.push(Value::real_array(any_entry));
                }
                Value::real_array(entry)
            })
            .collect()
    }

    pub(crate) fn dispatch_classhow_roles(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Detect whether the invocant is an instance (Mixin or Instance) vs a
        // type object (Package). For punned role instances, the role itself
        // should be included in the result.
        let is_instance = matches!(&args[0], Value::Instance { .. } | Value::Mixin(_, _));
        let class_name = match &args[0] {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            Value::Mixin(inner, _) => match inner.as_ref() {
                Value::Instance { class_name, .. } => class_name.resolve(),
                _ => value_type_name(&args[0]).to_string(),
            },
            _ => value_type_name(&args[0]).to_string(),
        };
        let local = args[1..]
            .iter()
            .any(|a| matches!(a, Value::Pair(k, v) if k == "local" && v.truthy()));
        let non_transitive = args[1..]
            .iter()
            .any(|a| matches!(a, Value::Pair(k, v) if k == "transitive" && !v.truthy()));
        let roles = self.collect_roles_for_class(&class_name, local, non_transitive, is_instance);
        Ok(Value::array(
            roles
                .into_iter()
                .map(|r| Value::Package(Symbol::intern(&r)))
                .collect(),
        ))
    }

    fn collect_roles_for_class(
        &self,
        class_name: &str,
        local_only: bool,
        non_transitive: bool,
        is_instance: bool,
    ) -> Vec<String> {
        // If the target is a role (not a class), return its role_parents.
        // For instances (punned roles), include the role itself in the list.
        let is_role = self.registry().roles.contains_key(class_name)
            && !self.registry().classes.contains_key(class_name);
        if is_role {
            let mut result = Vec::new();
            // For instances of punned roles, include the role itself first
            if is_instance {
                result.push(class_name.to_string());
            }
            if non_transitive {
                if !is_instance && let Some(parents) = self.registry().role_parents.get(class_name)
                {
                    result.extend(parents.clone());
                }
                return result;
            }
            if let Some(parents) = self.registry().role_parents.get(class_name) {
                let mut seen: std::collections::HashSet<String> = result.iter().cloned().collect();
                for p in parents {
                    if seen.insert(p.clone()) {
                        result.push(p.clone());
                    }
                }
                let parents_clone = parents.clone();
                for p in &parents_clone {
                    self.collect_transitive_roles(p, &mut result, &mut seen);
                }
            }
            return result;
        }

        let mut result = Vec::new();
        let mut seen = std::collections::HashSet::new();
        if local_only || non_transitive {
            if let Some(roles) = self.registry().class_composed_roles.get(class_name) {
                for r in roles {
                    if seen.insert(r.clone()) {
                        result.push(r.clone());
                    }
                }
            }
            if non_transitive {
                // Filter out roles that are transitively reachable from other
                // roles in the list (i.e. keep only directly-composed roles).
                let all = result.clone();
                let mut transitive = std::collections::HashSet::new();
                for r in &all {
                    let base = r.split_once('[').map(|(b, _)| b).unwrap_or(r.as_str());
                    if let Some(parents) = self.registry().role_parents.get(base) {
                        for p in parents {
                            transitive.insert(p.clone());
                            self.collect_transitive_set(p, &mut transitive);
                        }
                    }
                }
                result.retain(|r| !transitive.contains(r));
            }
        } else {
            let mro = if let Some(cd) = self.registry().classes.get(class_name)
                && !cd.mro.is_empty()
            {
                cd.mro.clone()
            } else {
                vec![class_name.to_string()]
            };
            for cn in &mro {
                if cn == "Any" || cn == "Mu" {
                    continue;
                }
                // Clone out before the recursive call so no registry read guard is
                // held across `collect_transitive_roles` (avoids a same-thread
                // recursive read lock).
                let roles = self.registry().class_composed_roles.get(cn).cloned();
                if let Some(roles) = roles {
                    for r in &roles {
                        if seen.insert(r.clone()) {
                            result.push(r.clone());
                        }
                        self.collect_transitive_roles(r, &mut result, &mut seen);
                    }
                }
            }
        }
        result
    }

    /// Collect all transitively reachable roles into a set (for filtering).
    fn collect_transitive_set(
        &self,
        role_name: &str,
        result: &mut std::collections::HashSet<String>,
    ) {
        let base = role_name
            .split_once('[')
            .map(|(b, _)| b)
            .unwrap_or(role_name);
        if let Some(parents) = self.registry().role_parents.get(base) {
            for p in parents {
                if result.insert(p.clone()) {
                    self.collect_transitive_set(p, result);
                }
            }
        }
    }

    fn collect_transitive_roles(
        &self,
        role_name: &str,
        result: &mut Vec<String>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        let base = role_name
            .split_once('[')
            .map(|(b, _)| b)
            .unwrap_or(role_name);
        if let Some(parents) = self.registry().role_parents.get(base) {
            for p in parents {
                if seen.insert(p.clone()) {
                    result.push(p.clone());
                    self.collect_transitive_roles(p, result, seen);
                }
            }
        }
    }
}
