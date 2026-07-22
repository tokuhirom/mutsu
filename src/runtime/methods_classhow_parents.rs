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
                .any(|a| matches!(a.view(), ValueView::Pair(k, v) if k == name && v.truthy()))
        };
        let local = has_flag("local");
        let all = has_flag("all");
        let tree = has_flag("tree");
        let class_name = match args[0].view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => value_type_name(&args[0]).to_string(),
        };
        if tree {
            // `.^parents(:tree)` returns the parent tree. A single direct parent
            // yields that parent's subtree directly (`[X, [Any, [Mu]]]`); multiple
            // direct parents yield a List of subtrees
            // (`([P1, ...], [P2, ...])`). `:all` does not change the tree.
            let parents = self.tree_effective_parents(&class_name);
            let subtrees: Vec<Value> = parents.iter().map(|p| self.parent_node_tree(p)).collect();
            return Ok(match subtrees.len() {
                1 => subtrees.into_iter().next().unwrap(),
                _ => Value::array(subtrees),
            });
        }
        let mro = self.classhow_mro_names(&args[0]);
        let parents: Vec<Value> = if local {
            // `:local` returns the immediately-declared parent class(es), with
            // composed roles excluded. A user class carries its explicit parents
            // in the registry (handles multiple inheritance); a class with no
            // explicit superclass — or a built-in type — falls back to the first
            // non-role entry of the MRO (`Any` for a bare class, `Cool` for Int).
            let registered: Vec<String> = self
                .registry()
                .classes
                .get(&class_name)
                .map(|cd| cd.parents.clone())
                .unwrap_or_default()
                .into_iter()
                .filter(|p| !self.parent_is_role(p))
                .collect();
            let locals = if registered.is_empty() {
                mro.iter()
                    .skip(1)
                    .find(|p| *p != &class_name && !self.parent_is_role(p))
                    .cloned()
                    .into_iter()
                    .collect()
            } else {
                registered
            };
            locals
                .into_iter()
                .map(|p| Value::package(Symbol::intern(&p)))
                .collect()
        } else if all {
            // `:all` walks the full MRO but still excludes composed roles.
            mro.into_iter()
                .skip(1)
                .filter(|p| !self.parent_is_role(p))
                .map(|p| Value::package(Symbol::intern(&p)))
                .collect()
        } else {
            // The default form stops at Cool/Any/Mu and drops composed roles.
            mro.into_iter()
                .skip(1)
                .filter(|p| p != "Any" && p != "Mu" && p != "Cool" && !self.parent_is_role(p))
                .map(|p| Value::package(Symbol::intern(&p)))
                .collect()
        };
        // `.^parents` returns a List in raku (`.WHAT` is `(List)`, gists `(...)`),
        // not an Array — matching `.^roles`/`.^mro`, which already build a List.
        Ok(Value::array(parents))
    }

    /// Whether `name` names a role (parameterization stripped), so it should be
    /// excluded from a class's parent list — `.^parents` reports parent classes
    /// only, never composed roles.
    fn parent_is_role(&self, name: &str) -> bool {
        let base = name.split_once('[').map(|(b, _)| b).unwrap_or(name);
        self.is_role(base)
    }

    /// The direct parents used to build a `:tree`, filling in the implicit
    /// `Any`/`Mu` chain. A base class (no explicit parent) inherits `Any`;
    /// `Any` inherits `Mu`; `Mu` is the root (no parents). Composed roles are
    /// excluded — only parent classes participate in the tree.
    fn tree_effective_parents(&self, class_name: &str) -> Vec<String> {
        let registered: Vec<String> = self
            .registry()
            .classes
            .get(class_name)
            .map(|cd| cd.parents.clone())
            .unwrap_or_default()
            .into_iter()
            .filter(|p| !self.parent_is_role(p))
            .collect();
        if !registered.is_empty() {
            return registered;
        }
        match class_name {
            "Mu" => Vec::new(),
            "Any" => vec!["Mu".to_string()],
            _ => vec!["Any".to_string()],
        }
    }

    /// The subtree for a single node: `[node]` for a leaf (`Mu`), `[node, sub]`
    /// for one parent, and `[node, (sub1, sub2, ...)]` (children in a List) for
    /// multiple parents — matching Rakudo's `.^parents(:tree)` shape.
    fn parent_node_tree(&self, class_name: &str) -> Value {
        let node = Value::package(Symbol::intern(class_name));
        let parents = self.tree_effective_parents(class_name);
        if parents.is_empty() {
            return Value::real_array(vec![node]);
        }
        let children: Vec<Value> = parents.iter().map(|p| self.parent_node_tree(p)).collect();
        let entry = if children.len() == 1 {
            vec![node, children.into_iter().next().unwrap()]
        } else {
            vec![node, Value::array(children)]
        };
        Value::real_array(entry)
    }

    /// Whether the registered class `class_name` composes `role` (transitively,
    /// including roles reached through composed roles). The role name is matched
    /// on its base (parameterization stripped), so `Hash[Int]` matches `Hash`.
    pub(crate) fn class_does_role(&self, class_name: &str, role: &str) -> bool {
        if class_name == role {
            return true;
        }
        self.collect_roles_for_class(class_name, false, false, false)
            .iter()
            .any(|r| r.split_once('[').map(|(b, _)| b).unwrap_or(r) == role)
    }

    pub(crate) fn dispatch_classhow_roles(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Detect whether the invocant is an instance (Mixin or Instance) vs a
        // type object (Package). For punned role instances, the role itself
        // should be included in the result.
        let is_instance = matches!(
            args[0].view(),
            ValueView::Instance { .. } | ValueView::Mixin(_, _)
        );
        let class_name = match args[0].view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Mixin(inner, _) => match inner.as_ref().view() {
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => value_type_name(&args[0]).to_string(),
            },
            _ => value_type_name(&args[0]).to_string(),
        };
        let local = args[1..]
            .iter()
            .any(|a| matches!(a.view(), ValueView::Pair(k, v) if k == "local" && v.truthy()));
        let non_transitive = args[1..]
            .iter()
            .any(|a| matches!(a.view(), ValueView::Pair(k, v) if k == "transitive" && !v.truthy()));
        let roles = self.collect_roles_for_class(&class_name, local, non_transitive, is_instance);
        Ok(Value::array(
            roles
                .into_iter()
                .map(|r| Value::package(Symbol::intern(&r)))
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
                [crate::symbol::Symbol::intern(class_name)].into()
            };
            for cn in mro.iter().map(|s| s.as_str()) {
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
