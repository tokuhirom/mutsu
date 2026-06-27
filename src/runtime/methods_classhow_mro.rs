use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn classhow_mro_names(&mut self, invocant: &Value) -> Vec<String> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        let mut mro = if self.registry().classes.contains_key(&class_name) {
            self.class_mro(&class_name)
        } else {
            // Built-in type hierarchies for types that are not user-defined classes.
            // Any/Mu are appended unconditionally below, so we only list the chain
            // up to (but not including) Any here.
            let builtin: &[&str] = match class_name.as_str() {
                "Int" => &["Int", "Cool"],
                "Num" => &["Num", "Cool"],
                "Rat" | "FatRat" => &["Rat", "Cool"],
                "Complex" => &["Complex", "Cool"],
                "Str" => &["Str", "Cool"],
                "Bool" => &["Bool", "Int", "Cool"],
                "Array" => &["Array", "List", "Cool"],
                "List" => &["List", "Cool"],
                "Hash" => &["Hash", "Map", "Cool"],
                "Map" => &["Map", "Cool"],
                "Range" => &["Range", "Cool"],
                "Seq" => &["Seq", "Cool"],
                "Pair" => &["Pair"],
                _ => &[],
            };
            if builtin.is_empty() {
                vec![class_name.clone()]
            } else {
                builtin.iter().map(|s| s.to_string()).collect()
            }
        };
        if self.package_looks_like_grammar(&class_name) {
            for parent in ["Grammar", "Match", "Capture", "Cool", "Any", "Mu"] {
                if !mro.iter().any(|name| name == parent) {
                    mro.push(parent.to_string());
                }
            }
            return mro;
        }
        // Only append "Any" if the computed MRO doesn't already include it AND
        // the class does not explicitly bypass Any by inheriting directly from Mu.
        let has_any_in_mro = mro.iter().any(|name| name == "Any");
        let has_mu_in_mro = mro.iter().any(|name| name == "Mu");
        if class_name != "Mu" && !has_any_in_mro && !has_mu_in_mro {
            mro.push("Any".to_string());
        }
        if !has_mu_in_mro {
            mro.push("Mu".to_string());
        }
        mro
    }

    /// Build MRO with roles interleaved (for :roles or :concretizations).
    /// For each class in the MRO, insert its composed roles right after it.
    /// For :roles mode, use base role names. For :concretizations, use as-is.
    pub(super) fn classhow_mro_with_roles(
        &mut self,
        invocant: &Value,
        _concretizations: bool,
    ) -> Vec<Value> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        let base_mro = self.classhow_mro_names(invocant);
        let mut result: Vec<Value> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        for entry in &base_mro {
            // Check if this entry is a role (in the parents list because of `does`)
            let base_entry = entry
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(entry.as_str());
            if self.registry().roles.contains_key(base_entry)
                && entry != "Any"
                && entry != "Mu"
                && entry != &class_name
            {
                // This is a role in the class's parent list - include it with base name
                if seen.insert(base_entry.to_string()) {
                    result.push(Value::Package(Symbol::intern(base_entry)));
                    // Also add the role's parent classes that aren't already in base MRO
                    self.add_role_parents_to_mro(base_entry, &base_mro, &mut result, &mut seen);
                }
            } else {
                // This is a class
                if seen.insert(entry.clone()) {
                    result.push(Value::Package(Symbol::intern(entry)));
                    // Insert composed roles for this class
                    let composed = self
                        .registry()
                        .class_composed_roles
                        .get(entry)
                        .cloned()
                        .unwrap_or_default();
                    for role_name in &composed {
                        let base_role = role_name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(role_name.as_str());
                        if seen.insert(base_role.to_string()) {
                            result.push(Value::Package(Symbol::intern(base_role)));
                            // Add role's sub-roles (from `does` inside the role)
                            self.add_role_parents_to_mro(
                                base_role,
                                &base_mro,
                                &mut result,
                                &mut seen,
                            );
                        }
                    }
                }
            }
        }
        result
    }

    /// Add a role's parent roles/classes to the MRO result.
    pub(super) fn add_role_parents_to_mro(
        &self,
        role_name: &str,
        _base_mro: &[String],
        result: &mut Vec<Value>,
        seen: &mut HashSet<String>,
    ) {
        if let Some(parents) = self.registry().role_parents.get(role_name) {
            for parent in parents {
                let base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.registry().roles.contains_key(base) && seen.insert(base.to_string()) {
                    result.push(Value::Package(Symbol::intern(base)));
                    self.add_role_parents_to_mro(base, _base_mro, result, seen);
                }
                // Parent classes from roles are handled by the class MRO
            }
        }
    }

    /// Filter MRO to remove hidden classes and their associated roles.
    pub(super) fn filter_mro_unhidden(&self, invocant: &Value, mro: Vec<Value>) -> Vec<Value> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        // Collect hidden parent names for this class
        let hidden_parents: HashSet<String> = self
            .registry()
            .hidden_defer_parents
            .get(&class_name)
            .cloned()
            .unwrap_or_default();
        // Also collect classes marked `is hidden`
        let mut hidden_set: HashSet<String> = HashSet::new();
        for hp in &hidden_parents {
            hidden_set.insert(hp.clone());
        }
        for hc in &self.registry().hidden_classes {
            hidden_set.insert(hc.clone());
        }
        if hidden_set.is_empty() {
            return mro;
        }
        // Build set of all entries to hide: hidden classes + their composed roles
        let mut to_hide: HashSet<String> = HashSet::new();
        for hidden in &hidden_set {
            to_hide.insert(hidden.clone());
            // Also add the base name (strip type params)
            let hidden_base = hidden
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(hidden.as_str());
            to_hide.insert(hidden_base.to_string());
            // Also hide roles composed by the hidden class (try both full and base name)
            let composed_full = self
                .registry()
                .class_composed_roles
                .get(hidden.as_str())
                .cloned();
            let composed_base = self
                .registry()
                .class_composed_roles
                .get(hidden_base)
                .cloned();
            let composed = composed_full.or(composed_base).unwrap_or_default();
            for role in &composed {
                let base = role
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role.as_str());
                to_hide.insert(base.to_string());
                // And roles composed by those roles
                self.collect_hidden_roles(base, &mut to_hide);
            }
            // Also check role_parents for the hidden entry (in case it's a punned role)
            self.collect_hidden_roles(hidden_base, &mut to_hide);
        }
        mro.into_iter()
            .filter(|v| {
                if let Value::Package(name) = v {
                    !to_hide.contains(&name.resolve())
                } else {
                    true
                }
            })
            .collect()
    }

    pub(super) fn collect_hidden_roles(&self, role_name: &str, to_hide: &mut HashSet<String>) {
        if let Some(parents) = self.registry().role_parents.get(role_name) {
            for parent in parents {
                let base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.registry().roles.contains_key(base) && to_hide.insert(base.to_string()) {
                    self.collect_hidden_roles(base, to_hide);
                }
            }
        }
    }

    /// MRO without hidden classes (no roles)
    pub(super) fn classhow_mro_unhidden_names(&mut self, invocant: &Value) -> Vec<String> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        let mro = self.classhow_mro_names(invocant);
        let hidden_parents: HashSet<String> = self
            .registry()
            .hidden_defer_parents
            .get(&class_name)
            .cloned()
            .unwrap_or_default();
        let mut hidden_set: HashSet<String> = HashSet::new();
        for hp in &hidden_parents {
            hidden_set.insert(hp.clone());
        }
        for hc in &self.registry().hidden_classes {
            hidden_set.insert(hc.clone());
        }
        if hidden_set.is_empty() {
            return mro;
        }
        mro.into_iter()
            .filter(|name| !hidden_set.contains(name))
            .collect()
    }

    pub(super) fn package_looks_like_grammar(&self, package_name: &str) -> bool {
        let prefix = format!("{package_name}::");
        self.registry()
            .token_defs
            .keys()
            .any(|key| key.resolve().starts_with(&prefix))
    }
}
