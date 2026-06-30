use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn classhow_lookup(&self, invocant: &Value, method_name: &str) -> Option<Value> {
        let (class_name, class_name_str) = match invocant {
            Value::Package(name) => (*name, name.resolve()),
            other => {
                // For concrete values, derive the type name
                let type_name = crate::runtime::utils::value_type_name(other).to_string();
                (Symbol::intern(&type_name), type_name)
            }
        };
        // Check user-defined class methods first
        if let Some(class_def) = self.registry().classes.get(&class_name_str)
            && let Some(defs) = class_def.methods.get(method_name)
            && let Some(def) = defs.first()
        {
            let has_multi = defs.iter().any(|d| d.is_multi);
            let has_explicit_invocant = def
                .param_defs
                .iter()
                .any(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"));
            let mut full_param_defs = Vec::with_capacity(def.param_defs.len() + 1);
            if !has_explicit_invocant {
                full_param_defs.push(Self::make_invocant_param(&class_name_str));
            }
            full_param_defs.extend(def.param_defs.iter().cloned());
            let mut env = crate::env::Env::new();
            // Set callable type for .^name to return Method/Submethod
            let callable_type = if def.is_submethod {
                "Submethod"
            } else {
                "Method"
            };
            env.insert(
                "__mutsu_callable_type".to_string(),
                Value::str_from(callable_type),
            );
            // Carry the declared return type so `.signature.returns` / `.returns`
            // reflect a `--> Type` declaration on the method.
            if let Some(rt) = &def.return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            env.insert(
                "__mutsu_lookup_class".to_string(),
                Value::str(class_name_str.clone()),
            );
            env.insert(
                "__mutsu_lookup_method".to_string(),
                Value::str(method_name.to_string()),
            );
            // A single (non-multi) method is candidate 0; recording it lets
            // `.wrap()` register a method-level wrap chain that participates in
            // MRO `nextsame` dispatch (S06-advanced/wrap.t GH#2178). Multi methods
            // are wrapped via the `.candidates[N]` path, which carries its own idx.
            if !has_multi {
                env.insert("__mutsu_lookup_candidate_idx".to_string(), Value::Int(0));
            }
            return Some(Value::make_sub(
                class_name,
                Symbol::intern(method_name),
                def.params.clone(),
                full_param_defs,
                (*def.body).clone(),
                def.is_rw,
                env,
            ));
        }
        // Check role methods
        if let Some(role_def) = self.registry().roles.get(&class_name_str)
            && let Some(defs) = role_def.methods.get(method_name)
            && let Some(def) = defs.first()
        {
            let has_multi = defs.iter().any(|d| d.is_multi);
            let has_explicit_invocant = def
                .param_defs
                .iter()
                .any(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"));
            let mut full_param_defs = Vec::with_capacity(def.param_defs.len() + 1);
            if !has_explicit_invocant {
                full_param_defs.push(Self::make_invocant_param(&class_name_str));
            }
            full_param_defs.extend(def.param_defs.iter().cloned());
            let mut env = crate::env::Env::new();
            // Set callable type for .^name to return Method/Submethod
            let callable_type = if def.is_submethod {
                "Submethod"
            } else {
                "Method"
            };
            env.insert(
                "__mutsu_callable_type".to_string(),
                Value::str_from(callable_type),
            );
            // Carry the declared return type so `.signature.returns` / `.returns`
            // reflect a `--> Type` declaration on the role method.
            if let Some(rt) = &def.return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if has_multi {
                env.insert(
                    "__mutsu_lookup_class".to_string(),
                    Value::str(class_name_str.clone()),
                );
                env.insert(
                    "__mutsu_lookup_method".to_string(),
                    Value::str(method_name.to_string()),
                );
            }
            return Some(Value::make_sub(
                class_name,
                Symbol::intern(method_name),
                def.params.clone(),
                full_param_defs,
                (*def.body).clone(),
                def.is_rw,
                env,
            ));
        }
        // Check auto-generated accessor methods for public attributes (has $.x, has $.x is rw).
        // These are not stored in class_def.methods but are generated on-the-fly.
        // ClassAttributeDef: (attr_name, is_public, default, is_rw, is_required, sigil, where)
        if let Some(class_def) = self.registry().classes.get(&class_name_str) {
            for (attr_name, is_public, _, is_rw, _, _, _) in &class_def.attributes {
                if *is_public && attr_name == method_name {
                    let mut env = crate::env::Env::new();
                    env.insert(
                        "__mutsu_callable_type".to_string(),
                        Value::str_from("Method"),
                    );
                    return Some(Value::make_sub(
                        class_name,
                        Symbol::intern(method_name),
                        vec!["self".to_string()],
                        vec![Self::make_invocant_param(&class_name_str)],
                        vec![],
                        *is_rw,
                        env,
                    ));
                }
            }
        }
        // Check grammar token/rule/regex definitions
        let token_key = format!("{}::{}", class_name_str, method_name);
        if let Some(defs) = self.registry().token_defs.get(&Symbol::intern(&token_key))
            && !defs.is_empty()
        {
            return Some(Value::Routine {
                package: class_name,
                name: Symbol::intern(method_name),
                is_regex: true,
            });
        }
        // Check built-in type methods — return a Routine marker that the
        // runtime can dispatch when called.
        if self.is_builtin_type_method(&class_name_str, method_name) {
            return Some(Value::Routine {
                package: class_name,
                name: Symbol::intern(method_name),
                is_regex: false,
            });
        }
        None
    }

    fn make_invocant_param(class_name: &str) -> crate::ast::ParamDef {
        crate::ast::ParamDef {
            name: String::new(),
            default: None,
            multi_invocant: true,
            required: false,
            named: false,
            slurpy: false,
            double_slurpy: false,
            onearg: false,
            sigilless: false,
            type_constraint: Some(class_name.to_string()),
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            is_invocant: true,
            traits: Vec::new(),
            optional_marker: false,
            outer_sub_signature: None,
            code_signature: None,
            shape_constraints: None,
        }
    }

    /// Return all multi method candidates for a class method as Sub values.
    pub(super) fn classhow_lookup_all_candidates(
        &self,
        class_name: &str,
        method_name: &str,
        package: crate::symbol::Symbol,
    ) -> Vec<Value> {
        // No user-code re-entry below (pure Value construction), so a let-bound
        // guard is safe.
        let registry = self.registry();
        let Some(class_def) = registry.classes.get(class_name) else {
            return Vec::new();
        };
        let Some(defs) = class_def.methods.get(method_name) else {
            return Vec::new();
        };
        defs.iter()
            .enumerate()
            .map(|(idx, def)| {
                let mut full_param_defs = vec![Self::make_invocant_param(class_name)];
                full_param_defs.extend(def.param_defs.iter().cloned());
                let mut env = crate::env::Env::new();
                // Set callable type for .^name to return Method/Submethod
                let callable_type = if def.is_submethod {
                    "Submethod"
                } else {
                    "Method"
                };
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from(callable_type),
                );
                env.insert(
                    "__mutsu_lookup_class".to_string(),
                    Value::str(class_name.to_string()),
                );
                env.insert(
                    "__mutsu_lookup_method".to_string(),
                    Value::str(method_name.to_string()),
                );
                env.insert(
                    "__mutsu_lookup_candidate_idx".to_string(),
                    Value::Int(idx as i64),
                );
                Value::make_sub(
                    package,
                    crate::symbol::Symbol::intern(method_name),
                    def.params.clone(),
                    full_param_defs,
                    (*def.body).clone(),
                    def.is_rw,
                    env,
                )
            })
            .collect()
    }

    /// Check if a method name belongs to a built-in type (Str, Int, etc.)
    /// by checking the hardcoded method lists for the type and its ancestors.
    fn is_builtin_type_method(&self, type_name: &str, method_name: &str) -> bool {
        // Check the type itself and common ancestors (Cool, Any, Mu)
        for tn in &[type_name, "Cool", "Any", "Mu"] {
            let mut methods = Vec::new();
            self.collect_builtin_type_methods(tn, &mut methods);
            if methods.iter().any(|m| m.to_string_value() == method_name) {
                return true;
            }
        }
        false
    }

    pub(super) fn classhow_find_method(
        &self,
        invocant: &Value,
        method_name: &str,
    ) -> Option<Value> {
        if matches!(
            method_name,
            "name"
                | "ver"
                | "auth"
                | "mro"
                | "mro_unhidden"
                | "archetypes"
                | "isa"
                | "can"
                | "does"
                | "lookup"
                | "find_method"
                | "add_attribute"
                | "add_method"
                | "add_multi_method"
                | "compose"
                | "methods"
                | "attributes"
                | "parents"
                | "roles"
                | "concretization"
                | "curried_role"
                | "pun"
                | "language-revision"
                | "submethod_table"
        ) {
            return Some(Value::str(method_name.to_string()));
        }
        if let Some(value) = self.classhow_lookup(invocant, method_name) {
            return Some(value);
        }
        // CREATE is a built-in method on all types
        if method_name == "CREATE" {
            return Some(Value::Routine {
                package: Symbol::intern("Mu"),
                name: Symbol::intern("CREATE"),
                is_regex: false,
            });
        }
        if let Value::Package(class_name) = invocant
            && let Some(class_def) = self.registry().classes.get(&class_name.resolve())
            && class_def.native_methods.contains(method_name)
        {
            return Some(Value::str(method_name.to_string()));
        }
        None
    }
}
