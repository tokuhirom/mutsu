use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn run_pending_instance_destroys(&mut self) -> Result<(), RuntimeError> {
        let pending = take_pending_instance_destroys();
        for item in pending {
            let Some(class_def) = self.classes.get(&item.class_name.resolve()) else {
                continue;
            };
            let Some(overloads) = class_def.methods.get("DESTROY").cloned() else {
                continue;
            };
            let Some(method_def) = overloads.into_iter().find(|def| {
                def.is_my && !def.is_private && self.method_args_match(&[], &def.param_defs)
            }) else {
                continue;
            };
            let class_name = item.class_name.resolve();
            let invocant =
                Value::make_instance_without_destroy(item.class_name, item.attributes.clone());
            let _ = self.run_instance_method_resolved(
                &class_name,
                &class_name,
                method_def,
                item.attributes,
                Vec::new(),
                Some(invocant),
            )?;
        }
        Ok(())
    }

    pub(super) fn detect_unresolved_role_method_conflicts(
        &self,
        class_name: &str,
        class_def: &ClassDef,
    ) -> Result<(), RuntimeError> {
        for (method_name, defs) in &class_def.methods {
            let class_defined = defs.iter().any(|def| def.role_origin.is_none());
            if class_defined {
                continue;
            }

            let mut conflicting_roles = Vec::new();
            for def in defs {
                if def.is_multi || Self::is_stub_routine_body(&def.body) {
                    continue;
                }
                let Some(role_name) = &def.role_origin else {
                    continue;
                };
                if !conflicting_roles.contains(role_name) {
                    conflicting_roles.push(role_name.clone());
                }
            }

            if conflicting_roles.len() > 1 {
                conflicting_roles.reverse();
                return Err(RuntimeError::new(format!(
                    "X::Role::Composition::Conflict: Method '{}' must be resolved by class {} because it exists in multiple roles ({})",
                    method_name,
                    class_name,
                    conflicting_roles.join(", "),
                )));
            }
        }

        Ok(())
    }

    pub(super) fn compute_class_mro(
        &mut self,
        class_name: &str,
        stack: &mut Vec<String>,
    ) -> Result<Vec<String>, RuntimeError> {
        if stack.iter().any(|name| name == class_name) {
            return Err(RuntimeError::new(format!(
                "C3 MRO cycle detected at {}",
                class_name
            )));
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return Ok(class_def.mro.clone());
        }
        stack.push(class_name.to_string());
        let parents = self
            .classes
            .get(class_name)
            .map(|c| c.parents.clone())
            .unwrap_or_default();
        let mut seqs: Vec<Vec<String>> = Vec::new();
        for parent in &parents {
            if self.classes.contains_key(parent) {
                let mro = self.compute_class_mro(parent, stack)?;
                seqs.push(mro);
            } else {
                seqs.push(vec![parent.clone()]);
            }
        }
        seqs.push(parents.clone());
        let mut result = vec![class_name.to_string()];
        while seqs.iter().any(|s| !s.is_empty()) {
            let mut candidate = None;
            for seq in &seqs {
                if seq.is_empty() {
                    continue;
                }
                let head = &seq[0];
                let mut in_tail = false;
                for other in &seqs {
                    if other.len() > 1 && other[1..].contains(head) {
                        in_tail = true;
                        break;
                    }
                }
                if !in_tail {
                    candidate = Some(head.clone());
                    break;
                }
            }
            if let Some(head) = candidate {
                result.push(head.clone());
                for seq in seqs.iter_mut() {
                    if !seq.is_empty() && seq[0] == head {
                        seq.remove(0);
                    }
                }
            } else {
                stack.pop();
                return Err(RuntimeError::new(format!(
                    "Inconsistent class hierarchy for {}",
                    class_name
                )));
            }
        }
        stack.pop();
        Ok(result)
    }

    pub(super) fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && (class_def.methods.contains_key(method_name)
                    || class_def.native_methods.contains(method_name))
            {
                return true;
            }
        }
        false
    }

    pub(crate) fn is_native_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && class_def.native_methods.contains(method_name)
            {
                return true;
            }
        }
        false
    }

    pub(crate) fn has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && let Some(defs) = class_def.methods.get(method_name)
            {
                return defs.iter().any(|d| !d.is_private);
            }
        }
        false
    }

    /// Check if a class has a public attribute accessor for the given name.
    pub(crate) fn has_public_accessor(&mut self, class_name: &str, method_name: &str) -> bool {
        let attrs = self.collect_class_attributes(class_name);
        attrs
            .iter()
            .any(|(attr_name, is_public, ..)| *is_public && attr_name == method_name)
    }

    /// Collect wildcard-handles attribute var names from the class and its MRO.
    pub(super) fn collect_wildcard_handles(&mut self, class_name: &str) -> Vec<String> {
        let mro = self.class_mro(class_name);
        let mut result = Vec::new();
        for cn in &mro {
            if let Some(class_def) = self.classes.get(cn) {
                result.extend(class_def.wildcard_handles.iter().cloned());
            }
        }
        result
    }

    pub(super) fn collect_class_attributes(&mut self, class_name: &str) -> Vec<ClassAttributeDef> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        for cn in mro.iter().rev() {
            if let Some(class_def) = self.classes.get(cn) {
                for attr in &class_def.attributes {
                    if let Some(pos) = attrs.iter().position(|(n, ..)| n == &attr.0) {
                        attrs.remove(pos);
                    }
                    attrs.push(attr.clone());
                }
            }
        }
        attrs
    }

    pub(crate) fn run_instance_method(
        &mut self,
        receiver_class_name: &str,
        attributes: HashMap<String, Value>,
        method_name: &str,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        let Some((owner_class, method_def)) =
            self.resolve_method_with_owner(receiver_class_name, method_name, &args)
        else {
            return Err(super::methods_signature::make_multi_no_match_error(
                method_name,
            ));
        };
        let all_candidates =
            self.resolve_all_methods_with_owner(receiver_class_name, method_name, &args);
        let invocant_for_dispatch = if let Some(inv) = &invocant {
            inv.clone()
        } else if attributes.is_empty() {
            Value::Package(Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
        };
        let remaining: Vec<(String, MethodDef)> = all_candidates
            .into_iter()
            .skip(1)
            .filter(|(candidate_owner, _)| {
                !self.should_skip_defer_method_candidate(receiver_class_name, candidate_owner)
            })
            .collect();
        let pushed_dispatch = !remaining.is_empty();
        self.samewith_context_stack
            .push((method_name.to_string(), Some(invocant_for_dispatch.clone())));
        if pushed_dispatch {
            self.method_dispatch_stack.push(MethodDispatchFrame {
                receiver_class: receiver_class_name.to_string(),
                invocant: invocant_for_dispatch,
                args: args.clone(),
                remaining,
            });
        }
        let result = self.run_instance_method_resolved(
            receiver_class_name,
            &owner_class,
            method_def,
            attributes,
            args,
            invocant,
        );
        self.samewith_context_stack.pop();
        if pushed_dispatch {
            self.method_dispatch_stack.pop();
        }
        result
    }

    pub(super) fn run_instance_method_resolved(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_def: MethodDef,
        mut attributes: HashMap<String, Value>,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        // For type-object calls (no attributes), use Package so self.new works
        let mut base = if let Some(invocant) = invocant {
            invocant
        } else if attributes.is_empty() {
            Value::Package(Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
        };
        let saved_env = self.env.clone();
        let saved_var_bindings = self.var_bindings.clone();
        let saved_readonly = self.save_readonly_vars();
        self.method_class_stack.push(owner_class.to_string());
        let role_context = if self.roles.contains_key(owner_class) {
            Some(owner_class.to_string())
        } else {
            method_def.role_origin.clone()
        };
        // Set ::?CLASS / ::?ROLE compile-time variable for the method body
        self.env.insert(
            "?CLASS".to_string(),
            Value::Package(Symbol::intern(owner_class)),
        );
        if let Some(role_name) = role_context {
            self.env.insert(
                "?ROLE".to_string(),
                Value::Package(Symbol::intern(&role_name)),
            );
        } else {
            self.env.remove("?ROLE");
        }
        self.env.insert("self".to_string(), base.clone());
        if let Some(role_bindings) = self.class_role_param_bindings.get(owner_class) {
            for (name, value) in role_bindings {
                self.env.insert(name.clone(), value.clone());
            }
        } else if let Some(role_bindings) = self.class_role_param_bindings.get(receiver_class_name)
        {
            for (name, value) in role_bindings {
                self.env.insert(name.clone(), value.clone());
            }
        }

        let mut bind_params = Vec::new();
        let mut bind_param_defs = Vec::new();
        for (idx, param_name) in method_def.params.iter().enumerate() {
            let is_invocant = method_def
                .param_defs
                .get(idx)
                .map(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                .unwrap_or(false);
            if is_invocant {
                if let Some(pd) = method_def.param_defs.get(idx)
                    && let Some(constraint) = &pd.type_constraint
                {
                    if let Some(captured_name) = constraint.strip_prefix("::") {
                        self.env
                            .insert(captured_name.to_string(), Self::captured_type_object(&base));
                    } else {
                        let coercion_target = if let Some(open) = constraint.find('(') {
                            if constraint.ends_with(')') && open > 0 {
                                Some(&constraint[..open])
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        let expected = coercion_target.unwrap_or(constraint.as_str());
                        if coercion_target.is_some() {
                            let mut candidate = self
                                .try_coerce_value_for_constraint(constraint, base.clone())
                                .unwrap_or_else(|_| base.clone());
                            if !self.type_matches_value(expected, &candidate)
                                && let Ok(coerced) =
                                    self.call_method_with_values(base.clone(), expected, vec![])
                            {
                                candidate = coerced;
                            }
                            if self.type_matches_value(expected, &candidate) {
                                base = candidate;
                                self.env.insert("self".to_string(), base.clone());
                            }
                        } else if !self.type_matches_value(constraint, &base)
                            && let Ok(coerced) =
                                self.try_coerce_value_for_constraint(constraint, base.clone())
                        {
                            base = coerced;
                            self.env.insert("self".to_string(), base.clone());
                        }
                        if !self.type_matches_value(expected, &base) {
                            self.method_class_stack.pop();
                            self.env = saved_env;
                            self.var_bindings = saved_var_bindings;
                            self.restore_readonly_vars(saved_readonly);
                            return Err(RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                                param_name,
                                constraint,
                                super::value_type_name(&base)
                            )));
                        }
                    }
                }
                self.env.insert(param_name.clone(), base.clone());
                continue;
            }
            bind_params.push(param_name.clone());
            if let Some(pd) = method_def.param_defs.get(idx) {
                bind_param_defs.push(pd.clone());
            }
        }

        for (attr_name, attr_val) in &attributes {
            self.env.insert(format!("!{}", attr_name), attr_val.clone());
            self.env.insert(format!(".{}", attr_name), attr_val.clone());
            self.var_bindings
                .insert(attr_name.clone(), format!("!{}", attr_name));
            self.var_bindings.insert(
                format!("{}::{}", owner_class, attr_name),
                format!("!{}", attr_name),
            );
        }
        // Method signatures must support full parameter binding semantics
        // (coercions, slurpy params, defaults, and named args) for both
        // type-object and instance invocations.
        match self.bind_function_args_values(&bind_param_defs, &bind_params, &args) {
            Ok(_) => {}
            Err(e) => {
                self.method_class_stack.pop();
                self.env = saved_env;
                self.var_bindings = saved_var_bindings;
                self.restore_readonly_vars(saved_readonly);
                return Err(e);
            }
        }
        for p in &bind_params {
            self.var_bindings.remove(p);
        }
        // When the method body is re-compiled by run_block, the compiler
        // qualifies bare variable names with current_package (e.g. "m" →
        // "G::m").  Mirror bound params under their qualified names so the
        // generated GetGlobal lookup succeeds.
        let pkg = self.current_package.clone();
        if pkg != "GLOBAL" {
            for p in &bind_params {
                if !p.contains("::")
                    && !p.starts_with('_')
                    && !p.starts_with('/')
                    && !p.starts_with('!')
                    && !p.starts_with('?')
                    && !p.starts_with('*')
                    && !p.starts_with('.')
                    && !p.starts_with('=')
                    && !p.starts_with('$')
                    && !p.starts_with('@')
                    && !p.starts_with('%')
                    && !p.starts_with('&')
                    && let Some(v) = self.env.get(p).cloned()
                {
                    self.env.insert(format!("{}::{}", pkg, p), v);
                }
            }
        }
        // Push onto routine_stack so that `return` inside the method body
        // compiles as `Return` (not `ReturnFromNonRoutine`).
        self.routine_stack
            .push((owner_class.to_string(), String::new()));
        let block_result = self.run_block(&method_def.body);
        self.routine_stack.pop();
        let implicit_return = self.env.get("_").cloned();
        let result = match block_result {
            Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
        };
        for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
            let original = attributes.get(&attr_name).cloned().unwrap_or(Value::Nil);
            let env_key = format!("!{}", attr_name);
            let public_env_key = format!(".{}", attr_name);
            let env_private = self.env.get(&env_key).cloned();
            let env_public = self.env.get(&public_env_key).cloned();
            if let (Some(private_val), Some(public_val)) = (&env_private, &env_public) {
                // `$.attr` aliases (public) should still write back when only the
                // public mirror changed (e.g. `$.count++`).
                if *private_val == original && *public_val != original {
                    attributes.insert(attr_name, public_val.clone());
                } else {
                    attributes.insert(attr_name, private_val.clone());
                }
                continue;
            }
            if let Some(val) = self.env.get(&env_key) {
                attributes.insert(attr_name, val.clone());
                continue;
            }
            if let Some(val) = self.env.get(&public_env_key) {
                attributes.insert(attr_name, val.clone());
            }
        }
        let mut merged_env = saved_env.clone();
        for (k, v) in self.env.iter() {
            if saved_env.contains_key(k) {
                merged_env.insert(k.clone(), v.clone());
            }
            if (k.starts_with('&') && !k.starts_with("&?"))
                || k.starts_with("__mutsu_method_value::")
            {
                merged_env.insert(k.clone(), v.clone());
            }
        }
        self.method_class_stack.pop();
        self.env = merged_env;
        self.var_bindings = saved_var_bindings;
        self.restore_readonly_vars(saved_readonly);
        result.map(|v| {
            let adjusted = match (&base, &v) {
                (
                    Value::Instance {
                        class_name,
                        id: base_id,
                        ..
                    },
                    Value::Instance { id: ret_id, .. },
                ) if base_id == ret_id => {
                    Value::make_instance_with_id(*class_name, attributes.clone(), *base_id)
                }
                _ => v,
            };
            (adjusted, attributes)
        })
    }
}
