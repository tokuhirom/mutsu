use super::*;

impl Interpreter {
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

    pub(super) fn is_native_method(&mut self, class_name: &str, method_name: &str) -> bool {
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
            return Err(RuntimeError::new(format!(
                "No matching candidates for method: {}",
                method_name
            )));
        };
        let all_candidates =
            self.resolve_all_methods_with_owner(receiver_class_name, method_name, &args);
        let invocant_for_dispatch = if let Some(inv) = &invocant {
            inv.clone()
        } else if attributes.is_empty() {
            Value::Package(receiver_class_name.to_string())
        } else {
            Value::make_instance(receiver_class_name.to_string(), attributes.clone())
        };
        let remaining: Vec<(String, MethodDef)> = all_candidates
            .into_iter()
            .skip(1)
            .filter(|(candidate_owner, _)| {
                !self.should_skip_defer_method_candidate(receiver_class_name, candidate_owner)
            })
            .collect();
        let pushed_dispatch = !remaining.is_empty();
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
        let base = if let Some(invocant) = invocant {
            invocant
        } else if attributes.is_empty() {
            Value::Package(receiver_class_name.to_string())
        } else {
            Value::make_instance(receiver_class_name.to_string(), attributes.clone())
        };
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        self.method_class_stack.push(owner_class.to_string());
        let role_context = if self.roles.contains_key(owner_class) {
            Some(owner_class.to_string())
        } else if let Some(composed) = self.class_composed_roles.get(receiver_class_name) {
            let target_fp = crate::ast::function_body_fingerprint(
                &method_def.params,
                &method_def.param_defs,
                &method_def.body,
            );
            composed.iter().find_map(|role_name| {
                let base_role = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role_name.as_str());
                self.roles.get(base_role).and_then(|role_def| {
                    role_def
                        .methods
                        .values()
                        .flatten()
                        .any(|candidate| {
                            crate::ast::function_body_fingerprint(
                                &candidate.params,
                                &candidate.param_defs,
                                &candidate.body,
                            ) == target_fp
                        })
                        .then(|| base_role.to_string())
                })
            })
        } else {
            None
        };
        // Set ::?CLASS / ::?ROLE compile-time variable for the method body
        self.env.insert(
            "?CLASS".to_string(),
            Value::Package(owner_class.to_string()),
        );
        if let Some(role_name) = role_context {
            self.env
                .insert("?ROLE".to_string(), Value::Package(role_name));
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
                    } else if !self.type_matches_value(constraint, &base) {
                        self.method_class_stack.pop();
                        self.env = saved_env;
                        self.restore_readonly_vars(saved_readonly);
                        return Err(RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                            param_name,
                            constraint,
                            super::value_type_name(&base)
                        )));
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
        }
        // Method signatures must support full parameter binding semantics
        // (coercions, slurpy params, defaults, and named args) for both
        // type-object and instance invocations.
        match self.bind_function_args_values(&bind_param_defs, &bind_params, &args) {
            Ok(_) => {}
            Err(e) => {
                self.method_class_stack.pop();
                self.env = saved_env;
                self.restore_readonly_vars(saved_readonly);
                return Err(e);
            }
        }
        let block_result = self.run_block(&method_def.body);
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
                ) if base_id == ret_id => Value::Instance {
                    class_name: class_name.clone(),
                    attributes: std::sync::Arc::new(attributes.clone()),
                    id: *base_id,
                },
                _ => v,
            };
            (adjusted, attributes)
        })
    }
}
