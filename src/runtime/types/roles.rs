use super::*;

impl Interpreter {
    pub(crate) fn role_def_for_mixin_role(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        role_name: &str,
    ) -> Option<RoleDef> {
        let role_id = mixins
            .get(&format!("__mutsu_role_id__{role_name}"))
            .and_then(|value| match value {
                Value::Int(id) if *id > 0 => Some(*id as u64),
                _ => None,
            });
        if let Some(role_id) = role_id
            && let Some(candidate) = self.role_candidates.get(role_name).and_then(|candidates| {
                candidates
                    .iter()
                    .find(|candidate| candidate.role_def.role_id == role_id)
            })
        {
            return Some(candidate.role_def.clone());
        }
        self.roles.get(role_name).cloned()
    }

    pub(crate) fn resolve_parametric_role_runtime(
        &mut self,
        base_name: &str,
        type_args: &[Value],
    ) -> Option<(RoleDef, Vec<String>)> {
        let mut selected_role = self.roles.get(base_name).cloned();
        let mut selected_param_names = self
            .role_type_params
            .get(base_name)
            .cloned()
            .unwrap_or_default();
        if let Some(candidates) = self.role_candidates.get(base_name).cloned() {
            let mut matching: Vec<(crate::runtime::RoleCandidateDef, i32, usize)> = candidates
                .into_iter()
                .enumerate()
                .filter_map(|(idx, candidate)| {
                    let candidate_param_names = candidate
                        .type_param_defs
                        .iter()
                        .map(|pd| pd.name.clone())
                        .collect::<Vec<_>>();
                    let positional_params = candidate
                        .type_param_defs
                        .iter()
                        .filter(|pd| !pd.named)
                        .collect::<Vec<_>>();
                    let has_positional_slurpy = positional_params
                        .iter()
                        .any(|pd| pd.slurpy && !pd.name.starts_with('%'));
                    let required = positional_params
                        .iter()
                        .filter(|pd| !pd.slurpy && pd.default.is_none() && !pd.optional_marker)
                        .count();
                    let arity_ok = if candidate.type_param_defs.is_empty() {
                        type_args.is_empty()
                    } else {
                        type_args.len() >= required
                            && (has_positional_slurpy || type_args.len() <= positional_params.len())
                    };
                    let ok = if arity_ok {
                        let saved_env = self.env.clone();
                        let ok = self
                            .bind_function_args_values(
                                &candidate.type_param_defs,
                                &candidate_param_names,
                                type_args,
                            )
                            .is_ok();
                        self.env = saved_env;
                        ok
                    } else {
                        false
                    };
                    if ok {
                        let score = candidate
                            .type_param_defs
                            .iter()
                            .filter(|pd| !pd.named)
                            .map(|pd| {
                                let mut s = if let Some(tc) = pd.type_constraint.as_deref() {
                                    if tc.starts_with("::") || tc == "Any" || tc == "Mu" {
                                        1
                                    } else {
                                        5
                                    }
                                } else {
                                    0
                                };
                                if pd.where_constraint.is_some() {
                                    s += 20;
                                }
                                if pd.literal_value.is_some() {
                                    s += 30;
                                }
                                s
                            })
                            .sum();
                        Some((candidate, score, idx))
                    } else {
                        None
                    }
                })
                .collect();
            matching.sort_by(|a, b| b.1.cmp(&a.1).then(b.2.cmp(&a.2)));
            if let Some((candidate, _, _)) = matching.into_iter().next() {
                selected_param_names = candidate.type_params.clone();
                selected_role = Some(candidate.role_def.clone());
            }
        }
        selected_role.map(|role| (role, selected_param_names))
    }

    pub(crate) fn delegated_role_attr_key_from_mixins(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        method_name: &str,
    ) -> Option<String> {
        for role_name in mixins
            .keys()
            .filter_map(|key| key.strip_prefix("__mutsu_role__"))
        {
            let Some(role) = self.role_def_for_mixin_role(mixins, role_name) else {
                continue;
            };
            let Some(method_defs) = role.methods.get(method_name) else {
                continue;
            };
            for method_def in method_defs {
                let Some((attr_var_name, target_method)) = &method_def.delegation else {
                    continue;
                };
                if target_method == method_name {
                    let attr_name = attr_var_name.trim_start_matches(['.', '!']);
                    return Some(format!("__mutsu_attr__{attr_name}"));
                }
            }
        }
        None
    }

    pub(crate) fn eval_does_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some((role_name, args)) = self.extract_role_application(&right) {
            let result = self.compose_role_on_value(left.clone(), &role_name, &args)?;
            // Call BUILD submethods from the composed role
            let result = self.call_role_build_submethods(result, &role_name)?;
            if let Some(target_name) = Self::var_target_name_from_value(&left) {
                self.set_var_meta_value(&target_name, result.clone());
            }
            return Ok(result);
        }
        let role_name = right.to_string_value();
        Ok(Value::Bool(left.does_check(&role_name)))
    }

    /// Apply multiple roles at once: `$obj does (RoleA, RoleB)`
    pub(crate) fn eval_does_values_list(
        &mut self,
        left: Value,
        roles: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut result = left.clone();
        let mut composed_role_names = Vec::new();
        for role_value in roles {
            if let Some((role_name, args)) = self.extract_role_application(role_value) {
                result = self.compose_role_on_value(result, &role_name, &args)?;
                composed_role_names.push(role_name);
            }
        }
        // Call BUILD submethods for all composed roles
        for role_name in &composed_role_names {
            result = self.call_role_build_submethods(result, role_name)?;
        }
        if let Some(target_name) = Self::var_target_name_from_value(&left) {
            self.set_var_meta_value(&target_name, result.clone());
        }
        Ok(result)
    }

    fn var_target_name_from_value(value: &Value) -> Option<String> {
        match value {
            Value::Mixin(inner, _) => Self::var_target_name_from_value(inner),
            Value::Instance { attributes, .. } => match attributes.get("__mutsu_var_target") {
                Some(Value::Str(name)) => Some(name.to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Check if a value represents a role application (used by VM to decide
    /// whether to fall back to the interpreter for `does` operations).
    pub(crate) fn is_role_application(&self, rhs: &Value) -> bool {
        self.extract_role_application(rhs).is_some()
    }

    fn extract_role_application(&self, rhs: &Value) -> Option<(String, Vec<Value>)> {
        match rhs {
            Value::ParametricRole {
                base_name,
                type_args,
            } if self.roles.contains_key(&base_name.resolve()) => {
                Some((base_name.resolve(), type_args.clone()))
            }
            Value::Pair(name, boxed) if self.roles.contains_key(name) => {
                if let Value::Array(args, ..) = boxed.as_ref() {
                    Some((name.clone(), args.as_ref().clone()))
                } else {
                    None
                }
            }
            Value::Package(name) if self.roles.contains_key(&name.resolve()) => {
                Some((name.resolve(), Vec::new()))
            }
            Value::Str(name) if self.roles.contains_key(name.as_str()) => {
                Some((name.to_string(), Vec::new()))
            }
            _ => None,
        }
    }

    fn compose_role_on_value(
        &mut self,
        left: Value,
        role_name: &str,
        role_args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let role = self.roles.get(role_name).cloned();
        if role.is_none()
            && !matches!(
                role_name,
                "Real" | "Numeric" | "Cool" | "Any" | "Mu" | "Positional" | "Associative"
            )
        {
            return Err(RuntimeError::new(format!("Unknown role: {}", role_name)));
        }

        let (inner, mut mixins) = match left {
            Value::Mixin(inner, existing) => (inner.as_ref().clone(), (*existing).clone()),
            other => (other, HashMap::new()),
        };
        mixins.insert(format!("__mutsu_role__{}", role_name), Value::Bool(true));
        // Store the role's unique ID so that different lexical roles with the
        // same name (e.g. two `my role A { }` in different scopes) produce
        // distinct mixin maps, making `===` return False for values mixed with
        // different role instances.
        let role_id = self.roles.get(role_name).map_or(0, |r| r.role_id);
        if role_id != 0 {
            mixins.insert(
                format!("__mutsu_role_id__{}", role_name),
                Value::Int(role_id as i64),
            );
        }

        if let Some(role) = role {
            // Temporarily merge captured environment from the role definition
            // so that attribute defaults can reference closure variables.
            let saved_env = if let Some(captured) = &role.captured_env {
                let saved = self.env.clone();
                for (k, v) in captured {
                    if !self.env.contains_key(k) {
                        self.env.insert(k.clone(), v.clone());
                    }
                }
                Some(saved)
            } else {
                None
            };
            for (idx, (attr_name, _is_public, default_expr, _, _, _, _)) in
                role.attributes.iter().enumerate()
            {
                let value = if let Some(arg) = role_args.get(idx) {
                    arg.clone()
                } else if let Some(default_expr) = default_expr {
                    self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?
                } else {
                    Value::Nil
                };
                mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
            }
            if let Some(saved) = saved_env {
                self.env = saved;
            }
        }

        Ok(Value::mixin(inner, mixins))
    }

    /// Call BUILD submethods from a role after mixin composition.
    /// In Raku 6.e, when `$obj does Role`, the BUILD submethods of the role
    /// are invoked on the resulting object.
    fn call_role_build_submethods(
        &mut self,
        target: Value,
        role_name: &str,
    ) -> Result<Value, RuntimeError> {
        let role = match self.roles.get(role_name).cloned() {
            Some(r) => r,
            None => return Ok(target),
        };
        let build_methods = role.methods.get("BUILD").cloned();
        if let Some(overloads) = build_methods {
            for def in overloads {
                // is_my is set to true for submethods in role method registration
                if def.is_my {
                    // Temporarily merge the role's captured environment so that
                    // closure variables from the role definition scope are accessible
                    // and modifications propagate back to the original scope.
                    if let Some(captured) = &role.captured_env {
                        for (k, v) in captured {
                            if !self.env.contains_key(k) {
                                self.env.insert(k.clone(), v.clone());
                            }
                        }
                    }
                    // Set self for the BUILD body
                    let saved_self = self.env.get("self").cloned();
                    self.env.insert("self".to_string(), target.clone());
                    // Execute BUILD body directly in current scope so closure
                    // variable mutations propagate to the outer scope.
                    let _result = self.eval_block_value(&def.body)?;
                    // Restore self
                    if let Some(prev) = saved_self {
                        self.env.insert("self".to_string(), prev);
                    } else {
                        self.env.remove("self");
                    }
                    break;
                }
            }
        }
        Ok(target)
    }
}
