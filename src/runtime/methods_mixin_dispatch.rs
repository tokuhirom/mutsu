use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch method calls on Value::Mixin targets.
    /// Returns Some(result) if the method was handled, None if not.
    pub(super) fn dispatch_mixin_method_call(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let Value::Mixin(inner, mixins) = target else {
            return None;
        };

        if args.is_empty() {
            if let Some(mixin_val) = mixins.get(method) {
                return Some(Ok(mixin_val.clone()));
            }
            // Check role attribute accessors: has $.foo stores as __mutsu_attr__foo
            let attr_key = format!("__mutsu_attr__{}", method);
            if let Some(attr_val) = mixins.get(&attr_key) {
                // An explicit method declared in a composed role shadows the
                // auto-generated accessor for an attribute of the same name
                // (e.g. `has $.b` + `method b() {...}` — the method wins).
                let has_explicit_method = mixins
                    .keys()
                    .filter_map(|k| k.strip_prefix("__mutsu_role__"))
                    .any(|role_name| {
                        self.registry()
                            .roles
                            .get(role_name)
                            .is_some_and(|role| role.methods.contains_key(method))
                    });
                let is_public = !has_explicit_method
                    && mixins
                        .keys()
                        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
                        .any(|role_name| {
                            self.registry().roles.get(role_name).is_some_and(|role| {
                                role.attributes
                                    .iter()
                                    .any(|(name, is_pub, ..)| name == method && *is_pub)
                            })
                        });
                if is_public {
                    return Some(Ok(attr_val.clone()));
                }
            }
            for mixin_val in mixins.values() {
                if let Value::Enum { enum_type, key, .. } = mixin_val {
                    if method == key.resolve() {
                        return Some(Ok(Value::Bool(true)));
                    }
                    if let Some(variants) = self.registry().enum_types.get(&enum_type.resolve())
                        && variants.iter().any(|(variant, _)| variant == method)
                    {
                        return Some(Ok(Value::Bool(false)));
                    }
                }
            }
        }
        let mut role_names: Vec<String> = mixins
            .iter()
            .filter_map(|(key, value)| {
                key.strip_prefix("__mutsu_role__")
                    .and_then(|name| value.truthy().then_some(name.to_string()))
            })
            .collect();
        role_names.sort();
        // Determine if this is a private method call (method starts with '!')
        let is_private_call = method.starts_with('!');
        let lookup_name = if is_private_call {
            &method[1..]
        } else {
            method
        };
        let mut role_has_method = false;
        for role_name in role_names {
            let Some(role) = self.role_def_for_mixin_role(mixins, &role_name) else {
                continue;
            };
            let Some(overloads) = role.methods.get(lookup_name).cloned() else {
                continue;
            };
            role_has_method = true;
            let role_param_bindings: Vec<(String, Value)> = mixins
                .iter()
                .filter_map(|(key, value)| {
                    key.strip_prefix("__mutsu_role_param__")
                        .map(|name| (name.to_string(), value.clone()))
                })
                .collect();
            let mut saved_role_params: Vec<(String, Option<Value>)> = Vec::new();
            for (name, value) in &role_param_bindings {
                saved_role_params.push((name.clone(), self.env.get(name).cloned()));
                self.env.insert(name.clone(), value.clone());
            }
            for def in overloads {
                // For private calls, only match private methods; for public calls, skip private
                if is_private_call != def.is_private
                    || !self.method_args_match(&args, &def.param_defs)
                {
                    continue;
                }
                // Build the attribute set visible to the role method body.
                // Start with the inner instance's own attributes (e.g. class
                // attributes like `@.order`) so that `$.attr` accessors inside
                // the role method see and can mutate the class's state, then
                // overlay the role's own `__mutsu_attr__` attributes.
                let (inner_class, inner_id, mut method_attrs) = match inner.as_ref() {
                    Value::Instance {
                        class_name,
                        attributes,
                        id,
                    } => (Some(class_name.resolve()), Some(*id), attributes.to_map()),
                    _ => (None, None, HashMap::new()),
                };
                for (key, value) in mixins.iter() {
                    if let Some(attr) = key.strip_prefix("__mutsu_attr__") {
                        method_attrs.insert(attr.to_string(), value.clone());
                    }
                }
                let method_result = self.run_instance_method_resolved(
                    &role_name,
                    &role_name,
                    def,
                    method_attrs,
                    args,
                    Some(target.clone()),
                );
                for (name, previous) in &saved_role_params {
                    if let Some(prev) = previous {
                        self.env.insert(name.clone(), prev.clone());
                    } else {
                        self.env.remove(name);
                    }
                }
                let (result, updated) = match method_result {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                // Propagate attribute mutations made by the role method back to
                // the inner instance, so that changes to class attributes (e.g.
                // `push @.order, ...`) are visible after the call returns. This
                // updates every binding in scope that holds the same instance
                // (including the one wrapped inside this Mixin).
                if let (Some(class_name), Some(id)) = (inner_class, inner_id) {
                    self.overwrite_instance_bindings_by_identity(&class_name, id, updated);
                }
                return Some(Ok(result));
            }
            for (name, previous) in saved_role_params {
                if let Some(prev) = previous {
                    self.env.insert(name, prev);
                } else {
                    self.env.remove(&name);
                }
            }
        }
        if role_has_method {
            return Some(Err(super::methods_signature::make_multi_no_match_error(
                method,
            )));
        }
        if method == "can" && args.len() == 1 {
            let method_name = args[0].to_string_value();
            // First collect from the inner value's MRO
            let mut results = self.collect_can_methods(inner, &method_name);
            // Also check mixin-specific methods
            if (mixins.contains_key(&method_name)
                || mixins.contains_key(&format!("__mutsu_attr__{}", method_name)))
                && results.is_empty()
            {
                results.push(Value::Routine {
                    package: Symbol::intern("Mixin"),
                    name: Symbol::intern(&method_name),
                    is_regex: false,
                });
            }
            for role_name in mixins.keys().filter_map(|key| {
                key.strip_prefix("__mutsu_role__")
                    .map(|name| name.to_string())
            }) {
                if let Some(role) = self.registry().roles.get(&role_name)
                    && let Some(defs) = role.methods.get(&method_name)
                {
                    for def in defs {
                        results.push(Value::make_sub(
                            Symbol::intern(&role_name),
                            Symbol::intern(&method_name),
                            def.params.clone(),
                            def.param_defs.clone(),
                            (*def.body).clone(),
                            def.is_rw,
                            crate::env::Env::new(),
                        ));
                    }
                }
            }
            return Some(Ok(Value::array(results)));
        }
        if method == "does" && args.len() == 1 {
            let does = match &args[0] {
                Value::Enum {
                    enum_type,
                    key: probe_key,
                    ..
                } => matches!(
                    mixins.get(&enum_type.resolve()),
                    Some(Value::Enum { key, .. }) if key == probe_key
                ),
                Value::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    let base = base_name.resolve();
                    let has_role = target.does_check(&base)
                        || mixins.contains_key(&base)
                        || mixins.contains_key(&format!("__mutsu_role__{}", base));
                    if has_role {
                        let key = format!("__mutsu_role_typeargs__{}", base);
                        if let Some(Value::Array(actual_args, ..)) = mixins.get(&key) {
                            actual_args.len() == type_args.len()
                                && actual_args
                                    .iter()
                                    .zip(type_args.iter())
                                    .all(|(a, e)| self.parametric_arg_subtypes(a, e))
                        } else {
                            type_args.is_empty()
                        }
                    } else {
                        false
                    }
                }
                Value::Package(name) => {
                    let n = name.resolve();
                    let base = n.split('[').next().unwrap_or(&n);
                    mixins.contains_key(&n)
                        || mixins.contains_key(base)
                        || mixins.contains_key(&format!("__mutsu_role__{}", n))
                        || mixins.contains_key(&format!("__mutsu_role__{}", base))
                        || self.type_matches_value(&n, target)
                }
                Value::Str(name) => {
                    mixins.contains_key(name.as_str())
                        || mixins.contains_key(&format!("__mutsu_role__{}", name))
                        || self.type_matches_value(name, target)
                }
                Value::Instance { class_name, .. } => {
                    self.type_matches_value(&class_name.resolve(), target)
                }
                other => self.type_matches_value(&other.to_string_value(), target),
            };
            return Some(Ok(Value::Bool(does)));
        }
        if method == "isa" && args.len() == 1 {
            let target_name = match args.first().cloned().unwrap_or(Value::Nil) {
                Value::Package(name) => name.resolve(),
                Value::Str(name) => name.to_string(),
                Value::Instance { class_name, .. } => class_name.resolve(),
                other => other.to_string_value(),
            };
            // Roles are excluded from isa checks
            let role_key = format!("__mutsu_role__{}", target_name);
            if mixins.contains_key(&role_key) {
                return Some(Ok(Value::Bool(false)));
            }
            // Delegate to inner value's isa check using class MRO
            let result = match inner.as_ref() {
                Value::Instance { class_name, .. } => {
                    self.class_mro(&class_name.resolve()).contains(&target_name)
                }
                _ => inner.isa_check(&target_name),
            };
            return Some(Ok(Value::Bool(result)));
        }

        None
    }
}
