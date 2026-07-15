use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch method calls on Mixin targets.
    /// Returns Some(result) if the method was handled, None if not.
    pub(super) fn dispatch_mixin_method_call(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, mixins) = target.view() else {
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
                if let ValueView::Enum { enum_type, key, .. } = mixin_val.view() {
                    if method == key.resolve() {
                        return Some(Ok(Value::TRUE));
                    }
                    if let Some(variants) = self.registry().enum_types.get(&enum_type.resolve())
                        && variants.iter().any(|(variant, _)| variant == method)
                    {
                        return Some(Ok(Value::FALSE));
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
                let (inner_cell, mut method_attrs) = match inner.as_ref().view() {
                    ValueView::Instance { attributes, .. } => {
                        (Some(attributes.clone()), attributes.to_map())
                    }
                    _ => (None, AttrMap::new()),
                };
                for (key, value) in mixins.iter() {
                    if let Some(attr) = key.strip_prefix("__mutsu_attr__") {
                        method_attrs.insert(attr, value.clone());
                    }
                }
                // Set up a method-dispatch frame so `nextsame`/`callsame` inside
                // the role method falls through to the mixed-in base object's
                // method of the same name: `A.new but Role` where the role's
                // method calls `nextsame` must reach the class's original method.
                let base_class = match inner.as_ref().view() {
                    ValueView::Instance { class_name, .. } => {
                        Some(class_name.resolve().to_string())
                    }
                    _ => None,
                };
                let base_remaining: Vec<(String, MethodDef)> = if let Some(bc) = &base_class {
                    self.resolve_all_methods_with_owner(bc, lookup_name, &args)
                        .into_iter()
                        .filter(|(_, d)| d.is_private == is_private_call)
                        .map(|(o, d)| (o.resolve(), d))
                        .collect()
                } else {
                    Vec::new()
                };
                let pushed_base_dispatch = !base_remaining.is_empty();
                if pushed_base_dispatch {
                    let rw_params =
                        super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs);
                    self.samewith_context_stack
                        .push((lookup_name.to_string(), Some(target.clone())));
                    self.method_dispatch_stack.push(super::MethodDispatchFrame {
                        receiver_class: base_class.clone().unwrap_or_default(),
                        invocant: target.clone(),
                        args: args.clone(),
                        remaining: base_remaining,
                        rw_params,
                    });
                }
                let method_result = self.run_resolved_method_compiled_or_treewalk(
                    &role_name,
                    &role_name,
                    lookup_name,
                    def,
                    method_attrs,
                    args,
                    Some(target.clone()),
                );
                if pushed_base_dispatch {
                    self.method_dispatch_stack.pop();
                    self.samewith_context_stack.pop();
                }
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
                if let Some(cell) = &inner_cell {
                    cell.commit_attrs(updated);
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
            return Some(Err(
                super::methods_signature_errors::make_multi_no_match_error(method),
            ));
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
                results.push(Value::routine_parts(
                    Symbol::intern("Mixin"),
                    Symbol::intern(&method_name),
                    false,
                ));
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
            let does = match args[0].view() {
                ValueView::Enum {
                    enum_type,
                    key: probe_key,
                    ..
                } => matches!(
                    mixins.get(&enum_type.resolve()).map(Value::view),
                    Some(ValueView::Enum { key, .. }) if key == probe_key
                ),
                ValueView::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    let base = base_name.resolve();
                    let has_role = target.does_check(&base)
                        || mixins.contains_key(&base)
                        || mixins.contains_key(&format!("__mutsu_role__{}", base));
                    if has_role {
                        let key = format!("__mutsu_role_typeargs__{}", base);
                        if let Some(ValueView::Array(actual_args, ..)) =
                            mixins.get(&key).map(Value::view)
                        {
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
                ValueView::Package(name) => {
                    let n = name.resolve();
                    let base = n.split('[').next().unwrap_or(&n);
                    mixins.contains_key(&n)
                        || mixins.contains_key(base)
                        || mixins.contains_key(&format!("__mutsu_role__{}", n))
                        || mixins.contains_key(&format!("__mutsu_role__{}", base))
                        || self.type_matches_value(&n, target)
                }
                ValueView::Str(name) => {
                    mixins.contains_key(name.as_str())
                        || mixins.contains_key(&format!("__mutsu_role__{}", *name))
                        || self.type_matches_value(&name, target)
                }
                ValueView::Instance { class_name, .. } => {
                    self.type_matches_value(&class_name.resolve(), target)
                }
                _ => self.type_matches_value(&args[0].to_string_value(), target),
            };
            return Some(Ok(Value::truth(does)));
        }
        if method == "isa" && args.len() == 1 {
            let arg0 = args.first().cloned().unwrap_or(Value::NIL);
            let target_name = match arg0.view() {
                ValueView::Package(name) => name.resolve(),
                ValueView::Str(name) => name.to_string(),
                ValueView::Instance { class_name, .. } => class_name.resolve(),
                _ => arg0.to_string_value(),
            };
            // Roles are excluded from isa checks
            let role_key = format!("__mutsu_role__{}", target_name);
            if mixins.contains_key(&role_key) {
                return Some(Ok(Value::FALSE));
            }
            // Delegate to inner value's isa check using class MRO
            let result = match inner.as_ref().view() {
                ValueView::Instance { class_name, .. } => self
                    .class_mro(&class_name.resolve())
                    .contains(&crate::symbol::Symbol::intern(&target_name)),
                _ => inner.isa_check(&target_name),
            };
            return Some(Ok(Value::truth(result)));
        }

        None
    }
}
