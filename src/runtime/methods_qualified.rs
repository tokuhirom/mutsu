use super::methods_signature::{make_method_not_found_error, make_private_permission_error};
use super::*;

impl Interpreter {
    /// Handle private method calls on non-Instance, non-Package values.
    /// Returns Some(err) if handled, None to continue.
    pub(super) fn dispatch_private_method_on_non_instance(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let private_rest = method.strip_prefix('!')?;
        if matches!(
            target,
            Value::Instance { .. } | Value::Package(_) | Value::Mixin(..)
        ) {
            return None;
        }
        // Owner-qualified: !Owner::method
        if let Some((owner_class, private_name)) = private_rest.split_once("::") {
            let caller_class = self
                .method_class_stack
                .last()
                .cloned()
                .or_else(|| Some(self.current_package().to_string()));
            let caller_allowed = caller_class.as_deref() == Some(owner_class)
                || self.class_trusts.get(owner_class).is_some_and(|trusted| {
                    caller_class
                        .as_ref()
                        .is_some_and(|caller| trusted.contains(caller))
                });
            if !caller_allowed {
                return Some(Err(make_private_permission_error(
                    private_name,
                    owner_class,
                )));
            }
        }
        // Unqualified private method on non-Instance — not found
        Some(Err(make_method_not_found_error(
            private_rest
                .split_once("::")
                .map_or(private_rest, |(_o, m)| m),
            "Any",
            true,
        )))
    }

    /// Handle qualified method names: Class::method (e.g., $o.Parent::x).
    /// Returns Some(result) if handled, None to continue.
    pub(super) fn dispatch_qualified_instance_method(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let (qualifier, actual_method) = method.rsplit_once("::")?;
        if method.starts_with('!') {
            return None;
        }
        let Value::Instance {
            class_name: inst_class,
            attributes,
            id: target_id,
        } = target
        else {
            return None;
        };

        // Check that the qualifier class/role is in the instance's MRO or composed roles
        let inst_cn_str = inst_class.resolve();
        let inst_mro = self.class_mro(&inst_cn_str);
        let in_mro = inst_mro.iter().any(|c| c == qualifier);
        let in_composed_roles = if !in_mro {
            inst_mro.iter().any(|c| {
                self.class_composed_roles.get(c).is_some_and(|roles| {
                    roles.iter().any(|r| {
                        r == qualifier
                            || r.starts_with(qualifier) && r[qualifier.len()..].starts_with('[')
                    })
                })
            })
        } else {
            false
        };
        if !in_mro && !in_composed_roles {
            return Some(Err(RuntimeError::new(format!(
                "Cannot dispatch to method {} on {} because it is not inherited or done by {}",
                actual_method, qualifier, inst_cn_str
            ))));
        }
        // Read: look up the attribute in the qualifier class's attribute definitions
        if args.is_empty() {
            let class_attrs = self.collect_class_attributes(qualifier);
            for (attr_name, is_public, ..) in &class_attrs {
                if *is_public && attr_name == actual_method {
                    return Some(Ok(attributes
                        .get(actual_method)
                        .cloned()
                        .unwrap_or(Value::Nil)));
                }
            }
        }
        // Try running the actual method on the qualifier class
        if let Some((_owner, method_def)) =
            self.resolve_method_with_owner(qualifier, actual_method, &args)
        {
            let attrs_map = (**attributes).clone();
            let inst_cn = inst_cn_str.to_string();
            let tid = *target_id;
            let (result, updated) = match self.run_instance_method(
                qualifier,
                attrs_map,
                actual_method,
                args,
                Some(target.clone()),
            ) {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            self.overwrite_instance_bindings_by_identity(&inst_cn, tid, updated);
            if !self.in_lvalue_assignment
                && let Value::Proxy { ref fetcher, .. } = result
            {
                let _ = method_def;
                return Some(self.proxy_fetch(
                    fetcher,
                    None,
                    qualifier,
                    &(**attributes).clone(),
                    0,
                ));
            }
            return Some(Ok(result));
        }
        // Fallback: find a method with matching role_origin in the instance's class.
        if let Some(overloads) = self
            .classes
            .get(&inst_cn_str)
            .and_then(|c| c.methods.get(actual_method))
            .cloned()
        {
            for def in overloads {
                if def.role_origin.as_deref() == Some(qualifier)
                    && !def.is_private
                    && self.method_args_match(&args, &def.param_defs)
                {
                    let attrs_map = (**attributes).clone();
                    let inst_cn = inst_cn_str.to_string();
                    let tid = *target_id;
                    let (result, updated) = match self.run_instance_method_resolved(
                        &inst_cn,
                        qualifier,
                        def,
                        attrs_map,
                        args,
                        Some(target.clone()),
                    ) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };
                    self.overwrite_instance_bindings_by_identity(&inst_cn, tid, updated);
                    return Some(Ok(result));
                }
            }
        }
        // Also check the role definition directly
        let role_lookup = self.roles.get(qualifier).cloned().or_else(|| {
            self.roles
                .iter()
                .find(|(name, _)| {
                    name.starts_with(qualifier) && name[qualifier.len()..].starts_with('[')
                })
                .map(|(_, def)| def.clone())
        });
        if let Some(role_def) = role_lookup
            && let Some(overloads) = role_def.methods.get(actual_method)
        {
            for def in overloads {
                if !def.is_private && self.method_args_match(&args, &def.param_defs) {
                    let attrs_map = (**attributes).clone();
                    let inst_cn = inst_cn_str.to_string();
                    let tid = *target_id;
                    let (result, updated) = match self.run_instance_method_resolved(
                        &inst_cn,
                        qualifier,
                        def.clone(),
                        attrs_map,
                        args,
                        Some(target.clone()),
                    ) {
                        Ok(v) => v,
                        Err(e) => return Some(Err(e)),
                    };
                    self.overwrite_instance_bindings_by_identity(&inst_cn, tid, updated);
                    return Some(Ok(result));
                }
            }
        }

        None
    }

    /// Handle qualified method calls on non-Instance values: e.g. (-42).Int::abs
    pub(super) fn dispatch_qualified_non_instance_method(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let (qualifier, actual_method) = method.split_once("::")?;
        if method.starts_with('!') || matches!(target, Value::Instance { .. }) {
            return None;
        }
        // Let constructor dispatch handle qualified base-constructor calls like
        // `self.Mu::new(...)`. Routing this through unqualified `.new` re-enters
        // user-defined constructors and recurses indefinitely.
        if matches!(target, Value::Package(_)) && qualifier == "Mu" && actual_method == "new" {
            return None;
        }
        let type_name = super::utils::value_type_name(target);
        let type_matches = qualifier == type_name || Self::type_inherits(type_name, qualifier);
        if type_matches {
            return Some(self.call_method_with_values(target.clone(), actual_method, args));
        }
        Some(Err(RuntimeError::new(format!(
            "X::Method::InvalidQualifier: Cannot dispatch to a method on {} because it is not inherited or done by {}",
            qualifier, type_name
        ))))
    }

    /// Handle method calls on Proxy subclass values (accessing subclass attributes).
    pub(super) fn dispatch_proxy_subclass_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let Value::Proxy {
            subclass: Some((subclass_name, subclass_attrs)),
            ..
        } = target
        else {
            return None;
        };

        {
            let attrs = subclass_attrs.lock().unwrap();
            if let Some(val) = attrs.get(method)
                && args.is_empty()
            {
                self.pending_proxy_subclass_attr =
                    Some((subclass_attrs.clone(), method.to_string()));
                return Some(Ok(val.clone()));
            }
        }
        // Handle .raku on Proxy subclass
        if method == "raku" || method == "Str" || method == "gist" {
            return Some(Ok(Value::str(format!("{}()", subclass_name.resolve()))));
        }

        None
    }

    /// Auto-FETCH Proxy values for most method calls.
    pub(super) fn dispatch_proxy_auto_fetch(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let Value::Proxy {
            fetcher,
            decontainerized,
            ..
        } = target
        else {
            return None;
        };
        if *decontainerized {
            return None;
        }
        if matches!(
            method,
            "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
        ) {
            return None;
        }
        let fetched = match self.call_sub_value(*fetcher.clone(), vec![target.clone()], true) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        Some(self.call_method_with_values(fetched, method, args))
    }
}
