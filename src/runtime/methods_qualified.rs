use super::methods_signature::{
    make_method_not_found_error, make_private_permission_error, make_private_unqualified_error,
};
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
                || self
                    .registry()
                    .class_trusts
                    .get(owner_class)
                    .is_some_and(|trusted| {
                        caller_class
                            .as_ref()
                            .is_some_and(|caller| trusted.contains(caller))
                    });
            if !caller_allowed {
                return Some(Err(make_private_permission_error(
                    private_name,
                    owner_class,
                    caller_class.as_deref().unwrap_or("GLOBAL"),
                )));
            }
            // Owner trusts the caller but the value is not an instance of it:
            // the private method genuinely does not exist on this invocant.
            return Some(Err(make_method_not_found_error(private_name, "Any", true)));
        }
        // An unqualified private call (`$o!meth`) on something other than `self`
        // must name the defining package; Raku reports X::Method::Private::Unqualified.
        Some(Err(make_private_unqualified_error(private_rest)))
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
                self.registry()
                    .class_composed_roles
                    .get(c)
                    .is_some_and(|roles| {
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
        // Relaxed-dispatch ambiguity: if the qualifier names a parametric role and the
        // nearest consumer (the class itself, or a role it composes) composes that base
        // role under two or more *distinct* concretizations, the qualified lookup is
        // ambiguous. Resolution happens against the consumer's immediate roles, so an
        // indirect second concretization (reached through another role) does not count.
        if in_composed_roles
            && self.registry().roles.contains_key(qualifier)
            && self
                .role_concretizations_at_nearest(&inst_cn_str, qualifier)
                .len()
                > 1
        {
            return Some(Err(RuntimeError::new(format!(
                "Ambiguous concretization lookup for {}",
                qualifier
            ))));
        }
        // Read: look up the attribute in the qualifier class's attribute definitions
        if args.is_empty() {
            let class_attrs = self.collect_class_attributes(qualifier);
            for (attr_name, is_public, ..) in &class_attrs {
                if *is_public && attr_name == actual_method {
                    return Some(Ok(attributes
                        .as_map()
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
            let attrs_map = attributes.to_map();
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
                return Some(self.proxy_fetch(fetcher, None, qualifier, &attributes.to_map(), 0));
            }
            return Some(Ok(result));
        }
        // Fallback: find a method with matching role_origin in the instance's class.
        // Hoist clone to a `let` so the guard drops before re-entry (&mut self).
        let overloads = self
            .registry()
            .classes
            .get(&inst_cn_str)
            .and_then(|c| c.methods.get(actual_method))
            .cloned();
        if let Some(overloads) = overloads {
            for def in overloads {
                if def.role_origin.as_deref() == Some(qualifier)
                    && !def.is_private
                    && self.method_args_match(&args, &def.param_defs)
                {
                    let attrs_map = attributes.to_map();
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
        let role_lookup = self.registry().roles.get(qualifier).cloned().or_else(|| {
            self.registry()
                .roles
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
                    let attrs_map = attributes.to_map();
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

    /// Find the distinct concretizations of parametric role `base_role` as composed by
    /// the *nearest* consumer reachable from `class_name`. Resolution follows the
    /// "immediate roles" model: a breadth-first walk over consumer nodes (the class,
    /// then each role it composes, then those roles' composed roles, ...). The first
    /// consumer node that directly composes `base_role` determines the result; its set
    /// of distinct concretizations (e.g. `{"R1[Int]", "R1[Str]"}`) is returned. A second
    /// concretization reached only through a deeper role does not contribute, so it does
    /// not create ambiguity.
    pub(crate) fn role_concretizations_at_nearest(
        &self,
        class_name: &str,
        base_role: &str,
    ) -> std::collections::BTreeSet<String> {
        use std::collections::{BTreeSet, VecDeque};
        // The consumer's directly-composed roles: classes use class_composed_roles,
        // roles use role_parents (which records `does`-composed sub-roles).
        let direct_roles = |node: &str| -> Vec<String> {
            if let Some(roles) = self.registry().class_composed_roles.get(node) {
                roles.clone()
            } else if let Some(parents) = self.registry().role_parents.get(node) {
                parents.clone()
            } else {
                Vec::new()
            }
        };
        let base_of = |full: &str| -> String {
            full.split_once('[')
                .map(|(b, _)| b.to_string())
                .unwrap_or_else(|| full.to_string())
        };
        let mut queue: VecDeque<String> = VecDeque::new();
        let mut seen: BTreeSet<String> = BTreeSet::new();
        queue.push_back(class_name.to_string());
        seen.insert(class_name.to_string());
        while let Some(node) = queue.pop_front() {
            let composed = direct_roles(&node);
            let matches: BTreeSet<String> = composed
                .iter()
                .filter(|r| base_of(r) == base_role)
                .cloned()
                .collect();
            if !matches.is_empty() {
                // Nearest consumer found; ambiguity is decided by its own composition.
                return matches;
            }
            for r in composed {
                let rb = base_of(&r);
                if seen.insert(rb.clone()) {
                    queue.push_back(rb);
                }
            }
        }
        BTreeSet::new()
    }

    /// Handle qualified method names on a runtime-mixed-in value (Value::Mixin):
    /// e.g. after `self does Foo2`, calls like `self.Foo::foo`, `self.Foo1::foo`,
    /// `self.Foo2::foo` must still resolve through the inner instance's MRO/roles
    /// and through roles applied at run time.
    /// Returns Some(result) if handled, None to continue.
    pub(super) fn dispatch_qualified_mixin_method(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let (qualifier, actual_method) = method.rsplit_once("::")?;
        if method.starts_with('!') {
            return None;
        }
        let Value::Mixin(inner, mixins) = target else {
            return None;
        };

        // 1) Qualifier is a role: applied at run time on this mixin, composed on the
        //    inner instance's class, or otherwise a known role. Dispatch directly to
        //    that role's method, keeping the mixin as self so any run-time mixin
        //    survives the call.
        let inner_class = if let Value::Instance { class_name, .. } = inner.as_ref() {
            Some(class_name.resolve())
        } else {
            None
        };
        let composed_on_inner = inner_class.as_deref().is_some_and(|cn| {
            self.class_mro(cn).iter().any(|c| {
                self.registry()
                    .class_composed_roles
                    .get(c)
                    .is_some_and(|roles| {
                        roles.iter().any(|r| {
                            r == qualifier
                                || r.starts_with(qualifier) && r[qualifier.len()..].starts_with('[')
                        })
                    })
            })
        });
        let role_applied = mixins.contains_key(&format!("__mutsu_role__{qualifier}"))
            || mixins.contains_key(qualifier)
            || composed_on_inner;
        if role_applied
            && let Some(role) = self.role_def_for_mixin_role(mixins, qualifier)
            && let Some(overloads) = role.methods.get(actual_method).cloned()
        {
            let role_param_bindings: Vec<(String, Value)> = mixins
                .iter()
                .filter_map(|(key, value)| {
                    key.strip_prefix("__mutsu_role_param__")
                        .map(|name| (name.to_string(), value.clone()))
                })
                .collect();
            for def in overloads {
                if def.is_private || !self.method_args_match(&args, &def.param_defs) {
                    continue;
                }
                // Start from the inner instance's attributes (for compose-time
                // roles whose state lives on the class), then layer mixin-stored
                // role attributes on top (for run-time-applied roles).
                let mut role_attrs: HashMap<String, Value> =
                    if let Value::Instance { attributes, .. } = inner.as_ref() {
                        attributes.to_map()
                    } else {
                        HashMap::new()
                    };
                for (key, value) in mixins.iter() {
                    if let Some(attr) = key.strip_prefix("__mutsu_attr__") {
                        role_attrs.insert(attr.to_string(), value.clone());
                    }
                }
                let mut saved: Vec<(String, Option<Value>)> = Vec::new();
                for (name, value) in &role_param_bindings {
                    saved.push((name.clone(), self.env.get(name).cloned()));
                    self.env.insert(name.clone(), value.clone());
                }
                let res = self.run_instance_method_resolved(
                    qualifier,
                    qualifier,
                    def,
                    role_attrs,
                    args,
                    Some(target.clone()),
                );
                for (name, prev) in saved {
                    if let Some(prev) = prev {
                        self.env.insert(name, prev);
                    } else {
                        self.env.remove(&name);
                    }
                }
                return Some(res.map(|(result, _updated)| result));
            }
        }

        // 2) Otherwise the qualifier names a class (or compose-time role) reachable
        //    through the inner instance's MRO. Resolve and run the qualified method,
        //    but keep the Mixin as the invocant so the run-time mixin survives the
        //    call (a plain-instance identity writeback would drop the mixin).
        if let Value::Instance {
            class_name: inner_cn,
            ..
        } = inner.as_ref()
        {
            let inst_cn_str = inner_cn.resolve();
            let inst_mro = self.class_mro(&inst_cn_str);
            let in_mro = inst_mro.iter().any(|c| c == qualifier);
            let in_composed_roles = !in_mro
                && inst_mro.iter().any(|c| {
                    self.registry()
                        .class_composed_roles
                        .get(c)
                        .is_some_and(|roles| {
                            roles.iter().any(|r| {
                                r == qualifier
                                    || r.starts_with(qualifier)
                                        && r[qualifier.len()..].starts_with('[')
                            })
                        })
                });
            if !in_mro && !in_composed_roles {
                return Some(Err(RuntimeError::new(format!(
                    "X::Method::InvalidQualifier: Cannot dispatch to method {} on {} because it is not inherited or done by {}",
                    actual_method, qualifier, inst_cn_str
                ))));
            }
            if let Some((_owner, def)) =
                self.resolve_method_with_owner(qualifier, actual_method, &args)
            {
                // Use the inner instance's attributes so the method body can read
                // attributes, but run with the Mixin target as the invocant.
                let attrs_map = if let Value::Instance { attributes, .. } = inner.as_ref() {
                    attributes.to_map()
                } else {
                    HashMap::new()
                };
                let res = self.run_instance_method_resolved(
                    &inst_cn_str,
                    qualifier,
                    def,
                    attrs_map,
                    args,
                    Some(target.clone()),
                );
                return Some(res.map(|(result, _updated)| result));
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
