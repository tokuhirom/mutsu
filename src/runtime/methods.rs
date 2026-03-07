use super::methods_signature::{
    make_method_not_found_error, make_private_permission_error, make_x_immutable_error,
};
use super::*;
use crate::symbol::Symbol;
use crate::value::signature::extract_sig_info;

impl Interpreter {
    fn supply_list_values(
        &mut self,
        attributes: &HashMap<String, Value>,
        wait_until_done: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut items = match attributes.get("values") {
            Some(Value::Array(values, ..)) => values.to_vec(),
            _ => Vec::new(),
        };

        if let Some(Value::Int(supplier_id)) = attributes.get("supplier_id")
            && *supplier_id > 0
        {
            let supplier_id = *supplier_id as u64;
            let live = matches!(attributes.get("live"), Some(Value::Bool(true)));
            let deadline = if wait_until_done && live {
                Some(std::time::Instant::now() + std::time::Duration::from_secs(5))
            } else {
                None
            };
            let mut seen_emitted = 0usize;
            loop {
                let (emitted, done, quit_reason) =
                    crate::runtime::native_methods::supplier_snapshot(supplier_id);
                if emitted.len() > seen_emitted {
                    items.extend_from_slice(&emitted[seen_emitted..]);
                    seen_emitted = emitted.len();
                }
                if let Some(reason) = quit_reason {
                    let message = reason.to_string_value();
                    let mut err = RuntimeError::new(message);
                    err.exception = Some(Box::new(reason));
                    return Err(err);
                }
                if done || deadline.is_none() {
                    break;
                }
                if let Some(limit) = deadline
                    && std::time::Instant::now() >= limit
                {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
        }

        Ok(items)
    }

    fn should_autothread_method(method: &str) -> bool {
        !matches!(
            method,
            "Bool"
                | "so"
                | "WHAT"
                | "^name"
                | "gist"
                | "Str"
                | "defined"
                | "THREAD"
                | "raku"
                | "perl"
                | "first"
                | "grep"
        )
    }

    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Scalar containers are transparent for method dispatch (except .item itself)
        if let Value::Scalar(inner) = target {
            return self.call_method_with_values(*inner, method, args);
        }
        if Self::should_autothread_method(method)
            && let Value::Junction { kind, values } = &target
        {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                results.push(self.call_method_with_values(value.clone(), method, args.clone())?);
            }
            return Ok(Value::junction(kind.clone(), results));
        }
        if Self::should_autothread_method(method)
            && let Some((idx, kind, values)) =
                args.iter().enumerate().find_map(|(idx, arg)| match arg {
                    Value::Junction { kind, values } => Some((idx, kind.clone(), values.clone())),
                    _ => None,
                })
        {
            let mut results = Vec::with_capacity(values.len());
            for value in values.iter() {
                let mut threaded_args = args.clone();
                threaded_args[idx] = value.clone();
                results.push(self.call_method_with_values(
                    target.clone(),
                    method,
                    threaded_args,
                )?);
            }
            return Ok(Value::junction(kind, results));
        }
        if args.is_empty()
            && matches!(method, "raku" | "perl" | "gist")
            && let Value::Junction { kind, values } = &target
        {
            let kind_name = match kind {
                JunctionKind::Any => "any",
                JunctionKind::All => "all",
                JunctionKind::One => "one",
                JunctionKind::None => "none",
            };
            let render_method = if method == "gist" { "gist" } else { "raku" };
            let mut parts = Vec::with_capacity(values.len());
            for value in values.iter() {
                if method == "gist" && matches!(value, Value::Nil) {
                    parts.push("Nil".to_string());
                    continue;
                }
                let rendered =
                    self.call_method_with_values(value.clone(), render_method, vec![])?;
                parts.push(rendered.to_string_value());
            }
            return Ok(Value::str(format!("{}({})", kind_name, parts.join(", "))));
        }
        // Immutable List/Range: push/pop/shift/unshift/append/prepend/splice must throw X::Immutable
        if matches!(
            method,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        ) {
            let is_immutable = match &target {
                Value::Array(_, kind) => !kind.is_real_array(),
                Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. } => true,
                _ => false,
            };
            if is_immutable {
                let typename = match &target {
                    Value::Array(..) => "List",
                    _ => "Range",
                };
                return Err(make_x_immutable_error(method, typename));
            }
        }
        // Buf/Blob.allocate(size, fill?)
        if method == "allocate"
            && let Value::Package(name) = &target
        {
            let cn = name.resolve();
            if cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("buf")
                || cn.starts_with("blob")
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
            {
                return self.buf_allocate(*name, &args);
            }
        }
        let mut args = args;
        if matches!(method, "log" | "exp" | "atan2") {
            for arg in &mut args {
                if !matches!(arg, Value::Instance { .. }) {
                    continue;
                }
                let original = arg.clone();
                if let Ok(coerced) = self
                    .call_method_with_values(original.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(original.clone(), "Bridge", vec![]))
                {
                    *arg = coerced;
                }
            }
        }
        if matches!(method, "arity" | "count")
            && args.is_empty()
            && let Some(sig_info) = extract_sig_info(&target)
        {
            return Ok(if method == "arity" {
                Value::Int(Self::signature_required_positional_count(&sig_info))
            } else {
                Self::signature_count_value(&sig_info)
            });
        }

        if method == "Str"
            && args.is_empty()
            && let Value::Instance { class_name, .. } = &target
        {
            let receiver = class_name.resolve();
            if !self.class_mro(&receiver).iter().any(|cn| cn == "Str") {
                // Non-Str classes should keep normal method lookup/coercion behavior.
                // Only values in Str's inheritance tree get identity .Str by default.
                // This preserves custom .Str methods on unrelated classes.
            } else if let Some((owner, _)) = self.resolve_method_with_owner(&receiver, "Str", &[]) {
                if owner == "Str" {
                    return Ok(target.clone());
                }
            } else {
                return Ok(target.clone());
            }
        }

        // Early check: private method call on non-Instance, non-Package values
        if let Some(private_rest) = method.strip_prefix('!')
            && !matches!(&target, Value::Instance { .. } | Value::Package(_))
        {
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
                    return Err(make_private_permission_error(private_name, owner_class));
                }
            }
            // Unqualified private method on non-Instance — not found
            return Err(make_method_not_found_error(
                private_rest
                    .split_once("::")
                    .map_or(private_rest, |(_o, m)| m),
                "Any",
                true,
            ));
        }

        // Handle qualified method names: Class::method (e.g., $o.Parent::x)
        // Access the attribute as defined in the specified class, not the most-derived one.
        if let Some((qualifier, actual_method)) = method.split_once("::")
            && !method.starts_with('!')
            && let Value::Instance {
                class_name: _,
                attributes,
                ..
            } = &target
            && args.is_empty()
        {
            // Read: look up the attribute in the qualifier class's attribute definitions
            let class_attrs = self.collect_class_attributes(qualifier);
            for (attr_name, is_public, ..) in &class_attrs {
                if *is_public && attr_name == actual_method {
                    return Ok(attributes.get(actual_method).cloned().unwrap_or(Value::Nil));
                }
            }
            // Also try running the actual method on the qualifier class
            if let Some((_owner, method_def)) =
                self.resolve_method_with_owner(qualifier, actual_method, &args)
            {
                let attrs_map = (**attributes).clone();
                let (result, _) =
                    self.run_instance_method(qualifier, attrs_map, actual_method, args, None)?;
                if let Value::Proxy { ref fetcher, .. } = result {
                    let _ = method_def;
                    return self.proxy_fetch(fetcher, None, qualifier, &(**attributes).clone(), 0);
                }
                return Ok(result);
            }
        }

        // Handle method calls on Proxy subclass values (accessing subclass attributes)
        if let Value::Proxy {
            subclass: Some((ref subclass_name, ref subclass_attrs)),
            ..
        } = target
        {
            {
                let attrs = subclass_attrs.lock().unwrap();
                if let Some(val) = attrs.get(method)
                    && args.is_empty()
                {
                    // Store a reference to the subclass attrs and attribute name
                    // so that subsequent mutating method calls (e.g. .push) can
                    // write back to the shared storage.
                    self.pending_proxy_subclass_attr =
                        Some((subclass_attrs.clone(), method.to_string()));
                    return Ok(val.clone());
                }
            }
            // Handle .raku on Proxy subclass
            if method == "raku" || method == "Str" || method == "gist" {
                return Ok(Value::str(format!("{}()", subclass_name.resolve())));
            }
        }

        // Auto-FETCH Proxy values for most method calls (not .VAR, .WHAT, .WHICH, .raku etc.)
        // Decontainerized proxies (from .VAR) are never auto-FETCHed.
        // Auto-FETCH Proxy values for most method calls (not .VAR, .WHAT, .WHICH, .raku etc.)
        // Decontainerized proxies (from .VAR) are never auto-FETCHed.
        if let Value::Proxy {
            ref fetcher,
            decontainerized,
            ..
        } = target
            && !decontainerized
            && !matches!(
                method,
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            )
        {
            let fetched = self.call_sub_value(*fetcher.clone(), vec![target.clone()], true)?;
            return self.call_method_with_values(fetched, method, args);
        }

        // Dispatch temporal n-arg methods (later, earlier, clone, truncated-to, etc.)
        if let Some(result) =
            super::methods_temporal::dispatch_temporal_method(&target, method, &args)
        {
            return result;
        }

        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Format"
        {
            let fmt = attributes
                .get("format")
                .map(Value::to_string_value)
                .unwrap_or_default();
            match method {
                "CALL-ME" => {
                    return Ok(Value::str(super::sprintf::format_sprintf_args(&fmt, &args)));
                }
                "Str" | "gist" => return Ok(Value::str(fmt)),
                _ => {}
            }
        }
        // Handle Match.make method — stores value via make() mechanism
        if let Value::Instance { class_name, .. } = &target
            && class_name == "Match"
            && method == "make"
        {
            let value = args.first().cloned().unwrap_or(Value::Nil);
            self.env.insert("made".to_string(), value.clone());
            self.action_made = Some(value.clone());
            return Ok(value);
        }

        // Handle Routine::WrapHandle .restore() method
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Routine::WrapHandle"
            && method == "restore"
        {
            let sub_id = attributes.get("sub-id").and_then(|v| {
                if let Value::Int(i) = v {
                    Some(*i as u64)
                } else {
                    None
                }
            });
            let handle_id = attributes.get("handle-id").and_then(|v| {
                if let Value::Int(i) = v {
                    Some(*i as u64)
                } else {
                    None
                }
            });
            if let (Some(sub_id), Some(handle_id)) = (sub_id, handle_id) {
                if let Some(chain) = self.wrap_chains.get_mut(&sub_id) {
                    chain.retain(|(hid, _)| *hid != handle_id);
                    if chain.is_empty() {
                        self.cleanup_wrap_name_entries(sub_id);
                    }
                }
                return Ok(Value::Bool(true));
            }
            return Err(RuntimeError::new(
                "Invalid WrapHandle: missing sub-id or handle-id",
            ));
        }
        if method == "gist" && args.is_empty() {
            fn collection_contains_instance(value: &Value) -> bool {
                match value {
                    Value::Instance { .. } => true,
                    _ if value.as_list_items().is_some() => value
                        .as_list_items()
                        .unwrap()
                        .iter()
                        .any(collection_contains_instance),
                    Value::Hash(map) => map.values().any(collection_contains_instance),
                    _ => false,
                }
            }
            fn gist_item(interp: &mut Interpreter, value: &Value) -> String {
                match value {
                    Value::Nil => "Nil".to_string(),
                    _ if value.as_list_items().is_some() => {
                        let inner = value
                            .as_list_items()
                            .unwrap()
                            .iter()
                            .map(|item| gist_item(interp, item))
                            .collect::<Vec<_>>()
                            .join(" ");
                        format!("({inner})")
                    }
                    other => match interp.call_method_with_values(other.clone(), "gist", vec![]) {
                        Ok(Value::Str(s)) => s.to_string(),
                        Ok(v) => v.to_string_value(),
                        Err(_) => other.to_string_value(),
                    },
                }
            }
            if let Some(items) = target.as_list_items()
                && items.iter().any(collection_contains_instance)
            {
                let inner = items
                    .iter()
                    .map(|item| gist_item(self, item))
                    .collect::<Vec<_>>()
                    .join(" ");
                return Ok(Value::str(format!("({inner})")));
            }
        }
        if matches!(method, "max" | "min" | "lines" | "delayed" | "reduce")
            && matches!(&target, Value::Package(name) if name == "Supply")
        {
            return Err(RuntimeError::new(format!(
                "Cannot call .{} on a Supply type object",
                method
            )));
        }
        if method == "delayed"
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
            && args.first().is_some_and(|delay| delay.to_f64() <= 0.0)
        {
            return Ok(target);
        }
        if let Value::Array(items, arr_kind) = &target {
            match (method, args.as_slice()) {
                ("EXISTS-POS", [idx]) => {
                    let index = match idx {
                        Value::Int(i) if *i >= 0 => Some(*i as usize),
                        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                        _ => None,
                    };
                    return Ok(Value::Bool(index.is_some_and(|i| i < items.len())));
                }
                ("ASSIGN-POS", [idx, value]) => {
                    let index = match idx {
                        Value::Int(i) if *i >= 0 => Some(*i as usize),
                        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                        _ => None,
                    };
                    let Some(index) = index else {
                        return Ok(Value::Nil);
                    };

                    if !matches!(value, Value::Nil)
                        && let Some((var_name, constraint)) =
                            self.env.iter().find_map(|(name, bound)| {
                                if let Value::Array(existing, ..) = bound
                                    && std::sync::Arc::ptr_eq(existing, items)
                                    && let Some(constraint) = self.var_type_constraint(name)
                                {
                                    return Some((name.clone(), constraint));
                                }
                                None
                            })
                        && !self.type_matches_value(&constraint, value)
                    {
                        return Err(RuntimeError::new(
                            crate::runtime::utils::type_check_element_error(
                                &var_name,
                                &constraint,
                                value,
                            ),
                        ));
                    }

                    let mut updated = items.to_vec();
                    if index >= updated.len() {
                        updated.resize(index + 1, Value::Package(Symbol::intern("Any")));
                    }
                    updated[index] = value.clone();
                    self.overwrite_array_bindings_by_identity(
                        items,
                        Value::Array(std::sync::Arc::new(updated), *arr_kind),
                    );
                    return Ok(value.clone());
                }
                ("BIND-POS", [_, _]) => {
                    return Err(RuntimeError::new("Cannot bind to a natively typed array"));
                }
                ("DELETE-POS", [_]) => {
                    return Err(RuntimeError::new(
                        "Cannot delete from a natively typed array",
                    ));
                }
                ("clone", _) => {
                    let cloned = items.to_vec();
                    return Ok(Value::Array(Arc::new(cloned), *arr_kind));
                }
                _ => {}
            }
        }

        if let Value::Mixin(inner, mixins) = &target {
            if args.is_empty() {
                if let Some(mixin_val) = mixins.get(method) {
                    return Ok(mixin_val.clone());
                }
                for mixin_val in mixins.values() {
                    if let Value::Enum { enum_type, key, .. } = mixin_val {
                        if method == key.resolve() {
                            return Ok(Value::Bool(true));
                        }
                        if let Some(variants) = self.enum_types.get(&enum_type.resolve())
                            && variants.iter().any(|(variant, _)| variant == method)
                        {
                            return Ok(Value::Bool(false));
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
            let mut role_has_method = false;
            for role_name in role_names {
                let Some(role) = self.roles.get(&role_name).cloned() else {
                    continue;
                };
                let Some(overloads) = role.methods.get(method).cloned() else {
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
                    if def.is_private || !self.method_args_match(&args, &def.param_defs) {
                        continue;
                    }
                    let role_attrs: HashMap<String, Value> = mixins
                        .iter()
                        .filter_map(|(key, value)| {
                            key.strip_prefix("__mutsu_attr__")
                                .map(|attr| (attr.to_string(), value.clone()))
                        })
                        .collect();
                    let method_result = self.run_instance_method_resolved(
                        &role_name,
                        &role_name,
                        def,
                        role_attrs,
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
                    let (result, _updated) = method_result?;
                    return Ok(result);
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
                return Err(super::methods_signature::make_multi_no_match_error(method));
            }
            if method == "can" && args.len() == 1 {
                let method_name = args[0].to_string_value();
                if mixins.contains_key(&method_name)
                    || mixins.contains_key(&format!("__mutsu_attr__{}", method_name))
                {
                    return Ok(Value::Bool(true));
                }
                for role_name in mixins.keys().filter_map(|key| {
                    key.strip_prefix("__mutsu_role__")
                        .map(|name| name.to_string())
                }) {
                    if self
                        .roles
                        .get(&role_name)
                        .is_some_and(|role| role.methods.contains_key(&method_name))
                    {
                        return Ok(Value::Bool(true));
                    }
                }
                return self.call_method_with_values((**inner).clone(), method, args);
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
                    Value::Package(name) => {
                        let n = name.resolve();
                        mixins.contains_key(&n)
                            || mixins.contains_key(&format!("__mutsu_role__{}", n))
                            || inner.does_check(&n)
                    }
                    Value::Str(name) => {
                        mixins.contains_key(name.as_str())
                            || mixins.contains_key(&format!("__mutsu_role__{}", name))
                            || inner.does_check(name)
                    }
                    other => inner.does_check(&other.to_string_value()),
                };
                return Ok(Value::Bool(does));
            }
        }

        // Role type-object method punning: calling a role method on the type object
        // first instantiates the role, then dispatches the method on that instance.
        if method != "new" {
            if let Value::Package(role_name) = &target
                && let Some(role) = self.roles.get(&role_name.resolve())
            {
                let is_public_attr_accessor = args.is_empty()
                    && role
                        .attributes
                        .iter()
                        .any(|(attr_name, is_public, ..)| *is_public && attr_name == method);
                if role.methods.contains_key(method) || is_public_attr_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            } else if let Value::ParametricRole { base_name, .. } = &target
                && let Some(role) = self.roles.get(&base_name.resolve())
            {
                let is_public_attr_accessor = args.is_empty()
                    && role
                        .attributes
                        .iter()
                        .any(|(attr_name, is_public, ..)| *is_public && attr_name == method);
                if role.methods.contains_key(method) || is_public_attr_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            }
        }

        // Enum type-object dispatch for Bool and user enums (e.g. `Order.roll`).
        let enum_type_name: Option<String> = match &target {
            Value::Package(type_name) => Some(type_name.resolve()),
            Value::Str(type_name) if self.enum_types.contains_key(type_name.as_str()) => {
                Some(type_name.to_string())
            }
            Value::Mixin(_, mixins) => mixins.values().find_map(|v| match v {
                Value::Enum { enum_type, .. }
                    if self.enum_types.contains_key(&enum_type.resolve()) =>
                {
                    Some(enum_type.resolve())
                }
                _ => None,
            }),
            _ => None,
        };
        if let Some(type_name) = enum_type_name
            && matches!(method, "pick" | "roll")
            && args.len() <= 1
        {
            let pool: Option<Vec<Value>> = if type_name == "Bool" {
                Some(vec![Value::Bool(false), Value::Bool(true)])
            } else {
                self.enum_types.get(&type_name).map(|variants| {
                    variants
                        .iter()
                        .enumerate()
                        .map(|(index, (key, value))| Value::Enum {
                            enum_type: Symbol::intern(&type_name),
                            key: Symbol::intern(key),
                            value: *value,
                            index,
                        })
                        .collect()
                })
            };
            if let Some(pool) = pool {
                if pool.is_empty() {
                    return Ok(Value::Nil);
                }
                if args.is_empty() {
                    let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                        % pool.len();
                    return Ok(pool[idx].clone());
                }
                let count = match &args[0] {
                    Value::Int(i) if *i > 0 => Some(*i as usize),
                    Value::Int(_) => Some(0),
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                    Value::Whatever => None,
                    Value::Str(s) => s.trim().parse::<i64>().ok().map(|n| n.max(0) as usize),
                    _ => None,
                };
                let Some(count) = count else {
                    if method == "pick" {
                        let mut items = pool.clone();
                        let len = items.len();
                        for i in (1..len).rev() {
                            let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64)
                                as usize
                                % (i + 1);
                            items.swap(i, j);
                        }
                        return Ok(Value::array(items));
                    }
                    let generated = 1024usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64)
                            as usize
                            % pool.len();
                        out.push(pool[idx].clone());
                    }
                    return Ok(Value::LazyList(std::sync::Arc::new(
                        crate::value::LazyList {
                            body: vec![],
                            env: crate::env::Env::new(),
                            cache: std::sync::Mutex::new(Some(out)),
                        },
                    )));
                };
                if method == "pick" {
                    if count == 0 {
                        return Ok(Value::array(Vec::new()));
                    }
                    let mut items = pool.clone();
                    let mut out = Vec::with_capacity(count.min(items.len()));
                    for _ in 0..count.min(items.len()) {
                        let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                            as usize
                            % items.len();
                        out.push(items.swap_remove(idx));
                    }
                    return Ok(Value::array(out));
                }
                if count == 0 {
                    return Ok(Value::array(Vec::new()));
                }
                if count == 1 {
                    let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                        % pool.len();
                    return Ok(pool[idx].clone());
                }
                let mut out = Vec::with_capacity(count);
                for _ in 0..count {
                    let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                        % pool.len();
                    out.push(pool[idx].clone());
                }
                return Ok(Value::array(out));
            }
        }

        // String pick/roll (character-wise), used when VM bypasses native fast path.
        if let Value::Str(s) = &target
            && matches!(method, "pick" | "roll")
            && args.len() <= 1
        {
            let chars: Vec<Value> = s.chars().map(|c| Value::str(c.to_string())).collect();
            if chars.is_empty() {
                return Ok(if args.is_empty() {
                    Value::Nil
                } else {
                    Value::array(Vec::new())
                });
            }
            if args.is_empty() {
                let idx = (crate::builtins::rng::builtin_rand() * chars.len() as f64) as usize
                    % chars.len();
                return Ok(chars[idx].clone());
            }
            let count = match &args[0] {
                Value::Int(i) if *i > 0 => Some(*i as usize),
                Value::Int(_) => Some(0),
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Whatever => None,
                Value::Str(n) => n.trim().parse::<i64>().ok().map(|v| v.max(0) as usize),
                _ => None,
            };
            let Some(count) = count else {
                if method == "pick" {
                    let mut items = chars.clone();
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    return Ok(Value::array(items));
                }
                let generated = 1024usize;
                let mut out = Vec::with_capacity(generated);
                for _ in 0..generated {
                    let idx = (crate::builtins::rng::builtin_rand() * chars.len() as f64) as usize
                        % chars.len();
                    out.push(chars[idx].clone());
                }
                return Ok(Value::LazyList(std::sync::Arc::new(
                    crate::value::LazyList {
                        body: vec![],
                        env: crate::env::Env::new(),
                        cache: std::sync::Mutex::new(Some(out)),
                    },
                )));
            };
            if method == "pick" {
                if count == 0 {
                    return Ok(Value::array(Vec::new()));
                }
                let mut items = chars.clone();
                let mut out = Vec::with_capacity(count.min(items.len()));
                for _ in 0..count.min(items.len()) {
                    let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize
                        % items.len();
                    out.push(items.swap_remove(idx));
                }
                return Ok(Value::array(out));
            }
            if count == 0 {
                return Ok(Value::array(Vec::new()));
            }
            let mut out = Vec::with_capacity(count);
            for _ in 0..count {
                let idx = (crate::builtins::rng::builtin_rand() * chars.len() as f64) as usize
                    % chars.len();
                out.push(chars[idx].clone());
            }
            return Ok(Value::array(out));
        }

        // Skip native fast path for pseudo-methods when called with quoted syntax
        let skip_pseudo = self
            .skip_pseudo_method_native
            .as_ref()
            .is_some_and(|m| m == method);
        if skip_pseudo {
            self.skip_pseudo_method_native = None;
        }
        let is_pseudo_method = matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        );
        let bypass_native_fastpath = skip_pseudo
            || method == "squish"
            || (matches!(method, "max" | "min")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (matches!(method, "list" | "Array" | "Seq")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "Supply"
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supplier"))
            || matches!(&target, Value::Instance { class_name, .. }
                if self.is_native_method(&class_name.resolve(), method))
            || (matches!(&target, Value::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric")))
            || matches!(&target, Value::Instance { class_name, .. } if self.has_user_method(&class_name.resolve(), "Bridge"))
            || (matches!(&target, Value::Instance { class_name, .. } if class_name == "Proc::Async")
                && matches!(
                    method,
                    "start"
                        | "kill"
                        | "write"
                        | "close-stdin"
                        | "ready"
                        | "print"
                        | "say"
                        | "command"
                        | "started"
                        | "w"
                        | "pid"
                        | "stdout"
                        | "stderr"
                        | "Supply"
                ))
            || (matches!(method, "AT-KEY" | "keys")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Stash"))
            || (method == "keys"
                && args.is_empty()
                && (matches!(&target, Value::Hash(_))
                    || matches!(&target, Value::Mixin(inner, _) if matches!(inner.as_ref(), Value::Hash(_)))))
            || (!is_pseudo_method
                && matches!(&target, Value::Instance { class_name, .. } if self.has_user_method(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(&target, Value::Package(class_name) if self.has_user_method(&class_name.resolve(), method)));
        let native_result = if bypass_native_fastpath {
            None
        } else {
            {
                let method_sym = crate::symbol::Symbol::intern(method);
                match args.as_slice() {
                    [] => crate::builtins::native_method_0arg(&target, method_sym),
                    [a] => crate::builtins::native_method_1arg(&target, method_sym, a),
                    [a, b] => crate::builtins::native_method_2arg(&target, method_sym, a, b),
                    _ => None,
                }
            }
        };
        if let Some(result) = native_result {
            return result;
        }

        // Force LazyList and re-dispatch as Seq for methods that need element access.
        // Save/restore the environment to prevent gather body side effects from leaking
        // into the outer scope (preserves laziness semantics).
        if let Value::LazyList(ll) = &target
            && matches!(
                method,
                "list"
                    | "Array"
                    | "Numeric"
                    | "Int"
                    | "first"
                    | "grep"
                    | "map"
                    | "sort"
                    | "reverse"
                    | "join"
                    | "head"
                    | "tail"
                    | "min"
                    | "max"
                    | "minmax"
                    | "sum"
                    | "flat"
                    | "unique"
                    | "squish"
                    | "classify"
                    | "categorize"
                    | "produce"
                    | "rotor"
                    | "batch"
                    | "reduce"
                    | "combinations"
                    | "permutations"
            )
        {
            let saved_env = self.env.clone();
            let items = self.force_lazy_list_bridge(ll)?;
            self.env = saved_env;
            let seq = Value::Seq(std::sync::Arc::new(items));
            return self.call_method_with_values(seq, method, args);
        }

        // Callable introspection and wrappers for routine handles
        if matches!(
            &target,
            Value::Routine { .. } | Value::Sub(_) | Value::WeakSub(_)
        ) && let Some(result) = self.dispatch_callable_method(&target, method, &args)
        {
            return result;
        }

        if method == "VAR"
            && args.is_empty()
            && let Some((source_name, inner)) = Self::varref_parts(&target)
        {
            return self.call_method_mut_with_values(&source_name, inner, "VAR", vec![]);
        }

        if method == "var"
            && args.is_empty()
            && let Some(source_name) = Self::var_target_from_meta_value(&target)
        {
            let source_value = self.env.get(&source_name).cloned().unwrap_or(Value::Nil);
            let mut named = std::collections::HashMap::new();
            named.insert("__mutsu_varref_name".to_string(), Value::str(source_name));
            named.insert("__mutsu_varref_value".to_string(), source_value);
            return Ok(Value::Capture {
                positional: Vec::new(),
                named,
            });
        }

        if method == "join"
            && let Value::LazyList(list) = &target
        {
            let items = self.force_lazy_list_bridge(list)?;
            return self.call_method_with_values(Value::real_array(items), method, args);
        }

        if let Some(meta_method) = method.strip_prefix('^')
            && meta_method != "name"
        {
            let how = self.call_method_with_values(target.clone(), "HOW", vec![])?;
            let mut how_args = Vec::with_capacity(args.len() + 1);
            how_args.push(target.clone());
            how_args.extend(args.clone());
            return self.call_method_with_values(how, meta_method, how_args);
        }

        if let Value::Instance { class_name, .. } = &target
            && (class_name == "Perl6::Metamodel::ClassHOW"
                || class_name == "Perl6::Metamodel::SubsetHOW"
                || class_name == "Perl6::Metamodel::EnumHOW"
                || class_name == "Perl6::Metamodel::CurriedRoleHOW"
                || class_name == "Perl6::Metamodel::ParametricRoleGroupHOW")
            && matches!(
                method,
                "can"
                    | "isa"
                    | "lookup"
                    | "find_method"
                    | "add_method"
                    | "archetypes"
                    | "name"
                    | "ver"
                    | "auth"
                    | "mro"
                    | "mro_unhidden"
                    | "methods"
                    | "candidates"
                    | "concretization"
                    | "curried_role"
                    | "enum_value_list"
                    | "coerce"
            )
        {
            let mut how_args = args.to_vec();
            if let Value::Instance { attributes, .. } = &target
                && !matches!(
                    how_args.first(),
                    Some(Value::Package(_))
                        | Some(Value::Instance { .. })
                        | Some(Value::ParametricRole { .. })
                )
                && let Some(Value::Str(type_name)) = attributes.get("name")
            {
                how_args.insert(0, Value::Package(Symbol::intern(type_name)));
            }
            return self.dispatch_classhow_method(method, how_args);
        }

        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Perl6::Metamodel::Archetypes"
            && method == "composable"
            && args.is_empty()
        {
            return Ok(attributes
                .get("composable")
                .cloned()
                .unwrap_or(Value::Bool(false)));
        }

        // CREATE method: allocate a bare instance without BUILD
        if method == "CREATE" && args.is_empty() {
            match &target {
                Value::CustomType {
                    how,
                    repr,
                    name,
                    id,
                    ..
                } => {
                    return Ok(Value::CustomTypeInstance {
                        type_id: *id,
                        how: how.clone(),
                        repr: repr.clone(),
                        type_name: *name,
                        attributes: std::sync::Arc::new(HashMap::new()),
                        id: crate::value::next_instance_id(),
                    });
                }
                Value::Package(class_name) => {
                    return Ok(Value::make_instance(*class_name, HashMap::new()));
                }
                _ => {}
            }
        }

        // Type-object coercion: Int.^coerce($x), Int(Str).^coerce($x), etc.
        if method == "coerce"
            && args.len() == 1
            && let Value::Package(type_name) = &target
        {
            return self.try_coerce_value_for_constraint(&type_name.resolve(), args[0].clone());
        }

        // Custom type method dispatch: delegate to HOW.find_method
        // Skip pseudo-methods (HOW, WHAT, DEFINITE, REPR, etc.) which are handled natively.
        if !matches!(
            method,
            "HOW"
                | "WHAT"
                | "WHO"
                | "WHY"
                | "WHICH"
                | "WHERE"
                | "DEFINITE"
                | "VAR"
                | "REPR"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "say"
                | "print"
                | "put"
                | "note"
                | "new"
        ) && let Value::CustomType { ref how, .. } | Value::CustomTypeInstance { ref how, .. } =
            target
        {
            let how_clone = *how.clone();
            let found = self.call_method_with_values(
                how_clone,
                "find_method",
                vec![target.clone(), Value::str(method.to_string())],
            );
            if let Ok(callable) = found
                && !matches!(callable, Value::Nil)
            {
                return self.eval_call_on_value(callable, vec![target.clone()]);
            }
        }

        if method == "leave" {
            return self.builtin_leave_method(target, &args);
        }

        // Primary method dispatch by name
        match method {
            "new" if matches!(&target, Value::Package(name) if matches!(name.resolve().as_str(), "IntStr" | "NumStr" | "RatStr" | "ComplexStr")) =>
            {
                let type_name = if let Value::Package(n) = &target {
                    n.resolve()
                } else {
                    unreachable!()
                };
                if args.len() < 2 {
                    return Err(RuntimeError::new(format!(
                        "{}.new requires two arguments (numeric, string)",
                        type_name
                    )));
                }
                let numeric = args[0].clone();
                let string = args[1].to_string_value();
                let mut mixins = std::collections::HashMap::new();
                mixins.insert("Str".to_string(), Value::str(string));
                return Ok(Value::mixin(numeric, mixins));
            }
            "new" if matches!(&target, Value::Package(name) if name == "Failure") => {
                let default_exception = || {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str("Failed".to_string()));
                    Value::make_instance(Symbol::intern("Exception"), attrs)
                };
                let exception = args
                    .first()
                    .cloned()
                    .filter(|v| !matches!(v, Value::Nil))
                    .or_else(|| {
                        self.env
                            .get("!")
                            .cloned()
                            .filter(|v| !matches!(v, Value::Nil))
                    })
                    .unwrap_or_else(default_exception);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("exception".to_string(), exception);
                attrs.insert("handled".to_string(), Value::Bool(false));
                return Ok(Value::make_instance(Symbol::intern("Failure"), attrs));
            }
            "handled"
                if args.is_empty()
                    && matches!(&target, Value::Instance { class_name, .. } if class_name == "Failure") =>
            {
                if let Value::Instance { attributes, .. } = &target {
                    return Ok(attributes
                        .get("handled")
                        .cloned()
                        .unwrap_or(Value::Bool(false)));
                }
            }
            "are" => {
                return self.dispatch_are(target, &args);
            }
            "classify" | "categorize" => {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.extend(args.iter().cloned());
                call_args.push(target);
                return self.builtin_classify(method, &call_args);
            }
            "classify-list" | "categorize-list" => {
                let classify_name = if method == "classify-list" {
                    "classify"
                } else {
                    "categorize"
                };
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.extend(args.iter().cloned());
                call_args.push(Value::Pair("into".to_string(), Box::new(target)));
                return self.builtin_classify(classify_name, &call_args);
            }
            "from-loop" | "from_loop" if matches!(&target, Value::Package(name) if name == "Seq") =>
            {
                let mut positional: Vec<Value> = Vec::new();
                let mut label: Option<String> = None;
                let mut repeat = false;

                for arg in &args {
                    if let Value::Pair(name, value) = arg {
                        if name == "label" {
                            label = Some(value.to_string_value());
                            continue;
                        }
                        if name == "repeat" {
                            repeat = value.truthy();
                            continue;
                        }
                        continue;
                    }
                    if let Value::ValuePair(name, value) = arg {
                        if let Value::Str(key) = name.as_ref() {
                            if key.as_str() == "label" {
                                label = Some(value.to_string_value());
                                continue;
                            }
                            if key.as_str() == "repeat" {
                                repeat = value.truthy();
                                continue;
                            }
                        }
                        continue;
                    }
                    positional.push(arg.clone());
                }

                let Some(body_callable) = positional.first().cloned() else {
                    return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
                };
                let cond_callable = positional.get(1).cloned();
                let step_callable = positional.get(2).cloned();

                let label_matches = |error_label: &Option<String>| {
                    error_label.as_deref() == label.as_deref() || error_label.is_none()
                };

                let mut items = Vec::new();
                let mut first_iteration = true;

                'from_loop: loop {
                    if (!first_iteration || !repeat)
                        && let Some(cond) = cond_callable.clone()
                    {
                        let cond_value = self.call_sub_value(cond, vec![], true)?;
                        if !cond_value.truthy() {
                            break;
                        }
                    }
                    first_iteration = false;

                    'body_redo: loop {
                        match self.call_sub_value(body_callable.clone(), vec![], true) {
                            Ok(value) => {
                                if !matches!(value, Value::Nil) {
                                    items.push(value);
                                }
                                break 'body_redo;
                            }
                            Err(e) if e.is_redo && label_matches(&e.label) => continue 'body_redo,
                            Err(e) if e.is_next && label_matches(&e.label) => break 'body_redo,
                            Err(e) if e.is_last && label_matches(&e.label) => break 'from_loop,
                            Err(e) => return Err(e),
                        }
                    }

                    if let Some(step) = step_callable.clone() {
                        match self.call_sub_value(step, vec![], true) {
                            Ok(_) => {}
                            Err(e) if e.is_next && label_matches(&e.label) => continue 'from_loop,
                            Err(e) if e.is_redo && label_matches(&e.label) => continue 'from_loop,
                            Err(e) if e.is_last && label_matches(&e.label) => break 'from_loop,
                            Err(e) => return Err(e),
                        }
                    }
                }

                return Ok(Value::Seq(std::sync::Arc::new(items)));
            }
            "say" if args.is_empty() => {
                let gist = self.render_gist_value(&target);
                self.write_to_named_handle("$*OUT", &gist, true)?;
                return Ok(Value::Bool(true));
            }
            "print" if args.is_empty() => {
                self.write_to_named_handle("$*OUT", &target.to_string_value(), false)?;
                return Ok(Value::Bool(true));
            }
            "put" if args.is_empty() => {
                self.write_to_named_handle("$*OUT", &target.to_string_value(), true)?;
                return Ok(Value::Bool(true));
            }
            "shape" if args.is_empty() => {
                if let Some(shape) = Self::infer_array_shape(&target) {
                    return Ok(Value::array(
                        shape.into_iter().map(|n| Value::Int(n as i64)).collect(),
                    ));
                }
            }
            "default" if args.is_empty() => {
                if matches!(target, Value::Array(..)) {
                    return Ok(Value::Package(Symbol::intern("Any")));
                }
                if matches!(target, Value::Set(_)) {
                    return Ok(Value::Bool(false));
                }
                if matches!(target, Value::Bag(_)) {
                    return Ok(Value::Int(0));
                }
                if matches!(target, Value::Mix(_)) {
                    return Ok(Value::Num(0.0));
                }
            }
            "note" if args.is_empty() => {
                let content = format!("{}\n", self.render_gist_value(&target));
                self.write_to_named_handle("$*ERR", &content, false)?;
                return Ok(Value::Nil);
            }
            "return-rw" if args.is_empty() => {
                return Ok(target);
            }
            "encode" => {
                let encoding = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "utf-8".to_string());
                let input = target.to_string_value();
                let translated = self.translate_newlines_for_encode(&input);
                let bytes = self.encode_with_encoding(&translated, &encoding)?;
                let bytes_vals: Vec<Value> =
                    bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes_vals));
                let type_name = match encoding.to_lowercase().as_str() {
                    "utf-8" | "utf8" => "utf8",
                    "utf-16" | "utf16" => "utf16",
                    _ => "Buf",
                };
                return Ok(Value::make_instance(Symbol::intern(type_name), attrs));
            }
            "decode" => {
                if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = &target
                    && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
                {
                    let encoding = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "utf-8".to_string());
                    let bytes = if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                        items
                            .iter()
                            .map(|v| match v {
                                Value::Int(i) => *i as u8,
                                _ => 0,
                            })
                            .collect()
                    } else {
                        Vec::new()
                    };
                    let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                    let normalized = self.translate_newlines_for_decode(&decoded);
                    return Ok(Value::str(normalized));
                }
            }
            "subbuf" => {
                if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = &target
                    && (class_name == "Buf" || class_name == "Blob")
                {
                    let bytes = if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                        items.to_vec()
                    } else {
                        Vec::new()
                    };
                    let len = bytes.len();
                    let start_raw = args
                        .first()
                        .map(|v| match v {
                            Value::Int(i) => *i,
                            Value::Num(n) => *n as i64,
                            other => other.to_f64() as i64,
                        })
                        .unwrap_or(0);
                    let start = if start_raw < 0 {
                        len.saturating_sub(start_raw.unsigned_abs() as usize)
                    } else {
                        (start_raw as usize).min(len)
                    };
                    let end = if let Some(length_raw) = args.get(1).map(|v| match v {
                        Value::Int(i) => *i,
                        Value::Num(n) => *n as i64,
                        other => other.to_f64() as i64,
                    }) {
                        if length_raw <= 0 {
                            start
                        } else {
                            start.saturating_add(length_raw as usize).min(len)
                        }
                    } else {
                        len
                    };
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "bytes".to_string(),
                        Value::array(bytes[start..end].to_vec()),
                    );
                    return Ok(Value::make_instance(*class_name, attrs));
                }
            }
            "polymod" => {
                return self.method_polymod(&target, &args);
            }
            "VAR" if args.is_empty() => {
                // Proxy .VAR returns a decontainerized copy (no auto-FETCH on subsequent calls).
                if matches!(&target, Value::Proxy { .. }) {
                    return Ok(Value::proxy_var_object(target, String::new()));
                }
                // Non-container .VAR is identity. Container variables are handled in
                // call_method_mut_with_values via target variable metadata.
                return Ok(target);
            }
            "does" if args.len() == 1 => {
                let role_name = match &args[0] {
                    Value::Package(name) => name.resolve(),
                    Value::Str(name) => name.to_string(),
                    _ => return Ok(Value::Bool(false)),
                };
                return Ok(Value::Bool(target.does_check(&role_name)));
            }
            "start" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let block = args.into_iter().next().unwrap_or(Value::Nil);
                    let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
                    let ret = Value::Promise(promise.clone());
                    let mut thread_interp = self.clone_for_thread();
                    std::thread::spawn(move || {
                        match thread_interp.call_sub_value(block, vec![], false) {
                            Ok(result) => {
                                let output = std::mem::take(&mut thread_interp.output);
                                let stderr = std::mem::take(&mut thread_interp.stderr_output);
                                promise.keep(result, output, stderr);
                            }
                            Err(e) => {
                                let output = std::mem::take(&mut thread_interp.output);
                                let stderr = std::mem::take(&mut thread_interp.stderr_output);
                                let error_val = if let Some(ex) = e.exception {
                                    *ex
                                } else {
                                    Value::str(e.message)
                                };
                                promise.break_with(error_val, output, stderr);
                            }
                        }
                    });
                    return Ok(ret);
                }
                // Thread.start
                if let Value::Package(ref class_name) = target
                    && class_name == "Thread"
                {
                    return self.dispatch_thread_start(&args);
                }
            }
            "in" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let secs = args.first().map(|v| v.to_f64()).unwrap_or(0.0).max(0.0);
                    let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
                    let ret = Value::Promise(promise.clone());
                    std::thread::spawn(move || {
                        if secs > 0.0 {
                            std::thread::sleep(Duration::from_secs_f64(secs));
                        }
                        promise.keep(Value::Bool(true), String::new(), String::new());
                    });
                    return Ok(ret);
                }
            }
            "THREAD" => {
                if let Value::Junction { values, .. } = &target {
                    let code = args.first().cloned().unwrap_or(Value::Nil);
                    for value in values.iter() {
                        self.call_sub_value(code.clone(), vec![value.clone()], false)?;
                    }
                    return Ok(Value::Nil);
                }
            }
            "at" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    // at_time may be an Instant (TAI) or a plain numeric (POSIX).
                    // Convert Instant values to POSIX for delay calculation.
                    let at_time = match args.first() {
                        Some(Value::Instance {
                            class_name,
                            attributes,
                            ..
                        }) if class_name == "Instant" => {
                            let tai = attributes.get("value").map(|v| v.to_f64()).unwrap_or(0.0);
                            crate::builtins::methods_0arg::temporal::instant_to_posix(tai)
                        }
                        Some(v) => v.to_f64(),
                        None => 0.0,
                    };
                    let now = crate::value::current_time_secs_f64();
                    let delay = (at_time - now).max(0.0);
                    let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
                    let ret = Value::Promise(promise.clone());
                    std::thread::spawn(move || {
                        if delay > 0.0 {
                            std::thread::sleep(Duration::from_secs_f64(delay));
                        }
                        promise.keep(Value::Bool(true), String::new(), String::new());
                    });
                    return Ok(ret);
                }
            }
            "kept" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let value = args.into_iter().next().unwrap_or(Value::Bool(true));
                    let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
                    promise.keep(value, String::new(), String::new());
                    return Ok(Value::Promise(promise));
                }
            }
            "broken" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let reason_val = args
                        .into_iter()
                        .next()
                        .unwrap_or_else(|| Value::str_from("Died"));
                    let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
                    promise.break_with(reason_val, String::new(), String::new());
                    return Ok(Value::Promise(promise));
                }
            }
            "allof" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
                    let ret = Value::Promise(promise.clone());
                    let mut promises = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Promise(p) => promises.push(p.clone()),
                            Value::Array(arr, ..) => {
                                for elem in arr.iter() {
                                    if let Value::Promise(p) = elem {
                                        promises.push(p.clone());
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    std::thread::spawn(move || {
                        for p in &promises {
                            p.wait();
                        }
                        promise.keep(Value::Bool(true), String::new(), String::new());
                    });
                    return Ok(ret);
                }
            }
            "anyof" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let promise = SharedPromise::new_with_class(Symbol::intern(&cls));
                    let ret = Value::Promise(promise.clone());
                    let mut promises = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Promise(p) => promises.push(p.clone()),
                            Value::Array(arr, ..) => {
                                for elem in arr.iter() {
                                    if let Value::Promise(p) = elem {
                                        promises.push(p.clone());
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    std::thread::spawn(move || {
                        // Poll until any promise resolves
                        loop {
                            for p in &promises {
                                if p.status() != "Planned" {
                                    promise.keep(Value::Bool(true), String::new(), String::new());
                                    return;
                                }
                            }
                            std::thread::sleep(Duration::from_millis(1));
                        }
                    });
                    return Ok(ret);
                }
            }
            "WHAT" if args.is_empty() => {
                if let Some(info) = self.container_type_metadata(&target) {
                    if let Some(declared) = info.declared_type {
                        return Ok(Value::Package(Symbol::intern(&declared)));
                    }
                    match &target {
                        Value::Array(_, _) => {
                            return Ok(Value::Package(Symbol::intern(&format!(
                                "Array[{}]",
                                info.value_type
                            ))));
                        }
                        Value::Hash(_) => {
                            let name = if let Some(key_type) = info.key_type {
                                format!("Hash[{},{}]", info.value_type, key_type)
                            } else {
                                format!("Hash[{}]", info.value_type)
                            };
                            return Ok(Value::Package(Symbol::intern(&name)));
                        }
                        _ => {}
                    }
                }
                let type_name: &str = match &target {
                    Value::Int(_) => "Int",
                    Value::BigInt(_) => "Int",
                    Value::Num(_) => "Num",
                    Value::Str(_) => "Str",
                    Value::Bool(_) => "Bool",
                    Value::Range(_, _) => "Range",
                    Value::RangeExcl(_, _)
                    | Value::RangeExclStart(_, _)
                    | Value::RangeExclBoth(_, _)
                    | Value::GenericRange { .. } => "Range",
                    Value::Array(_, kind) if kind.is_real_array() => "Array",
                    Value::Array(_, _) => "List",
                    Value::LazyList(_) => "Seq",
                    Value::Hash(_) => "Hash",
                    Value::Rat(_, _) => "Rat",
                    Value::FatRat(_, _) => "FatRat",
                    Value::BigRat(_, _) => "Rat",
                    Value::Complex(_, _) => "Complex",
                    Value::Set(_) => "Set",
                    Value::Bag(_) => "Bag",
                    Value::Mix(_) => "Mix",
                    Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
                    Value::Enum { enum_type, .. } => {
                        return Ok(Value::Package(Symbol::intern(&enum_type.resolve())));
                    }
                    Value::Nil => "Any",
                    Value::Package(name) => {
                        let resolved = name.resolve();
                        let visible = if crate::value::is_internal_anon_type_name(&resolved) {
                            ""
                        } else {
                            &resolved
                        };
                        return Ok(Value::Package(Symbol::intern(visible)));
                    }
                    Value::Routine { is_regex: true, .. } => "Regex",
                    Value::Routine { .. } => "Sub",
                    Value::Sub(data) => match data.env.get("__mutsu_callable_type") {
                        Some(Value::Str(kind)) if kind.as_str() == "Method" => "Method",
                        Some(Value::Str(kind)) if kind.as_str() == "WhateverCode" => "WhateverCode",
                        _ => "Sub",
                    },
                    Value::WeakSub(_) => "Sub",
                    Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
                    Value::Instance { class_name, .. } => {
                        let resolved = class_name.resolve();
                        let visible = if crate::value::is_internal_anon_type_name(&resolved) {
                            ""
                        } else {
                            &resolved
                        };
                        return Ok(Value::Package(Symbol::intern(visible)));
                    }
                    Value::Junction { .. } => "Junction",
                    Value::Regex(_) | Value::RegexWithAdverbs { .. } => "Regex",
                    Value::Version { .. } => "Version",
                    Value::Slip(_) => "Slip",
                    Value::Seq(_) => "Seq",
                    Value::Promise(_) => "Promise",
                    Value::Channel(_) => "Channel",
                    Value::Whatever => "Whatever",
                    Value::HyperWhatever => "HyperWhatever",
                    Value::Capture { .. } => "Capture",
                    Value::Uni { form, .. } => form.as_str(),
                    Value::Mixin(inner, mixins) => {
                        if let Some(allo) = crate::value::types::allomorph_type_name(inner, mixins)
                        {
                            return Ok(Value::Package(Symbol::intern(&allo)));
                        }
                        return self.call_method_with_values(
                            inner.as_ref().clone(),
                            "WHAT",
                            args.clone(),
                        );
                    }
                    Value::Proxy {
                        subclass: Some((name, _)),
                        ..
                    } => {
                        return Ok(Value::Package(*name));
                    }
                    Value::Proxy { .. } => "Proxy",
                    Value::CustomType { name, .. } => {
                        return Ok(Value::Package(*name));
                    }
                    Value::CustomTypeInstance { type_name: tn, .. } => {
                        return Ok(Value::Package(*tn));
                    }
                    Value::ParametricRole {
                        base_name,
                        type_args,
                    } => {
                        let args_str: Vec<String> = type_args
                            .iter()
                            .map(|a| match a {
                                Value::Package(n) => n.resolve(),
                                Value::ParametricRole { .. } => {
                                    // Recursively get the WHAT name for nested parametric roles
                                    if let Ok(Value::Package(n)) =
                                        self.call_method_with_values(a.clone(), "WHAT", Vec::new())
                                    {
                                        // Strip surrounding parens from (Name)
                                        n.resolve()
                                            .trim_start_matches('(')
                                            .trim_end_matches(')')
                                            .to_string()
                                    } else {
                                        a.to_string_value()
                                    }
                                }
                                _ => a.to_string_value(),
                            })
                            .collect();
                        let name = format!("{}[{}]", base_name, args_str.join(","));
                        return Ok(Value::Package(Symbol::intern(&name)));
                    }
                    Value::Scalar(inner) => {
                        return self.call_method_with_values(*inner.clone(), "WHAT", args.clone());
                    }
                };
                let visible_type_name = if crate::value::is_internal_anon_type_name(type_name) {
                    ""
                } else {
                    type_name
                };
                return Ok(Value::Package(Symbol::intern(visible_type_name)));
            }
            "HOW" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(
                        "X::Syntax::Argument::MOPMacro: HOW does not take arguments",
                    ));
                }
                // Return custom HOW for CustomType/CustomTypeInstance
                // Check rebless map first for reblessed instances
                if let Value::CustomTypeInstance { id, .. } = &target
                    && let Some(new_how) = self.rebless_map.get(id).cloned()
                {
                    return Ok(new_how);
                }
                if let Value::CustomType { ref how, .. }
                | Value::CustomTypeInstance { ref how, .. } = target
                {
                    return Ok(*how.clone());
                }
                // Return CurriedRoleHOW for parameterized roles
                if let Value::ParametricRole {
                    base_name,
                    type_args,
                } = &target
                {
                    let args_str = type_args
                        .iter()
                        .map(|v| match v {
                            Value::Package(n) => n.resolve(),
                            other => other.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    let full_name = format!("{}[{}]", base_name, args_str);
                    let mut attrs = HashMap::new();
                    attrs.insert("name".to_string(), Value::str(full_name));
                    return Ok(Value::make_instance(
                        Symbol::intern("Perl6::Metamodel::CurriedRoleHOW"),
                        attrs,
                    ));
                }
                // Return a meta-object (ClassHOW) for any value
                let type_name = match &target {
                    Value::Package(name) => name.resolve(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    _ => {
                        // Get type name via WHAT logic
                        let tn = match &target {
                            Value::Int(_) | Value::BigInt(_) => "Int",
                            Value::Num(_) => "Num",
                            Value::Str(_) => "Str",
                            Value::Bool(_) => "Bool",
                            Value::Hash(_) => "Hash",
                            Value::Array(_, kind) if kind.is_real_array() => "Array",
                            Value::Array(_, _) => "List",
                            Value::Nil => "Any",
                            _ => "Mu",
                        };
                        tn.to_string()
                    }
                };
                // Use appropriate HOW metaclass for each type kind
                let how_name = if self.roles.contains_key(&type_name) && !type_name.contains('[')
                    || matches!(
                        type_name.as_str(),
                        "Numeric"
                            | "Real"
                            | "Stringy"
                            | "Positional"
                            | "Associative"
                            | "Callable"
                            | "Setty"
                            | "Baggy"
                            | "Mixy"
                            | "Dateish"
                            | "Iterable"
                            | "Iterator"
                            | "PositionalBindFailover"
                    ) {
                    "Perl6::Metamodel::ParametricRoleGroupHOW"
                } else if self.enum_types.contains_key(&type_name) {
                    "Perl6::Metamodel::EnumHOW"
                } else if self.subsets.contains_key(&type_name)
                    || matches!(type_name.as_str(), "UInt" | "NativeInt")
                {
                    "Perl6::Metamodel::SubsetHOW"
                } else {
                    "Perl6::Metamodel::ClassHOW"
                };
                let mut attrs = HashMap::new();
                attrs.insert("name".to_string(), Value::str(type_name));
                return Ok(Value::make_instance(Symbol::intern(how_name), attrs));
            }
            "WHO" if args.is_empty() => {
                if let Value::Package(name) = &target {
                    return Ok(self.package_stash_value(&name.resolve()));
                }
                return Ok(Value::Hash(Arc::new(HashMap::new())));
            }
            "WHY" if args.is_empty() => {
                // Return declarator doc comment attached to this type/package/sub
                let keys: Vec<String> = match &target {
                    Value::Package(name) => vec![name.resolve()],
                    Value::Instance { class_name, .. } => vec![class_name.resolve()],
                    Value::Sub(sub_data) => {
                        let mut k = Vec::new();
                        if sub_data.package != "" && sub_data.name != "" {
                            k.push(format!("{}::{}", sub_data.package, sub_data.name));
                        }
                        if sub_data.name != "" {
                            k.push(sub_data.name.resolve());
                        }
                        k
                    }
                    _ => vec![],
                };
                for key in keys {
                    if let Some(doc) = self.doc_comments.get(&key) {
                        return Ok(Value::str(doc.clone()));
                    }
                }
                return Ok(Value::Nil);
            }
            "^name" if args.is_empty() => {
                return Ok(Value::str(match &target {
                    Value::Package(name) => name.resolve(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    Value::Promise(p) => p.class_name().resolve(),
                    Value::ParametricRole {
                        base_name,
                        type_args,
                    } => {
                        let args_str = type_args
                            .iter()
                            .map(|v| match v {
                                Value::Package(n) => n.resolve(),
                                other => other.to_string_value(),
                            })
                            .collect::<Vec<_>>()
                            .join(",");
                        format!("{}[{}]", base_name, args_str)
                    }
                    other => value_type_name(other).to_string(),
                }));
            }
            "^enum_value_list" | "enum_value_list" => {
                let type_name_owned = match &target {
                    Value::Package(name) => Some(name.resolve()),
                    Value::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                let type_name = type_name_owned.as_deref();
                if let Some(type_name) = type_name
                    && let Some(variants) = self.enum_types.get(type_name)
                {
                    let values: Vec<Value> = variants
                        .iter()
                        .enumerate()
                        .map(|(index, (key, val))| Value::Enum {
                            enum_type: Symbol::intern(type_name),
                            key: Symbol::intern(key),
                            value: *val,
                            index,
                        })
                        .collect();
                    return Ok(Value::array(values));
                }
            }
            "enums" => {
                let type_name_owned = match &target {
                    Value::Package(name) => Some(name.resolve()),
                    Value::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                let type_name = type_name_owned.as_deref();
                if let Some(type_name) = type_name
                    && let Some(variants) = self.enum_types.get(type_name)
                {
                    let mut map = HashMap::new();
                    for (k, v) in variants {
                        map.insert(k.clone(), Value::Int(*v));
                    }
                    return Ok(Value::hash(map));
                }
            }
            "invert" => {
                if let Value::Str(type_name) = &target
                    && let Some(variants) = self.enum_types.get(type_name.as_str())
                {
                    let mut result = Vec::new();
                    for (k, v) in variants {
                        result.push(Value::Pair(v.to_string(), Box::new(Value::str(k.clone()))));
                    }
                    return Ok(Value::array(result));
                }
            }
            "subparse" | "parse" | "parsefile" => {
                if let Value::Package(ref package_name) = target {
                    return self.dispatch_package_parse(&package_name.resolve(), method, &args);
                }
            }
            "match" => {
                if args.is_empty() {
                    return Ok(Value::Nil);
                }
                let text = target.to_string_value();
                let mut overlap = false;
                let mut global = false;
                let mut anchored_pos: Option<usize> = None;
                let mut repeat_bounds: Option<(usize, Option<usize>)> = None;
                let mut pattern_arg: Option<&Value> = None;
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        if (key == "ov" || key == "overlap") && value.truthy() {
                            overlap = true;
                        } else if key == "p" || key == "pos" {
                            anchored_pos = Some(value.to_f64() as usize);
                        } else if (key == "g" || key == "global") && value.truthy() {
                            global = true;
                        } else if key == "x" {
                            repeat_bounds = Self::parse_match_repeat_bounds(value);
                        }
                        continue;
                    }
                    if pattern_arg.is_none() {
                        pattern_arg = Some(arg);
                    }
                }
                let Some(pattern) = pattern_arg else {
                    return Ok(Value::Nil);
                };
                let pat: String = match &pattern {
                    Value::Regex(p) => p.to_string(),
                    Value::Str(p) => p.to_string(),
                    _ => return Ok(Value::Nil),
                };
                return {
                    if global || overlap || repeat_bounds.is_some() {
                        let all = self.regex_match_all_with_captures(&pat, &text);
                        let mut selected = if overlap {
                            let mut best_by_start: std::collections::BTreeMap<
                                usize,
                                RegexCaptures,
                            > = std::collections::BTreeMap::new();
                            for capture in all {
                                let key = capture.from;
                                match best_by_start.get(&key) {
                                    Some(existing) if capture.to <= existing.to => {}
                                    _ => {
                                        best_by_start.insert(key, capture);
                                    }
                                }
                            }
                            best_by_start.into_values().collect::<Vec<_>>()
                        } else {
                            self.select_non_overlapping_matches(all)
                        };
                        if let Some((min_required, max_to_return)) = repeat_bounds {
                            let Some(restricted) = Self::select_matches_by_repeat_bounds(
                                selected,
                                min_required,
                                max_to_return,
                            ) else {
                                self.env.insert("/".to_string(), Value::Nil);
                                return Ok(Value::array(Vec::new()));
                            };
                            selected = restricted;
                        }
                        if selected.is_empty() {
                            self.env.insert("/".to_string(), Value::Nil);
                            return Ok(Value::array(Vec::new()));
                        }
                        let matches: Vec<Value> = selected
                            .iter()
                            .map(|c| {
                                Value::make_match_object_full(
                                    c.matched.clone(),
                                    c.from as i64,
                                    c.to as i64,
                                    &c.positional,
                                    &c.named,
                                    &c.named_subcaps,
                                    &c.positional_subcaps,
                                    Some(&text),
                                )
                            })
                            .collect();
                        let result = Value::array(matches);
                        self.env.insert("/".to_string(), result.clone());
                        return Ok(result);
                    }
                    // Use anchored match if :p(N) or :pos(N) is specified
                    let captures = if let Some(pos) = anchored_pos {
                        self.regex_match_with_captures_at(&pat, &text, pos)
                    } else {
                        self.regex_match_with_captures(&pat, &text)
                    };
                    if let Some(captures) = captures {
                        let matched = captures.matched.clone();
                        let from = captures.from as i64;
                        let to = captures.to as i64;
                        // Execute code blocks from regex for side effects
                        self.execute_regex_code_blocks(&captures.code_blocks);
                        let match_obj = Value::make_match_object_full(
                            matched,
                            from,
                            to,
                            &captures.positional,
                            &captures.named,
                            &captures.named_subcaps,
                            &captures.positional_subcaps,
                            Some(&text),
                        );
                        // Set positional capture env vars ($0, $1, ...) from match object
                        if let Value::Instance { ref attributes, .. } = match_obj
                            && let Some(Value::Array(list, _)) = attributes.get("list")
                        {
                            for (i, v) in list.iter().enumerate() {
                                self.env.insert(i.to_string(), v.clone());
                            }
                        }
                        // Set named capture env vars from match object
                        if let Value::Instance { ref attributes, .. } = match_obj
                            && let Some(Value::Hash(named_hash)) = attributes.get("named")
                        {
                            for (k, v) in named_hash.iter() {
                                self.env.insert(format!("<{}>", k), v.clone());
                            }
                        }
                        self.env.insert("/".to_string(), match_obj.clone());
                        Ok(match_obj)
                    } else {
                        Ok(Value::Nil)
                    }
                };
            }
            "subst" => {
                return self.dispatch_subst(target, &args);
            }
            "comb" if args.len() == 1 => {
                let text = target.to_string_value();
                match &args[0] {
                    Value::Int(n) if *n > 0 => {
                        let chunk_size = *n as usize;
                        let chars: Vec<char> = text.chars().collect();
                        let result: Vec<Value> = chars
                            .chunks(chunk_size)
                            .map(|chunk| Value::str(chunk.iter().collect()))
                            .collect();
                        return Ok(Value::array(result));
                    }
                    Value::Str(needle) => {
                        if needle.is_empty() {
                            let chars = text
                                .chars()
                                .map(|ch| Value::str(ch.to_string()))
                                .collect::<Vec<_>>();
                            return Ok(Value::array(chars));
                        }
                        let mut result = Vec::new();
                        let mut offset = 0usize;
                        while offset <= text.len() {
                            let Some(pos) = text[offset..].find(needle.as_str()) else {
                                break;
                            };
                            let start = offset + pos;
                            let end = start + needle.len();
                            result.push(Value::str(text[start..end].to_string()));
                            offset = end;
                        }
                        return Ok(Value::array(result));
                    }
                    Value::Regex(pat) => {
                        let matches = self.regex_find_all(pat, &text);
                        let chars: Vec<char> = text.chars().collect();
                        let result: Vec<Value> = matches
                            .iter()
                            .map(|(start, end)| {
                                let s: String = chars[*start..*end].iter().collect();
                                Value::str(s)
                            })
                            .collect();
                        return Ok(Value::array(result));
                    }
                    _ => {
                        let pattern = args[0].to_string_value();
                        let matches = self.regex_find_all(&pattern, &text);
                        let chars: Vec<char> = text.chars().collect();
                        let result: Vec<Value> = matches
                            .iter()
                            .map(|(start, end)| {
                                let s: String = chars[*start..*end].iter().collect();
                                Value::str(s)
                            })
                            .collect();
                        return Ok(Value::array(result));
                    }
                }
            }
            "IO" if args.is_empty() => {
                let s = target.to_string_value();
                if s.contains('\0') {
                    return Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    ));
                }
                return Ok(self.make_io_path_instance(&s));
            }
            "contains" => {
                return self.dispatch_contains(target, &args);
            }
            "starts-with" => {
                return self.dispatch_starts_with(target, &args);
            }
            "ends-with" => {
                return self.dispatch_ends_with(target, &args);
            }
            "index" => {
                return self.dispatch_index(target, &args);
            }
            "rindex" => {
                return self.dispatch_rindex(target, &args);
            }
            "substr-eq" => {
                return self.dispatch_substr_eq(target, &args);
            }
            "substr" => {
                return self.dispatch_substr(target, &args);
            }
            "trans" => {
                return self.dispatch_trans(target, &args);
            }
            // Trig methods on user-defined types: coerce via .Numeric or .Bridge
            "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sec" | "cosec"
            | "cotan" | "asec" | "acosec" | "acotan" | "sinh" | "cosh" | "tanh" | "sech"
            | "cosech" | "cotanh" | "asinh" | "acosh" | "atanh" | "asech" | "acosech"
            | "acotanh"
                if matches!(target, Value::Instance { .. }) =>
            {
                // Try .Numeric first, then .Bridge
                let coerced = if let Ok(v) =
                    self.call_method_with_values(target.clone(), "Numeric", vec![])
                {
                    v
                } else if let Ok(v) = self.call_method_with_values(target.clone(), "Bridge", vec![])
                {
                    v
                } else {
                    return Err(RuntimeError::new(format!(
                        "Cannot coerce to numeric for {}",
                        method
                    )));
                };
                return self.call_method_with_values(coerced, method, args);
            }
            // .atan2(Instance) — coerce Instance arg
            "atan2" if args.len() == 1 && matches!(&args[0], Value::Instance { .. }) => {
                let coerced_arg = self
                    .call_method_with_values(args[0].clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(args[0].clone(), "Bridge", vec![]))?;
                return self.call_method_with_values(target, "atan2", vec![coerced_arg]);
            }
            "Seq" if args.is_empty() => {
                return Ok(match target {
                    Value::Seq(_) => target,
                    Value::Array(items, ..) => Value::Seq(items),
                    Value::Slip(items) => Value::Seq(items),
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Supply" => {
                        let values = if let Some(on_demand_cb) =
                            attributes.get("on_demand_callback")
                        {
                            let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                                let mut a = HashMap::new();
                                a.insert("emitted".to_string(), Value::array(Vec::new()));
                                a.insert("done".to_string(), Value::Bool(false));
                                a
                            });
                            self.supply_emit_buffer.push(Vec::new());
                            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                            self.supply_emit_buffer.pop().unwrap_or_default()
                        } else if attributes.get("values").is_some() {
                            self.supply_list_values(&attributes, true)?
                        } else {
                            Vec::new()
                        };
                        Value::Seq(std::sync::Arc::new(values))
                    }
                    Value::LazyList(ll) => {
                        let items = Self::value_to_list(&Value::LazyList(ll));
                        Value::Seq(std::sync::Arc::new(items))
                    }
                    other @ (Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. }) => {
                        let items = Self::value_to_list(&other);
                        Value::Seq(std::sync::Arc::new(items))
                    }
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if {
                        let cn = class_name.resolve();
                        cn == "Buf"
                            || cn == "Blob"
                            || cn == "utf8"
                            || cn == "utf16"
                            || cn.starts_with("Buf[")
                            || cn.starts_with("Blob[")
                            || cn.starts_with("buf")
                            || cn.starts_with("blob")
                    } =>
                    {
                        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                            Value::Seq(items.clone())
                        } else {
                            Value::Seq(std::sync::Arc::new(Vec::new()))
                        }
                    }
                    other => Value::Seq(std::sync::Arc::new(vec![other])),
                });
            }
            "list" | "Array" if args.is_empty() => {
                if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = &target
                    && class_name == "Supply"
                {
                    let values = self.supply_list_values(attributes, true)?;
                    return Ok(Value::array(values));
                }
            }
            "List" if args.is_empty() => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                {
                    let cn = class_name.resolve();
                    if cn == "Buf"
                        || cn == "Blob"
                        || cn == "utf8"
                        || cn == "utf16"
                        || cn.starts_with("Buf[")
                        || cn.starts_with("Blob[")
                        || cn.starts_with("buf")
                        || cn.starts_with("blob")
                    {
                        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                            return Ok(Value::Array(items.clone(), crate::value::ArrayKind::List));
                        }
                        return Ok(Value::Array(
                            std::sync::Arc::new(Vec::new()),
                            crate::value::ArrayKind::List,
                        ));
                    }
                }
                let items = Self::value_to_list(&target);
                return Ok(Value::Array(
                    std::sync::Arc::new(items),
                    crate::value::ArrayKind::List,
                ));
            }
            "Set" | "SetHash" if args.is_empty() => {
                return self.dispatch_to_set(target);
            }
            "Bag" | "BagHash" if args.is_empty() => {
                return self.dispatch_to_bag(target);
            }
            "Mix" | "MixHash" if args.is_empty() => {
                let result = self.dispatch_to_mix(target)?;
                if method == "MixHash" {
                    self.register_container_type_metadata(
                        &result,
                        ContainerTypeInfo {
                            value_type: "Real".to_string(),
                            key_type: None,
                            declared_type: Some("MixHash".to_string()),
                        },
                    );
                }
                return Ok(result);
            }
            "Setty" | "Baggy" | "Mixy" if args.is_empty() => {
                // Role-ish quant hash family conversion on type objects.
                // Raku maps Set/Bag/Mix families to the corresponding family,
                // preserving hash flavor for *Hash type objects.
                let source_type = match &target {
                    Value::Package(name) => Some(name.resolve()),
                    Value::Set(_) => Some("Set".to_string()),
                    Value::Bag(_) => Some("Bag".to_string()),
                    Value::Mix(_) => Some("Mix".to_string()),
                    _ => None,
                };
                if let Some(source_type) = source_type
                    && matches!(
                        source_type.as_str(),
                        "Set" | "SetHash" | "Bag" | "BagHash" | "Mix" | "MixHash"
                    )
                {
                    let hashy = matches!(source_type.as_str(), "SetHash" | "BagHash" | "MixHash");
                    let mapped = match method {
                        "Setty" => {
                            if hashy {
                                "SetHash"
                            } else {
                                "Set"
                            }
                        }
                        "Baggy" => {
                            if hashy {
                                "BagHash"
                            } else {
                                "Bag"
                            }
                        }
                        _ => {
                            if hashy {
                                "MixHash"
                            } else {
                                "Mix"
                            }
                        }
                    };
                    return Ok(Value::Package(Symbol::intern(mapped)));
                }
            }
            "Map" | "Hash" if args.is_empty() => {
                // Type objects return the corresponding type object
                if matches!(&target, Value::Package(_)) {
                    return Ok(Value::Package(Symbol::intern(method)));
                }
                if method == "Map" {
                    return self.dispatch_to_map(target);
                }
                return self.dispatch_to_hash(target);
            }
            "hash" if args.is_empty() => {
                return self.dispatch_to_hash(target);
            }
            "any" | "all" | "one" | "none" if args.is_empty() => {
                let kind = match method {
                    "any" => JunctionKind::Any,
                    "all" => JunctionKind::All,
                    "one" => JunctionKind::One,
                    _ => JunctionKind::None,
                };
                let values = Self::value_to_list(&target);
                return Ok(Value::junction(kind, values));
            }
            "iterator" if args.is_empty() => {
                if matches!(&target, Value::Instance { class_name, .. } if class_name == "Iterator")
                {
                    return Ok(target);
                }
                if let Value::Seq(items) = &target {
                    let seq_id = std::sync::Arc::as_ptr(items) as usize;
                    if let Some(meta) = self.squish_iterator_meta.remove(&seq_id) {
                        for key in meta.revert_remove {
                            self.env.remove(&key);
                        }
                        for (key, value) in meta.revert_values {
                            self.env.insert(key, value);
                        }
                        let mut attrs = HashMap::new();
                        attrs.insert("squish_source".to_string(), Value::array(meta.source_items));
                        attrs.insert("squish_as".to_string(), meta.as_func.unwrap_or(Value::Nil));
                        attrs.insert(
                            "squish_with".to_string(),
                            meta.with_func.unwrap_or(Value::Nil),
                        );
                        attrs.insert("squish_scan_index".to_string(), Value::Int(0));
                        attrs.insert("squish_prev_key".to_string(), Value::Nil);
                        attrs.insert("squish_initialized".to_string(), Value::Bool(false));
                        return Ok(Value::make_instance(Symbol::intern("Iterator"), attrs));
                    }
                }
                let items = crate::runtime::utils::value_to_list(&target);
                let mut attrs = HashMap::new();
                attrs.insert("items".to_string(), Value::array(items));
                attrs.insert("index".to_string(), Value::Int(0));
                return Ok(Value::make_instance(Symbol::intern("Iterator"), attrs));
            }
            "produce" => {
                let callable = args
                    .first()
                    .cloned()
                    .ok_or_else(|| RuntimeError::new("produce expects a callable"))?;
                if !matches!(
                    target,
                    Value::Array(_, _)
                        | Value::Seq(_)
                        | Value::Slip(_)
                        | Value::LazyList(_)
                        | Value::Range(_, _)
                        | Value::RangeExcl(_, _)
                        | Value::RangeExclStart(_, _)
                        | Value::RangeExclBoth(_, _)
                        | Value::GenericRange { .. }
                        | Value::Hash(_)
                ) {
                    return Ok(target);
                }
                return self.call_function("produce", vec![callable, target]);
            }
            "reduce" => {
                let callable = args
                    .first()
                    .cloned()
                    .ok_or_else(|| RuntimeError::new("reduce expects a callable"))?;
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    if !matches!(
                        callable,
                        Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                    ) {
                        return Err(RuntimeError::new("must be code if specified"));
                    }
                    if attributes.get("supplier_id").is_some()
                        || attributes.get("on_demand_callback").is_some()
                    {
                        let mut reduce_attrs = HashMap::new();
                        reduce_attrs.insert("values".to_string(), Value::array(Vec::new()));
                        reduce_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                        reduce_attrs.insert("live".to_string(), Value::Bool(false));
                        reduce_attrs.insert("reduce_source".to_string(), target);
                        reduce_attrs.insert("reduce_callable".to_string(), callable);
                        return Ok(Value::make_instance(Symbol::intern("Supply"), reduce_attrs));
                    }
                    let items = self.supply_list_values(attributes, true)?;
                    let reduced = self.reduce_items(callable, items)?;
                    let values = if matches!(reduced, Value::Nil) {
                        Vec::new()
                    } else {
                        vec![reduced]
                    };
                    let mut reduce_attrs = HashMap::new();
                    reduce_attrs.insert("values".to_string(), Value::array(values));
                    reduce_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    reduce_attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), reduce_attrs));
                }
                let items = Self::value_to_list(&target);
                return self.reduce_items(callable, items);
            }
            "map" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    let mapper = args.first().cloned().unwrap_or(Value::Nil);
                    let source_values =
                        if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                            let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                                let mut a = HashMap::new();
                                a.insert("emitted".to_string(), Value::array(Vec::new()));
                                a.insert("done".to_string(), Value::Bool(false));
                                a
                            });
                            self.supply_emit_buffer.push(Vec::new());
                            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                            self.supply_emit_buffer.pop().unwrap_or_default()
                        } else {
                            attributes
                                .get("values")
                                .and_then(|v| {
                                    if let Value::Array(items, ..) = v {
                                        Some(items.to_vec())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or_default()
                        };

                    let mut mapped_values = Vec::with_capacity(source_values.len());
                    for value in source_values {
                        mapped_values.push(self.call_sub_value(
                            mapper.clone(),
                            vec![value],
                            true,
                        )?);
                    }

                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(mapped_values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert(
                        "live".to_string(),
                        attributes
                            .get("live")
                            .cloned()
                            .unwrap_or(Value::Bool(false)),
                    );
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }
                let items = Self::value_to_list(&target);
                return self.eval_map_over_items(args.first().cloned(), items);
            }
            "max" | "min" => {
                if matches!(target, Value::Hash(_)) {
                    let mut call_args = vec![target.clone()];
                    if let Some(first) = args.first() {
                        if matches!(
                            first,
                            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                        ) {
                            call_args.push(Value::Pair("by".to_string(), Box::new(first.clone())));
                        } else {
                            call_args.extend(args.clone());
                        }
                    }
                    return if method == "max" {
                        self.builtin_max(&call_args)
                    } else {
                        self.builtin_min(&call_args)
                    };
                }
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_running_extrema(target, method, &args);
                }
            }
            "minpairs" | "maxpairs" if args.is_empty() => {
                return self.dispatch_minmaxpairs(target, method);
            }
            "pop" if args.is_empty() => {
                // pop on a non-variable array value (e.g. [1,2,3].pop)
                match target {
                    Value::Array(mut items, ..) => {
                        let items_mut = Arc::make_mut(&mut items);
                        return Ok(if items_mut.is_empty() {
                            make_empty_array_failure("pop")
                        } else {
                            items_mut.pop().unwrap_or(Value::Nil)
                        });
                    }
                    _ => {
                        return Ok(make_empty_array_failure("pop"));
                    }
                }
            }
            "sort" => {
                return self.dispatch_sort(target, &args);
            }
            "unique" => {
                // Supply.unique is handled by native_supply
                if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
                {
                    return self.dispatch_unique(target, &args);
                }
            }
            "squish" => {
                return self.dispatch_squish(target, &args);
            }
            "collate" if args.is_empty() => {
                return self.dispatch_collate(target);
            }
            "take" if args.is_empty() => {
                self.take_value(target.clone());
                return Ok(target);
            }
            "rotor" => {
                return self.dispatch_rotor(target, &args);
            }
            "from-list" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    let mut values = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Array(items, kind) if !kind.is_itemized() => {
                                values.extend(items.iter().cloned());
                            }
                            Value::Range(..)
                            | Value::RangeExcl(..)
                            | Value::RangeExclStart(..)
                            | Value::RangeExclBoth(..)
                            | Value::GenericRange { .. } => {
                                values.extend(Self::value_to_list(arg));
                            }
                            Value::Slip(items) | Value::Seq(items) => {
                                values.extend(items.iter().cloned());
                            }
                            other => values.push(other.clone()),
                        }
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }
            }
            "repository-for-name" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "CompUnit::RepositoryRegistry"
                {
                    let name = args.first().map(Value::to_string_value).unwrap_or_default();
                    if let Some(prefix) = name.strip_prefix("file#") {
                        let new_args = vec![Value::Pair(
                            "prefix".to_string(),
                            Box::new(Value::str(prefix.to_string())),
                        )];
                        return self.call_method_with_values(
                            Value::Package(Symbol::intern("CompUnit::Repository::FileSystem")),
                            "new",
                            new_args,
                        );
                    }
                    return Ok(Value::Nil);
                }
            }
            "signal" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(Vec::new()));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(true));
                    attrs.insert("signals".to_string(), Value::array(args.clone()));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }
            }
            "on-demand" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    let callback = args.first().cloned().unwrap_or(Value::Nil);
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(Vec::new()));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    attrs.insert("on_demand_callback".to_string(), callback);
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }
                // on-demand called on a Supply instance should die
                if let Value::Instance { ref class_name, .. } = target
                    && class_name == "Supply"
                {
                    return Err(RuntimeError::new(
                        "Cannot call on-demand on a Supply instance",
                    ));
                }
            }
            "find" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Encoding::Registry"
                {
                    return self.dispatch_encoding_registry_find(&args);
                }
            }
            "register" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Encoding::Registry"
                {
                    return self.dispatch_encoding_registry_register(&args);
                }
            }
            "connect" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::INET"
                {
                    return self.dispatch_socket_connect(&args);
                }
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::Async"
                {
                    return self.dispatch_socket_async_connect(&args);
                }
            }
            "listen" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::Async"
                {
                    return self.dispatch_socket_async_listen(&args);
                }
            }
            "new" => {
                return self.dispatch_new(target, args);
            }
            "bless" => {
                // self.bless(:attr1($val1), :attr2($val2), ...)
                // Creates a new instance of the invocant's class with attributes from named args
                let class_name = match &target {
                    Value::Package(name) => *name,
                    Value::Instance { class_name, .. } => *class_name,
                    _ => {
                        return Err(RuntimeError::new(
                            "bless can only be called on a class or instance",
                        ));
                    }
                };
                if matches!(
                    class_name.resolve().as_str(),
                    "Sub" | "Routine" | "Method" | "Code" | "Block"
                ) {
                    return Err(RuntimeError::new(
                        "getcodename requires a concrete code object",
                    ));
                }
                // Initialize with default attribute values
                let mut attributes = HashMap::new();
                if self.classes.contains_key(&class_name.resolve()) {
                    for (attr_name, _is_public, default, _is_rw, _, _, _) in
                        self.collect_class_attributes(&class_name.resolve())
                    {
                        let val = if let Some(expr) = default {
                            self.eval_block_value(&[Stmt::Expr(expr)])?
                        } else {
                            Value::Nil
                        };
                        attributes.insert(attr_name, val);
                    }
                }
                // Override with named args from bless call
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        attributes.insert(key.clone(), *value.clone());
                    }
                }
                // Run BUILD/TWEAK if defined
                if self.class_has_method(&class_name.resolve(), "BUILD") {
                    let (_v, updated) = self.run_instance_method(
                        &class_name.resolve(),
                        attributes.clone(),
                        "BUILD",
                        Vec::new(),
                        Some(Value::make_instance(class_name, attributes.clone())),
                    )?;
                    attributes = updated;
                }
                if self.class_has_method(&class_name.resolve(), "TWEAK") {
                    let (_v, updated) = self.run_instance_method(
                        &class_name.resolve(),
                        attributes.clone(),
                        "TWEAK",
                        Vec::new(),
                        Some(Value::make_instance(class_name, attributes.clone())),
                    )?;
                    attributes = updated;
                }
                return Ok(Value::make_instance(class_name, attributes));
            }
            "now" if args.is_empty() => {
                if let Value::Package(ref class_name) = target
                    && class_name == "DateTime"
                {
                    use crate::builtins::methods_0arg::temporal;
                    let secs = crate::value::current_time_secs_f64();
                    let total_i = secs.floor() as i64;
                    let frac = secs - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    let h = day_secs / 3600;
                    let mi = (day_secs % 3600) / 60;
                    let s = (day_secs % 60) as f64 + frac;
                    return Ok(temporal::make_datetime(y, m, d, h, mi, s, 0));
                }
            }
            "today" if args.is_empty() => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Date"
                {
                    use crate::builtins::methods_0arg::temporal;
                    let secs = crate::value::current_time_secs_f64() as i64;
                    let epoch_days = secs.div_euclid(86400);
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    return Ok(temporal::make_date(y, m, d));
                }
            }
            "grab" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    let values = match attributes.get("values") {
                        Some(Value::Array(items, ..)) => items.to_vec(),
                        _ => Vec::new(),
                    };
                    let func = args.first().cloned().unwrap_or(Value::Nil);
                    let values_list = Value::array(values);
                    let result = self.eval_call_on_value(func, vec![values_list])?;
                    let result_values = Self::value_to_list(&result);
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(result_values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }
                // Class-level Supply.grab should die
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    return Err(RuntimeError::new(
                        "Cannot call .grab on a Supply type object",
                    ));
                }
            }
            "skip" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    let n = if args.is_empty() {
                        1usize
                    } else {
                        let arg = &args[0];
                        match arg {
                            Value::Int(i) => *i as usize,
                            Value::Num(f) => *f as usize,
                            Value::Str(s) => s.parse::<usize>().map_err(|_| {
                                RuntimeError::new(format!(
                                    "X::Str::Numeric: Cannot convert string '{}' to a number",
                                    s
                                ))
                            })?,
                            _ => arg.to_f64() as usize,
                        }
                    };
                    let values = match attributes.get("values") {
                        Some(Value::Array(items, ..)) => {
                            items.iter().skip(n).cloned().collect::<Vec<_>>()
                        }
                        _ => Vec::new(),
                    };
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }
            }
            "join" if args.len() <= 1 => {
                if matches!(
                    target,
                    Value::Array(..)
                        | Value::Seq(..)
                        | Value::Slip(..)
                        | Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..)
                        | Value::GenericRange { .. }
                ) {
                    let sep = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let joined = Self::value_to_list(&target)
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    return Ok(Value::str(joined));
                }
            }
            "grep" => {
                return self.dispatch_grep(target, &args);
            }
            "eager" if args.is_empty() => {
                return match target {
                    Value::LazyList(list) => Ok(Value::array(self.force_lazy_list_bridge(&list)?)),
                    Value::Array(..) | Value::Seq(..) | Value::Slip(..) => Ok(target),
                    Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. } => {
                        Ok(Value::array(crate::runtime::utils::value_to_list(&target)))
                    }
                    other => Ok(other),
                };
            }
            "is-lazy" if args.is_empty() => {
                let value_is_lazy = |v: &Value| match v {
                    Value::LazyList(_) => true,
                    Value::Range(_, end)
                    | Value::RangeExcl(_, end)
                    | Value::RangeExclStart(_, end)
                    | Value::RangeExclBoth(_, end) => *end == i64::MAX,
                    Value::GenericRange { end, .. } => {
                        let end_f = end.to_f64();
                        end_f.is_infinite() && end_f.is_sign_positive()
                    }
                    _ => false,
                };
                let is_lazy = if value_is_lazy(&target) {
                    true
                } else if let Some(items) = target.as_list_items() {
                    items.iter().any(value_is_lazy)
                } else {
                    false
                };
                return Ok(Value::Bool(is_lazy));
            }
            "first" if !args.is_empty() => {
                return self.dispatch_first(target, &args);
            }
            "first" if args.is_empty() => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_first(target, &args);
                }
            }
            "tree" if !args.is_empty() => {
                return self.dispatch_tree(target, &args);
            }
            "keys" if args.is_empty() => match target {
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => {
                    let keys = match attributes.get("symbols") {
                        Some(Value::Hash(map)) => {
                            map.keys().cloned().map(Value::str).collect::<Vec<Value>>()
                        }
                        _ => Vec::new(),
                    };
                    return Ok(Value::array(keys));
                }
                Value::Hash(ref map) => {
                    if let Some(info) = self.container_type_metadata(&target)
                        && let Some(key_constraint) = info.key_type
                    {
                        let mut keys = Vec::with_capacity(map.len());
                        for key in map.keys() {
                            let key_value = Value::str(key.clone());
                            let coerced = self
                                .try_coerce_value_for_constraint(&key_constraint, key_value)
                                .unwrap_or_else(|_| Value::str(key.clone()));
                            keys.push(coerced);
                        }
                        return Ok(Value::array(keys));
                    }
                    let keys = map.keys().cloned().map(Value::str).collect::<Vec<Value>>();
                    return Ok(Value::array(keys));
                }
                Value::Mixin(inner, _) if matches!(inner.as_ref(), Value::Hash(_)) => {
                    return self.call_method_with_values(inner.as_ref().clone(), "keys", vec![]);
                }
                _ => {}
            },
            "values" if args.is_empty() => match target {
                Value::Capture { positional, named } => {
                    let mut vals = positional.clone();
                    vals.extend(named.values().cloned());
                    return Ok(Value::array(vals));
                }
                Value::Array(items, ..) => return Ok(Value::array(items.to_vec())),
                Value::Hash(map) => return Ok(Value::array(map.values().cloned().collect())),
                Value::Pair(_, value) => return Ok(Value::array(vec![*value.clone()])),
                Value::ValuePair(_, value) => return Ok(Value::array(vec![*value.clone()])),
                _ => return Ok(Value::array(Vec::new())),
            },
            "AT-KEY" if args.len() == 1 => match (&target, &args[0]) {
                (
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    },
                    idx,
                ) if class_name == "Stash" => {
                    if let Some(Value::Hash(symbols)) = attributes.get("symbols") {
                        let stash_lookup = |raw_key: &str| {
                            if let Some(value) = symbols.get(raw_key) {
                                return Some(value.clone());
                            }
                            if !raw_key.starts_with('$')
                                && !raw_key.starts_with('@')
                                && !raw_key.starts_with('%')
                                && !raw_key.starts_with('&')
                            {
                                let scalar = format!("${raw_key}");
                                if let Some(value) = symbols.get(&scalar) {
                                    return Some(value.clone());
                                }
                            }
                            None
                        };

                        if let Value::Array(items, ..) = idx {
                            let values = items
                                .iter()
                                .map(|item| {
                                    let key = item.to_string_value();
                                    stash_lookup(&key).unwrap_or(Value::Nil)
                                })
                                .collect::<Vec<_>>();
                            return Ok(Value::array(values));
                        }

                        let key = idx.to_string_value();
                        if let Some(value) = stash_lookup(&key) {
                            return Ok(value);
                        }
                    }
                    return Ok(Value::Nil);
                }
                (Value::Pair(key, value), idx) => {
                    if key == &idx.to_string_value() {
                        return Ok(*value.clone());
                    }
                    return Ok(Value::Nil);
                }
                (Value::ValuePair(key, value), idx) => {
                    if key.to_string_value() == idx.to_string_value() {
                        return Ok(*value.clone());
                    }
                    return Ok(Value::Nil);
                }
                _ => {}
            },
            "rotate" => {
                return self.dispatch_rotate(target, &args);
            }
            _ => {}
        }

        // Enum dispatch
        if let Value::Enum {
            enum_type,
            key,
            value,
            index,
        } = &target
        {
            match method {
                "key" => return Ok(Value::str(key.resolve())),
                "value" | "Int" | "Numeric" => return Ok(Value::Int(*value)),
                "WHAT" => return Ok(Value::Package(*enum_type)),
                "raku" | "perl" => {
                    return Ok(Value::str(format!("{}::{}", enum_type, key)));
                }
                "gist" | "Str" => return Ok(Value::str(key.resolve())),
                "kv" => {
                    return Ok(Value::array(vec![
                        Value::str(key.resolve()),
                        Value::Int(*value),
                    ]));
                }
                "pair" => {
                    return Ok(Value::Pair(key.resolve(), Box::new(Value::Int(*value))));
                }
                "ACCEPTS" => {
                    if args.is_empty() {
                        return Err(RuntimeError::new("ACCEPTS requires an argument"));
                    }
                    let other = &args[0];
                    return Ok(Value::Bool(match other {
                        Value::Enum {
                            enum_type: other_type,
                            key: other_key,
                            ..
                        } => enum_type == other_type && key == other_key,
                        _ => false,
                    }));
                }
                "pred" => {
                    if *index == 0 {
                        return Ok(Value::Nil);
                    }
                    if let Some(variants) = self.enum_types.get(&enum_type.resolve())
                        && let Some((prev_key, prev_val)) = variants.get(index - 1)
                    {
                        return Ok(Value::Enum {
                            enum_type: *enum_type,
                            key: Symbol::intern(prev_key),
                            value: *prev_val,
                            index: index - 1,
                        });
                    }
                    return Ok(Value::Nil);
                }
                "succ" => {
                    if let Some(variants) = self.enum_types.get(&enum_type.resolve())
                        && let Some((next_key, next_val)) = variants.get(index + 1)
                    {
                        return Ok(Value::Enum {
                            enum_type: *enum_type,
                            key: Symbol::intern(next_key),
                            value: *next_val,
                            index: index + 1,
                        });
                    }
                    return Ok(Value::Nil);
                }
                _ => {}
            }
        }

        // SharedPromise dispatch
        if let Value::Promise(ref shared) = target {
            return self.dispatch_promise_method(shared, method, args, &target);
        }

        // SharedChannel dispatch
        if let Value::Channel(ref ch) = target {
            return self.dispatch_channel_method(ch, method, args);
        }

        // Promise::Vow forwards keep/break to the backing Promise.
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name.resolve() == "Promise::Vow"
        {
            return self.dispatch_promise_vow_method(attributes, method, args);
        }

        if let Value::Mixin(inner, mixins) = &target {
            if args.is_empty() {
                let attr_key = format!("__mutsu_attr__{}", method);
                if let Some(value) = mixins.get(&attr_key) {
                    return Ok(value.clone());
                }
            }
            return self.call_method_with_values(inner.as_ref().clone(), method, args);
        }

        // Instance dispatch, package dispatch, and fallback paths
        self.dispatch_instance_and_fallback(target, method, args)
    }

    fn buf_allocate(&mut self, class_name: Symbol, args: &[Value]) -> Result<Value, RuntimeError> {
        let size = match args.first() {
            Some(v) => super::to_int(v) as usize,
            None => 0,
        };
        let fill_arg = args.get(1);
        let byte_vals: Vec<Value> = if let Some(fill) = fill_arg {
            match fill {
                Value::Int(n) => vec![Value::Int(*n); size],
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                    let pattern: Vec<Value> = items.to_vec();
                    if pattern.is_empty() {
                        vec![Value::Int(0); size]
                    } else {
                        (0..size)
                            .map(|i| pattern[i % pattern.len()].clone())
                            .collect()
                    }
                }
                _ => vec![fill.clone(); size],
            }
        } else {
            vec![Value::Int(0); size]
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(byte_vals));
        Ok(Value::make_instance(class_name, attrs))
    }
}
