use super::methods_signature::{
    make_method_not_found_error, make_private_permission_error, make_x_immutable_error,
};
use super::*;
use crate::symbol::Symbol;
use crate::value::signature::extract_sig_info;

impl Interpreter {
    pub(super) fn promise_combinator_error(combinator: &str) -> RuntimeError {
        let message = format!(
            "Can only use {} to combine defined Promise objects",
            combinator
        );
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(Symbol::intern("X::Promise::Combinator"), attrs);
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        err
    }

    pub(super) fn collect_promise_combinator_inputs(
        &self,
        combinator: &str,
        args: &[Value],
    ) -> Result<Vec<SharedPromise>, RuntimeError> {
        let mut promises = Vec::new();
        for arg in args {
            match arg {
                Value::Promise(promise) => promises.push(promise.clone()),
                _ if arg.as_list_items().is_some() => {
                    for item in arg.as_list_items().unwrap().iter() {
                        if let Value::Promise(promise) = item {
                            promises.push(promise.clone());
                        } else {
                            return Err(Self::promise_combinator_error(combinator));
                        }
                    }
                }
                _ => return Err(Self::promise_combinator_error(combinator)),
            }
        }
        Ok(promises)
    }

    pub(super) fn supply_list_values(
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

    pub(super) fn iterator_supports_predictive_methods(
        attributes: &HashMap<String, Value>,
    ) -> bool {
        matches!(attributes.get("items"), Some(Value::Array(..)))
            || matches!(attributes.get("squish_source"), Some(Value::Array(..)))
    }

    pub(super) fn iterator_count_only_from_attrs(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        if let Some(Value::Array(items, ..)) = attributes.get("items") {
            let index = match attributes.get("index") {
                Some(Value::Int(i)) if *i >= 0 => *i as usize,
                _ => 0,
            };
            return Ok(Some(Value::Int(items.len().saturating_sub(index) as i64)));
        }

        let Some(Value::Array(source, ..)) = attributes.get("squish_source") else {
            return Ok(None);
        };

        let mut scan_index = match attributes.get("squish_scan_index") {
            Some(Value::Int(i)) if *i >= 0 => *i as usize,
            _ => 0,
        };
        let mut prev_key = attributes
            .get("squish_prev_key")
            .cloned()
            .unwrap_or(Value::Nil);
        let initialized = matches!(
            attributes.get("squish_initialized"),
            Some(Value::Bool(true))
        );
        let as_func = attributes
            .get("squish_as")
            .cloned()
            .filter(|v| !matches!(v, Value::Nil));
        let with_func = attributes
            .get("squish_with")
            .cloned()
            .filter(|v| !matches!(v, Value::Nil));

        let mut remaining = 0usize;

        if !initialized {
            let Some(first) = source.first().cloned() else {
                return Ok(Some(Value::Int(0)));
            };
            prev_key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![first], true)?
            } else {
                first
            };
            scan_index = 1;
            remaining += 1;
        }

        while scan_index < source.len() {
            let item = source[scan_index].clone();
            let key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![item.clone()], true)?
            } else {
                item
            };
            let duplicate = if let Some(func) = with_func.clone() {
                self.call_sub_value(func, vec![prev_key.clone(), key.clone()], true)?
                    .truthy()
            } else {
                crate::runtime::values_identical(&prev_key, &key)
            };
            prev_key = key;
            scan_index += 1;
            if !duplicate {
                remaining += 1;
            }
        }

        Ok(Some(Value::Int(remaining as i64)))
    }

    pub(super) fn iterator_bool_only_from_attrs(
        &mut self,
        attributes: &HashMap<String, Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(count) = self.iterator_count_only_from_attrs(attributes)? else {
            return Ok(None);
        };
        Ok(Some(Value::Bool(super::to_int(&count) > 0)))
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
        // .return method: triggers a return from the enclosing sub with the invocant
        if method == "return" && args.is_empty() {
            let mut err = RuntimeError::new("return");
            err.return_value = Some(target);
            return Err(err);
        }
        // Unhandled Failure explosion: calling a non-Failure method on an unhandled
        // Failure should throw the stored exception (Raku behavior).
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name.resolve() == "Failure"
            && !attributes.get("handled").is_some_and(Value::truthy)
            && !matches!(
                method,
                "exception"
                    | "handled"
                    | "self"
                    | "defined"
                    | "Bool"
                    | "so"
                    | "gist"
                    | "Str"
                    | "raku"
                    | "perl"
                    | "WHICH"
                    | "backtrace"
                    | "is-handling"
                    | "WHAT"
                    | "^name"
                    | "isa"
                    | "does"
                    | "ACCEPTS"
                    | "Failure"
                    | "sink"
            )
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&target)
        {
            return Err(err);
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
        // Enum type collection methods: .pairs, .keys, .values, .kv, .antipairs, .invert
        if let Value::Package(pkg_name) = &target
            && args.is_empty()
            && matches!(
                method,
                "pairs" | "keys" | "values" | "kv" | "antipairs" | "invert"
            )
        {
            if let Some(variants) = self.enum_types.get(&pkg_name.resolve()) {
                let variants = variants.clone();
                return self.dispatch_enum_type_collection(method, &variants);
            }
            // Non-enum Package types: return empty for collection methods
            return Ok(Value::array(Vec::new()));
        }

        if matches!(&target, Value::Instance { class_name, .. } if class_name == "IterationBuffer")
            && matches!(
                method,
                "elems"
                    | "AT-POS"
                    | "BIND-POS"
                    | "push"
                    | "unshift"
                    | "List"
                    | "Slip"
                    | "Seq"
                    | "append"
                    | "prepend"
                    | "clear"
            )
        {
            return self.dispatch_instance_and_fallback(target, method, args);
        }
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Iterator"
        {
            match method {
                "count-only" if args.is_empty() => {
                    if let Some(value) = self.iterator_count_only_from_attrs(attributes.as_ref())? {
                        return Ok(value);
                    }
                }
                "bool-only" if args.is_empty() => {
                    if let Some(value) = self.iterator_bool_only_from_attrs(attributes.as_ref())? {
                        return Ok(value);
                    }
                }
                "can"
                    if args.len() == 1
                        && Self::iterator_supports_predictive_methods(attributes.as_ref()) =>
                {
                    let method_name = args[0].to_string_value();
                    if matches!(method_name.as_str(), "count-only" | "bool-only") {
                        return Ok(Value::array(vec![Value::str(method_name)]));
                    }
                }
                _ => {}
            }
        }
        if args.is_empty()
            && matches!(method, "Str" | "gist")
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|name| name == "DateTime" || name == "Date")
            && let Some(formatter) = attributes.get("formatter")
        {
            let saved_env = self.env().clone();
            let saved_readonly = self.save_readonly_vars();
            let rendered = self.eval_call_on_value(formatter.clone(), vec![target.clone()])?;
            *self.env_mut() = saved_env;
            self.restore_readonly_vars(saved_readonly);
            return Ok(Value::str(rendered.to_string_value()));
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
        // Non-container definite values: push/pop/shift/unshift/etc. must throw X::Method::NotFound
        if matches!(
            method,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        ) && matches!(
            &target,
            Value::Int(_)
                | Value::Num(_)
                | Value::Str(_)
                | Value::Bool(_)
                | Value::Rat(..)
                | Value::Complex(..)
        ) {
            let type_name = crate::runtime::utils::value_type_name(&target);
            return Err(make_method_not_found_error(method, type_name, false));
        }
        // Mutating array methods on Value::Array (non-container path).
        // This handles cases like %hash<key>.push(4) where the target is an
        // Array value retrieved from a container and we need to produce a
        // modified copy that the caller can write back.
        if matches!(
            method,
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend"
        ) && matches!(&target, Value::Array(_, kind) if kind.is_real_array())
        {
            return self.array_mutate_copy(target, method, args);
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

        // .throw on user-defined Exception subclasses (MRO-aware)
        if method == "throw"
            && args.is_empty()
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
        {
            let cn = class_name.resolve();
            let is_exception = cn == "Exception"
                || cn.starts_with("X::")
                || cn.starts_with("CX::")
                || self
                    .class_mro(&cn)
                    .iter()
                    .any(|p| p == "Exception" || p.starts_with("X::") || p.starts_with("CX::"));
            if is_exception {
                let msg = attributes
                    .get("message")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| target.to_string_value());
                let mut err = RuntimeError::new(&msg);
                err.exception = Some(Box::new(target.clone()));
                return Err(err);
            }
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
            && !matches!(
                &target,
                Value::Instance { .. } | Value::Package(_) | Value::Mixin(..)
            )
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
        // Use rsplit_once to handle multi-part qualifiers like Bar::R::method
        if let Some((qualifier, actual_method)) = method.rsplit_once("::")
            && !method.starts_with('!')
            && let Value::Instance {
                class_name: inst_class,
                attributes,
                id: target_id,
            } = &target
        {
            // Check that the qualifier class/role is in the instance's MRO or composed roles
            // Also check roles across the whole MRO (not just the instance's class)
            // and match base name of parameterized roles (e.g., "Bar::R" matches "Bar::R[Str]")
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
                return Err(RuntimeError::new(format!(
                    "Cannot dispatch to method {} on {} because it is not inherited or done by {}",
                    actual_method, qualifier, inst_cn_str
                )));
            }
            // Read: look up the attribute in the qualifier class's attribute definitions
            if args.is_empty() {
                let class_attrs = self.collect_class_attributes(qualifier);
                for (attr_name, is_public, ..) in &class_attrs {
                    if *is_public && attr_name == actual_method {
                        return Ok(attributes.get(actual_method).cloned().unwrap_or(Value::Nil));
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
                let (result, updated) = self.run_instance_method(
                    qualifier,
                    attrs_map,
                    actual_method,
                    args,
                    Some(target.clone()),
                )?;
                self.overwrite_instance_bindings_by_identity(&inst_cn, tid, updated);
                if let Value::Proxy { ref fetcher, .. } = result {
                    let _ = method_def;
                    return self.proxy_fetch(fetcher, None, qualifier, &(**attributes).clone(), 0);
                }
                return Ok(result);
            }
            // Fallback: find a method with matching role_origin in the instance's class.
            // This handles qualified calls like self.RoleName::method() where the role's
            // methods have been composed into the class.
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
                        let (result, updated) = self.run_instance_method_resolved(
                            &inst_cn,
                            qualifier,
                            def,
                            attrs_map,
                            args,
                            Some(target.clone()),
                        )?;
                        self.overwrite_instance_bindings_by_identity(&inst_cn, tid, updated);
                        return Ok(result);
                    }
                }
            }
            // Also check the role definition directly
            // Try parameterized role names too (e.g., qualifier "Bar::R" matches role "Bar::R[Str]")
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
                        let (result, updated) = self.run_instance_method_resolved(
                            &inst_cn,
                            qualifier,
                            def.clone(),
                            attrs_map,
                            args,
                            Some(target.clone()),
                        )?;
                        self.overwrite_instance_bindings_by_identity(&inst_cn, tid, updated);
                        return Ok(result);
                    }
                }
            }
        }

        // Handle qualified method calls on non-Instance values: e.g. (-42).Int::abs
        // Split qualifier::method, verify the qualifier matches the target's type,
        // then dispatch the actual method on the target.
        if let Some((qualifier, actual_method)) = method.split_once("::")
            && !method.starts_with('!')
            && !matches!(&target, Value::Instance { .. })
        {
            let type_name = super::utils::value_type_name(&target);
            // Check type hierarchy: Int ~~ Cool ~~ Any ~~ Mu
            let type_matches = qualifier == type_name || Self::type_inherits(type_name, qualifier);
            if type_matches {
                return self.call_method_with_values(target, actual_method, args);
            }
            return Err(RuntimeError::new(format!(
                "X::Method::InvalidQualifier: Cannot dispatch to a method on {} because it is not inherited or done by {}",
                qualifier, type_name
            )));
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
            // If the result is a Date with a formatter but no rendered output, render it
            let val = result?;
            if let Value::Instance { ref attributes, .. } = val
                && attributes.contains_key("formatter")
                && !attributes.contains_key("__formatter_rendered")
            {
                let formatter = attributes.get("formatter").unwrap().clone();
                return self.render_date_formatter(val, formatter);
            }
            return Ok(val);
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
        // Handle Match.make method — stores value in Match instance's `ast` attribute
        // and updates `$/` in the environment.
        if let Value::Instance {
            class_name,
            attributes,
            id,
        } = &target
            && class_name == "Match"
            && method == "make"
        {
            let value = args.first().cloned().unwrap_or(Value::Nil);
            let mut attrs = crate::value::InstanceAttrs::clone(attributes);
            attrs.insert("ast".to_string(), value.clone());
            let updated = Value::Instance {
                class_name: *class_name,
                attributes: std::sync::Arc::new(crate::value::InstanceAttrs::new(
                    *class_name,
                    attrs,
                    *id,
                    false,
                )),
                id: *id,
            };
            self.env.insert("/".to_string(), updated);
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
        if matches!(
            method,
            "max"
                | "min"
                | "lines"
                | "delayed"
                | "reduce"
                | "classify"
                | "start"
                | "squish"
                | "produce"
                | "map"
                | "flat"
                | "batch"
                | "rotor"
                | "rotate"
                | "comb"
                | "words"
                | "snip"
                | "minmax"
                | "wait"
        ) && matches!(&target, Value::Package(name) if name == "Supply")
        {
            return Err(RuntimeError::new(format!(
                "Cannot call .{} on a Supply type object",
                method
            )));
        }
        // Supply.merge(...) as a class method
        if method == "merge" && matches!(&target, Value::Package(name) if name == "Supply") {
            return self.dispatch_supply_merge(&args);
        }
        // Supply.zip(...) as a class method
        if method == "zip" && matches!(&target, Value::Package(name) if name == "Supply") {
            return self.dispatch_supply_zip_class(&args);
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
            // Determine if this is a private method call (method starts with '!')
            let is_private_call = method.starts_with('!');
            let lookup_name = if is_private_call {
                &method[1..]
            } else {
                method
            };
            let mut role_has_method = false;
            for role_name in role_names {
                let Some(role) = self.roles.get(&role_name).cloned() else {
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
                    if let Some(role) = self.roles.get(&role_name)
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
                return Ok(Value::array(results));
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
                            || self.type_matches_value(&n, &target)
                    }
                    Value::Str(name) => {
                        mixins.contains_key(name.as_str())
                            || mixins.contains_key(&format!("__mutsu_role__{}", name))
                            || self.type_matches_value(name, &target)
                    }
                    Value::Instance { class_name, .. } => {
                        self.type_matches_value(&class_name.resolve(), &target)
                    }
                    other => self.type_matches_value(&other.to_string_value(), &target),
                };
                return Ok(Value::Bool(does));
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
                    return Ok(Value::Bool(false));
                }
                // Delegate to inner value's isa check using class MRO
                let result = match inner.as_ref() {
                    Value::Instance { class_name, .. } => {
                        self.class_mro(&class_name.resolve()).contains(&target_name)
                    }
                    _ => inner.isa_check(&target_name),
                };
                return Ok(Value::Bool(result));
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
                            value: value.clone(),
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
                        crate::value::LazyList::new_cached(out),
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
                    crate::value::LazyList::new_cached(out),
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
            || (matches!(
                method,
                "max"
                    | "min"
                    | "head"
                    | "flat"
                    | "sort"
                    | "comb"
                    | "words"
                    | "batch"
                    | "rotor"
                    | "rotate"
                    | "produce"
                    | "snip"
                    | "minmax"
                    | "start"
                    | "wait"
                    | "zip"
                    | "zip-latest"
            ) && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "elems" && matches!(&target, Value::Instance { .. }))
            || (matches!(method, "list" | "Array" | "Seq")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "Supply"
                && matches!(&target, Value::Instance { class_name, .. }
                    if class_name == "Supplier" || class_name == "Supplier::Preserving"))
            || matches!(&target, Value::Instance { class_name, .. }
                if self.is_native_method(&class_name.resolve(), method))
            || (matches!(&target, Value::Instance { class_name, .. } if class_name == "IO::Handle")
                && matches!(method, "chomp" | "encoding" | "opened" | "DESTROY"))
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
                        | "bind-stdin"
                        | "bind-stdout"
                        | "bind-stderr"
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
                && matches!(&target, Value::Instance { class_name, .. } if self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(&target, Value::Package(class_name) if self.has_user_method(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(&target, Value::Package(class_name) if self.has_class_level_attr(&class_name.resolve(), method) && !self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(&target, Value::Instance { class_name, .. } if self.has_class_level_attr(&class_name.resolve(), method) && !self.has_public_accessor(&class_name.resolve(), method)));
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
        if method == "tail"
            && !bypass_native_fastpath
            && !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
        {
            return self.dispatch_tail(target, &args);
        }
        // .raku/.perl on constrained Hash: produce (my Int % = ...) format
        if matches!(method, "raku" | "perl")
            && args.is_empty()
            && matches!(&target, Value::Hash(_))
            && let Some(info) = self.container_type_metadata(&target)
            && let Value::Hash(map) = &target
        {
            let mut sorted_keys: Vec<&String> = map.keys().collect();
            sorted_keys.sort();
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| {
                    let v = &map[*k];
                    if let Value::Bool(true) = v {
                        format!(":{}", k)
                    } else if let Value::Bool(false) = v {
                        format!(":!{}", k)
                    } else {
                        let repr = if matches!(v, Value::Nil) {
                            "Any".to_string()
                        } else {
                            self.call_method_with_values(v.clone(), "raku", vec![])
                                .map(|r| r.to_string_value())
                                .unwrap_or_else(|_| format!("{:?}", v))
                        };
                        format!(":{}({})", k, repr)
                    }
                })
                .collect();
            let key_suffix = if let Some(ref kt) = info.key_type {
                format!("{{{}}}", kt)
            } else {
                String::new()
            };
            let inner = parts.join(", ");
            let result = if inner.is_empty() {
                format!("(my {} %{})", info.value_type, key_suffix)
            } else {
                format!("(my {} %{} = {})", info.value_type, key_suffix, inner)
            };
            return Ok(Value::str(result));
        }

        if let Some(result) = native_result {
            if method == "decode" {
                return result.map(|value| match value {
                    Value::Str(decoded) => Value::str(self.translate_newlines_for_decode(&decoded)),
                    other => other,
                });
            }
            return result;
        }

        // Comprehensive split handler (for regex splitters and 3+ args)
        if method == "split"
            && !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
        {
            return self.handle_split_method(target, args);
        }

        // .of on Array/Hash: check container_type_metadata for element type
        if method == "of" && args.is_empty() && matches!(&target, Value::Array(..) | Value::Hash(_))
        {
            if let Some(info) = self.container_type_metadata(&target) {
                return Ok(Value::Package(Symbol::intern(&info.value_type)));
            }
            return Ok(Value::Package(Symbol::intern("Mu")));
        }

        // .keyof on Hash: check container_type_metadata for key type constraint
        if method == "keyof" && args.is_empty() && matches!(&target, Value::Hash(_)) {
            if let Some(info) = self.container_type_metadata(&target)
                && let Some(ref key_type) = info.key_type
            {
                return Ok(Value::Package(Symbol::intern(key_type)));
            }
            // Default key type for Hash is Str(Any) (coercion type)
            return Ok(Value::Package(Symbol::intern("Str(Any)")));
        }

        // Complex→Num conversion needs $*TOLERANCE from dynamic scope
        if method == "Num"
            && args.is_empty()
            && let Value::Complex(r, im) = &target
        {
            let tolerance = self
                .get_dynamic_var("*TOLERANCE")
                .ok()
                .and_then(|v| match v {
                    Value::Num(n) => Some(n),
                    Value::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                    Value::Int(n) => Some(n as f64),
                    _ => None,
                })
                .unwrap_or(1e-15);
            if im.abs() > tolerance {
                let msg = format!(
                    "Cannot convert {}{}{}i to Num: imaginary part not zero",
                    r,
                    if *im >= 0.0 { "+" } else { "" },
                    im
                );
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                attrs.insert("target".to_string(), Value::str_from("Num"));
                attrs.insert("source".to_string(), target.clone());
                let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs);
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
            return Ok(Value::Num(*r));
        }

        // Zero-denominator Rat/FatRat .Str should throw X::Numeric::DivideByZero
        if matches!(method, "Str" | "gist")
            && args.is_empty()
            && matches!(&target, Value::Rat(_, 0) | Value::FatRat(_, 0))
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str_from("Attempt to divide by zero when coercing Rational to Str"),
            );
            let ex = Value::make_instance(Symbol::intern("X::Numeric::DivideByZero"), attrs);
            let mut err =
                RuntimeError::new("Attempt to divide by zero when coercing Rational to Str");
            err.exception = Some(Box::new(ex));
            return Err(err);
        }

        // Force LazyList and re-dispatch as Seq for methods that need element access.
        if let Value::LazyList(ll) = &target
            && matches!(
                method,
                "list"
                    | "Array"
                    | "Numeric"
                    | "Int"
                    | "elems"
                    | "hyper"
                    | "race"
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
                    | "repeated"
                    | "squish"
                    | "classify"
                    | "categorize"
                    | "produce"
                    | "rotor"
                    | "batch"
                    | "reduce"
                    | "combinations"
                    | "permutations"
                    | "values"
                    | "List"
                    | "Str"
                    | "Stringy"
                    | "gist"
                    | "raku"
                    | "perl"
                    | "Seq"
                    | "item"
                    | "cache"
                    | "pick"
                    | "roll"
                    | "keys"
                    | "kv"
                    | "pairs"
                    | "antipairs"
            )
        {
            let saved_env = self.env.clone();
            let items = self.force_lazy_list_bridge(ll)?;
            if !matches!(method, "elems" | "hyper" | "race") {
                self.env = saved_env;
            }
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
                    | "does"
                    | "isa"
                    | "lookup"
                    | "find_method"
                    | "add_method"
                    | "add_multi_method"
                    | "compose"
                    | "archetypes"
                    | "name"
                    | "ver"
                    | "auth"
                    | "mro"
                    | "mro_unhidden"
                    | "methods"
                    | "attributes"
                    | "parents"
                    | "roles"
                    | "candidates"
                    | "concretization"
                    | "curried_role"
                    | "enum_value_list"
                    | "coerce"
                    | "pun"
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

        // Enum.new should throw X::Constructor::BadType
        if method == "new" {
            let enum_name = match &target {
                Value::Package(name) if name == "Bool" => Some("Bool".to_string()),
                Value::Bool(_) => Some("Bool".to_string()),
                Value::Package(name) if self.enum_types.contains_key(&name.resolve()) => {
                    Some(name.resolve())
                }
                Value::Enum { enum_type, .. } => Some(enum_type.resolve()),
                _ => None,
            };
            if let Some(ename) = enum_name {
                let msg = format!(
                    "Enum '{}' is insufficiently type-like to be instantiated.  Did you mean 'class'?",
                    ename
                );
                let mut attrs = HashMap::new();
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                let ex = Value::make_instance(Symbol::intern("X::Constructor::BadType"), attrs);
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
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
                let raw_exception = args
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
                // Wrap non-Exception values in X::AdHoc (Raku behavior)
                let exception = if let Value::Instance { class_name, .. } = &raw_exception {
                    let cn = class_name.resolve();
                    if cn == "Exception"
                        || cn.starts_with("X::")
                        || cn.starts_with("CX::")
                        || self.mro_readonly(&cn).iter().any(|p| {
                            p == "Exception" || p.starts_with("X::") || p.starts_with("CX::")
                        })
                    {
                        raw_exception
                    } else {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("payload".to_string(), raw_exception.clone());
                        attrs.insert(
                            "message".to_string(),
                            Value::str(raw_exception.to_string_value()),
                        );
                        Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
                    }
                } else {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("payload".to_string(), raw_exception.clone());
                    attrs.insert(
                        "message".to_string(),
                        Value::str(raw_exception.to_string_value()),
                    );
                    Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
                };
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
            "classify" | "categorize" if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply") =>
            {
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
                return self.dispatch_seq_from_loop(args);
            }
            "say" if args.is_empty() => {
                return self.dispatch_say(&target);
            }
            "print" if args.is_empty() => {
                return self.dispatch_print(&target);
            }
            "put" if args.is_empty() => {
                return self.dispatch_put(&target);
            }
            "shape" if args.is_empty() => {
                if let Some(result) = self.dispatch_shape(&target) {
                    return result;
                }
            }
            "default" if args.is_empty() => {
                if let Some(result) = Self::dispatch_default(&target) {
                    return result;
                }
            }
            "note" if args.is_empty() => {
                return self.dispatch_note(&target);
            }
            "return-rw" if args.is_empty() => {
                return Ok(target);
            }
            "encode" if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply") =>
            {
                return self.dispatch_encode(&target, &args);
            }
            "decode" => {
                if let Some(result) = self.dispatch_decode(&target, &args) {
                    return result;
                }
            }
            "subbuf" => {
                if let Some(result) = self.dispatch_subbuf(&target, &args) {
                    return result;
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
            "can" if args.len() == 1 => {
                let method_name = args[0].to_string_value();
                let results = self.collect_can_methods(&target, &method_name);
                return Ok(Value::array(results));
            }
            "does" if args.len() == 1 => {
                let type_name = match &args[0] {
                    Value::Package(name) => name.resolve(),
                    Value::Str(name) => name.to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    other => other.to_string_value(),
                };
                return Ok(Value::Bool(self.type_matches_value(&type_name, &target)));
            }
            "start" => {
                if let Some(result) = self.dispatch_promise_start(&target, &args) {
                    return result;
                }
            }
            "in" => {
                if let Some(result) = self.dispatch_promise_in(&target, &args) {
                    return result;
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
                if let Some(result) = self.dispatch_promise_at(&target, &args) {
                    return result;
                }
            }
            "kept" => {
                if let Some(result) = self.dispatch_promise_kept(&target, &args) {
                    return result;
                }
            }
            "broken" => {
                if let Some(result) = self.dispatch_promise_broken(&target, &args) {
                    return result;
                }
            }
            "allof" => {
                if let Some(result) = self.dispatch_promise_allof(&target, &args) {
                    return result;
                }
            }
            "anyof" => {
                if let Some(result) = self.dispatch_promise_anyof(&target, &args) {
                    return result;
                }
            }
            "WHAT" if args.is_empty() => {
                return self.dispatch_what(&target, args);
            }
            "HOW" => {
                return self.dispatch_how(&target, &args);
            }
            "WHO" if args.is_empty() => {
                return self.dispatch_who(&target);
            }
            "WHY" if args.is_empty() => {
                return self.dispatch_why(&target);
            }
            "^name" if args.is_empty() => {
                return self.dispatch_caret_name(&target);
            }
            "^enum_value_list" | "enum_value_list" => {
                if let Some(result) = self.dispatch_enum_value_list(&target) {
                    return result;
                }
            }
            "enums" => {
                if let Some(result) = self.dispatch_enums(&target) {
                    return result;
                }
            }
            "invert" => {
                if let Some(result) = self.dispatch_invert_enum(&target) {
                    return result;
                }
            }
            "subparse" | "parse" | "parsefile" => {
                if let Value::Package(ref package_name) = target {
                    return self.dispatch_package_parse(&package_name.resolve(), method, &args);
                }
            }
            "match" => {
                return self.dispatch_match_method(target, &args);
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
            "substr-rw" => {
                // In non-lvalue context, substr-rw just returns the substring
                return self.dispatch_substr_rw(target, &args);
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
                return self.dispatch_seq_coercion(target);
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
                return self.dispatch_list_coercion(target);
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
                if let Some(result) = self.dispatch_setty_baggy_mixy(&target, method) {
                    return result;
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
            "hash" if args.is_empty() && !matches!(&target, Value::Instance { .. }) => {
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
                let lazy = crate::builtins::methods_0arg::is_value_lazy(&target);
                let items = crate::runtime::utils::value_to_list(&target);
                let mut attrs = HashMap::new();
                attrs.insert("items".to_string(), Value::array(items));
                attrs.insert("index".to_string(), Value::Int(0));
                if lazy {
                    attrs.insert("is_lazy".to_string(), Value::Bool(true));
                }
                return Ok(Value::make_instance(Symbol::intern("Iterator"), attrs));
            }
            "produce" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_transform(target, "produce", &args);
                }
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
                    let attrs_clone = (**attributes).clone();
                    return self.dispatch_supply_reduce(target, &attrs_clone, callable);
                }
                let items = Self::value_to_list(&target);
                return self.reduce_items(callable, items);
            }
            "elems" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_elems(attributes, &args);
                }
                return self.call_function("elems", vec![target]);
            }
            "map" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_map(attributes, &args);
                }
                let items = Self::value_to_list(&target);
                return self.eval_map_over_items(args.first().cloned(), items);
            }
            "duckmap" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                return self.duckmap_iterate(&block, &target);
            }
            "deepmap" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                return self.deepmap_iterate(&block, &target);
            }
            "nodemap" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                return self.nodemap_iterate(&block, &target);
            }
            "max" | "min" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_running_extrema(target, method, &args);
                }
                // For all types (Hash, Array, Seq, List, scalars, etc.),
                // convert .min/.max method call args to builtin call args.
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
            "minpairs" | "maxpairs" if args.is_empty() => {
                return self.dispatch_minmaxpairs(target, method);
            }
            "pop" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "Too many positionals passed; expected 1 argument but got {}",
                        args.len() + 1
                    )));
                }
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
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_transform(target, "sort", &args);
                }
                if let Value::Package(name) = &target
                    && name == "Supply"
                {
                    // Supply.sort on type object: return a Seq containing the type object
                    return Ok(Value::Seq(std::sync::Arc::new(vec![target])));
                }
                return self.dispatch_sort(target, &args);
            }
            "unique" => {
                // Supply.unique is handled by native_supply
                if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
                {
                    return self.dispatch_unique(target, &args);
                }
            }
            "repeated" => {
                if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
                {
                    return self.dispatch_repeated(target, &args);
                }
            }
            "squish" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_transform(target, "squish", &args);
                }
                return self.dispatch_squish(target, &args);
            }
            "minmax" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    if !args.is_empty() {
                        return self.dispatch_supply_transform(target, method, &args);
                    }
                    // Fall through for 0-arg Supply.minmax
                } else {
                    // .minmax on lists/arrays/ranges (with or without comparator block)
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
                    return self.builtin_minmax(&call_args);
                }
            }
            "snip" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_transform(target, method, &args);
                }
                if !args.is_empty() {
                    let matcher = args[0].clone();
                    let items = crate::runtime::utils::value_to_list(&target);
                    return self.eval_snip(matcher, items);
                }
            }
            "head" | "flat" | "batch" | "comb" | "words" | "wait" | "zip" | "zip-latest" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_transform(target, method, &args);
                }
            }
            "collate" if args.is_empty() => {
                return self.dispatch_collate(target);
            }
            "take" if args.is_empty() => {
                self.take_value(target.clone())?;
                return Ok(target);
            }
            "rotor" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_transform(target, "rotor", &args);
                }
                return self.dispatch_rotor(target, &args);
            }
            "from-list" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_from_list(&args);
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
                    return self.dispatch_supply_signal(&args);
                }
            }
            "on-demand" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_on_demand(&args);
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
            // self.Mu::new(...)  –  qualified call to Mu's default constructor.
            // In Raku, Mu.new simply calls self.bless with the named arguments,
            // so we redirect to the "bless" logic, bypassing any user-defined new.
            "Mu::new" => {
                return self.call_method_with_values(target, "bless", args);
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
                // Run BUILD/TWEAK submethods in MRO order (base-first)
                let mro = self.class_mro(&class_name.resolve());
                let is_6e = crate::parser::current_language_version().starts_with("6.e");
                for mro_class in mro.iter().rev() {
                    if mro_class == "Any" || mro_class == "Mu" {
                        continue;
                    }
                    // Skip role entries in MRO
                    if self.roles.contains_key(mro_class) && !self.classes.contains_key(mro_class) {
                        continue;
                    }
                    // Under v6.e+, call BUILD submethods from composed roles first
                    if is_6e {
                        let role_order = self.ordered_role_submethods_for_class(mro_class, "BUILD");
                        for (role_name, method_def) in role_order {
                            let (_v, updated) = self.run_instance_method_resolved(
                                &class_name.resolve(),
                                &role_name,
                                method_def,
                                attributes.clone(),
                                Vec::new(),
                                Some(Value::make_instance(class_name, attributes.clone())),
                            )?;
                            attributes = updated;
                        }
                    }
                    let has_build = self
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("BUILD"))
                        .is_some();
                    if has_build {
                        let (_v, updated) = self.run_instance_method(
                            mro_class,
                            attributes.clone(),
                            "BUILD",
                            Vec::new(),
                            Some(Value::make_instance(class_name, attributes.clone())),
                        )?;
                        attributes = updated;
                    }
                }
                for mro_class in mro.iter().rev() {
                    if mro_class == "Any" || mro_class == "Mu" {
                        continue;
                    }
                    // Skip role entries in MRO
                    if self.roles.contains_key(mro_class) && !self.classes.contains_key(mro_class) {
                        continue;
                    }
                    // Under v6.e+, call TWEAK submethods from composed roles first
                    if is_6e {
                        let role_order = self.ordered_role_submethods_for_class(mro_class, "TWEAK");
                        for (role_name, method_def) in role_order {
                            let (_v, updated) = self.run_instance_method_resolved(
                                &class_name.resolve(),
                                &role_name,
                                method_def,
                                attributes.clone(),
                                Vec::new(),
                                Some(Value::make_instance(class_name, attributes.clone())),
                            )?;
                            attributes = updated;
                        }
                    }
                    let has_tweak = self
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("TWEAK"))
                        .is_some();
                    if has_tweak {
                        let (_v, updated) = self.run_instance_method(
                            mro_class,
                            attributes.clone(),
                            "TWEAK",
                            Vec::new(),
                            Some(Value::make_instance(class_name, attributes.clone())),
                        )?;
                        attributes = updated;
                    }
                }
                return Ok(Value::make_instance(class_name, attributes));
            }
            "now" => {
                if let Value::Package(ref class_name) = target
                    && self
                        .class_mro(&class_name.resolve())
                        .iter()
                        .any(|name| name == "DateTime")
                {
                    use crate::builtins::methods_0arg::temporal;
                    let mut timezone = 0i64;
                    let mut formatter: Option<Value> = None;
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            match key.as_str() {
                                "timezone" => timezone = value.to_f64() as i64,
                                "formatter" => formatter = Some(*value.clone()),
                                _ => {}
                            }
                        }
                    }
                    let secs = crate::value::current_time_secs_f64() + timezone as f64;
                    let total_i = secs.floor() as i64;
                    let frac = secs - total_i as f64;
                    let day_secs = total_i.rem_euclid(86400);
                    let epoch_days = (total_i - day_secs) / 86400;
                    let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                    let h = day_secs / 3600;
                    let mi = (day_secs % 3600) / 60;
                    let s = (day_secs % 60) as f64 + frac;
                    let dt = temporal::make_datetime(y, m, d, h, mi, s, timezone);
                    if let Some(formatter_value) = formatter
                        && let Value::Instance {
                            class_name,
                            ref attributes,
                            id,
                        } = dt
                    {
                        let mut attrs = (**attributes).clone();
                        attrs.insert("formatter".to_string(), formatter_value.clone());
                        let dt_with_formatter = Value::make_instance_with_id(class_name, attrs, id);
                        let saved_env = self.env().clone();
                        let saved_readonly = self.save_readonly_vars();
                        let rendered = self
                            .eval_call_on_value(formatter_value, vec![dt_with_formatter.clone()])?
                            .to_string_value();
                        *self.env_mut() = saved_env;
                        self.restore_readonly_vars(saved_readonly);
                        if let Value::Instance {
                            class_name,
                            attributes,
                            id,
                        } = dt_with_formatter
                        {
                            let mut updated = (*attributes).clone();
                            updated
                                .insert("__formatter_rendered".to_string(), Value::str(rendered));
                            return Ok(Value::make_instance_with_id(class_name, updated, id));
                        }
                    }
                    if class_name != "DateTime" {
                        return self.dispatch_new(target.clone(), vec![dt]);
                    }
                    return Ok(dt);
                }
            }
            "Date" if args.is_empty() => {
                if let Value::Package(ref class_name) = target
                    && self
                        .class_mro(&class_name.resolve())
                        .iter()
                        .any(|name| name == "DateTime")
                {
                    return Ok(Value::Package(Symbol::intern("Date")));
                }
            }
            "DateTime" if args.is_empty() => {
                if let Value::Package(ref class_name) = target
                    && self
                        .class_mro(&class_name.resolve())
                        .iter()
                        .any(|name| name == "DateTime")
                {
                    return Ok(target.clone());
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
                    return self.dispatch_supply_grab(attributes, &args);
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
                    return self.dispatch_supply_skip(attributes, &args);
                }
                // General list/range skip: skip first N elements
                let n = if args.is_empty() {
                    1usize
                } else {
                    args[0].to_f64().max(0.0) as usize
                };
                let items = crate::runtime::utils::value_to_list(&target);
                let result: Vec<Value> = items.into_iter().skip(n).collect();
                return Ok(Value::Seq(Arc::new(result)));
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
                    let items = Self::value_to_list(&target);
                    let mut parts = Vec::with_capacity(items.len());
                    for v in &items {
                        if matches!(v, Value::Instance { .. }) {
                            // Call user-defined Str() method on instances
                            let s = self.call_method_with_values(v.clone(), "Str", vec![])?;
                            parts.push(s.to_string_value());
                        } else {
                            parts.push(v.to_string_value());
                        }
                    }
                    let joined = parts.join(&sep);
                    return Ok(Value::str(joined));
                }
            }
            "grep" => {
                return self.dispatch_grep(target, &args);
            }
            "toggle" => {
                return self.dispatch_toggle(target, &args);
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
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_transform(target, "rotate", &args);
                }
                return self.dispatch_rotate(target, &args);
            }
            _ => {}
        }

        // Enum dispatch
        if let Some(result) = self.dispatch_enum_method(&target, method, &args) {
            return result;
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

    /// Produce a modified copy of an Array value for push/pop/shift/unshift/append/prepend.
    /// Used when the target is not a named variable (e.g., a hash or array element).
    fn array_mutate_copy(
        &self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut items = match target {
            Value::Array(ref v, ..) => v.to_vec(),
            _ => Vec::new(),
        };
        let kind = match &target {
            Value::Array(_, k) => *k,
            _ => crate::value::ArrayKind::Array,
        };
        match method {
            "push" => {
                let normalized = Self::normalize_push_args_for_copy(args);
                items.extend(normalized);
                Ok(Value::Array(Arc::new(items), kind))
            }
            "append" => {
                let flat = flatten_append_args(args);
                items.extend(flat);
                Ok(Value::Array(Arc::new(items), kind))
            }
            "pop" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "Too many positionals passed; expected 1 argument but got {}",
                        args.len() + 1
                    )));
                }
                if items.is_empty() {
                    Ok(Value::Array(Arc::new(items), kind))
                } else {
                    items.pop();
                    Ok(Value::Array(Arc::new(items), kind))
                }
            }
            "shift" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "Too many positionals passed; expected 1 argument but got {}",
                        args.len() + 1
                    )));
                }
                if items.is_empty() {
                    Ok(Value::Array(Arc::new(items), kind))
                } else {
                    items.remove(0);
                    Ok(Value::Array(Arc::new(items), kind))
                }
            }
            "unshift" | "prepend" => {
                let normalized = Self::normalize_push_args_for_copy(args);
                for (i, arg) in normalized.into_iter().enumerate() {
                    items.insert(i, arg);
                }
                Ok(Value::Array(Arc::new(items), kind))
            }
            _ => unreachable!(),
        }
    }

    /// Normalize push/unshift arguments (unwrap Scalar containers, deitemize).
    fn normalize_push_args_for_copy(args: Vec<Value>) -> Vec<Value> {
        args.into_iter()
            .map(|arg| match arg {
                Value::Scalar(inner) => *inner,
                Value::Array(items, kind) if kind.is_itemized() => Value::Array(items, kind),
                other => other,
            })
            .collect()
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

    /// Check if a builtin type inherits from a given ancestor type.
    /// Covers the standard Raku type hierarchy for builtin types.
    fn type_inherits(type_name: &str, ancestor: &str) -> bool {
        // Standard hierarchy chains for builtin types
        let chain: &[&str] = match type_name {
            "Int" => &["Int", "Cool", "Any", "Mu"],
            "Num" => &["Num", "Cool", "Any", "Mu"],
            "Rat" | "FatRat" => &["Rat", "Cool", "Any", "Mu"],
            "Str" => &["Str", "Cool", "Any", "Mu"],
            "Bool" => &["Bool", "Int", "Cool", "Any", "Mu"],
            "Array" => &["Array", "List", "Cool", "Any", "Mu"],
            "List" => &["List", "Cool", "Any", "Mu"],
            "Hash" => &["Hash", "Cool", "Any", "Mu"],
            "Range" => &["Range", "Cool", "Any", "Mu"],
            "Pair" => &["Pair", "Cool", "Any", "Mu"],
            "Set" => &["Set", "Any", "Mu"],
            "Bag" => &["Bag", "Any", "Mu"],
            "Mix" => &["Mix", "Any", "Mu"],
            "Complex" => &["Complex", "Cool", "Any", "Mu"],
            "Regex" => &["Regex", "Method", "Routine", "Block", "Code", "Any", "Mu"],
            "Sub" => &["Sub", "Routine", "Block", "Code", "Any", "Mu"],
            "Junction" => &["Junction", "Mu"],
            _ => &["Any", "Mu"],
        };
        chain.contains(&ancestor)
    }
}
