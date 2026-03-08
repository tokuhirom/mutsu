use super::methods_signature::{make_method_not_found_error, make_private_permission_error};
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch method calls for Instance values and handle all fallback paths.
    ///
    /// This is called from `call_method_with_values` after the main method name
    /// match and enum/promise/channel/mixin dispatch.
    pub(super) fn dispatch_instance_and_fallback(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Instance dispatch
        if let Value::Instance {
            class_name,
            attributes,
            id: target_id,
        } = &target
        {
            if let Some(private_rest) = method.strip_prefix('!') {
                let caller_class = self
                    .method_class_stack
                    .last()
                    .cloned()
                    .or_else(|| Some(self.current_package().to_string()));
                // Resolve: owner-qualified (!Owner::method) or unqualified (!method)
                let (pm_name, resolved) =
                    if let Some((owner_class, pm_name)) = private_rest.split_once("::") {
                        let caller_allowed = caller_class.as_deref() == Some(owner_class)
                            || self.class_trusts.get(owner_class).is_some_and(|trusted| {
                                caller_class
                                    .as_ref()
                                    .is_some_and(|caller| trusted.contains(caller))
                            });
                        if !caller_allowed {
                            return Err(make_private_permission_error(pm_name, owner_class));
                        }
                        (
                            pm_name,
                            self.resolve_private_method_with_owner(
                                &class_name.resolve(),
                                owner_class,
                                pm_name,
                                &args,
                            ),
                        )
                    } else {
                        (
                            private_rest,
                            self.resolve_private_method_any_owner(
                                &class_name.resolve(),
                                private_rest,
                                &args,
                            ),
                        )
                    };
                if let Some((resolved_owner, method_def)) = resolved {
                    let caller_allowed = caller_class.as_deref() == Some(resolved_owner.as_str())
                        || self
                            .class_trusts
                            .get(&resolved_owner)
                            .is_some_and(|trusted| {
                                caller_class
                                    .as_ref()
                                    .is_some_and(|caller| trusted.contains(caller))
                            });
                    if !caller_allowed {
                        return Err(make_private_permission_error(
                            pm_name,
                            &class_name.resolve(),
                        ));
                    }
                    let (result, updated) = self.run_instance_method_resolved(
                        &class_name.resolve(),
                        &resolved_owner,
                        method_def,
                        (**attributes).clone(),
                        args,
                        Some(target.clone()),
                    )?;
                    self.overwrite_instance_bindings_by_identity(
                        &class_name.resolve(),
                        *target_id,
                        updated,
                    );
                    return Ok(result);
                }
                // Private method not found
                return Err(make_method_not_found_error(
                    pm_name,
                    &class_name.resolve(),
                    true,
                ));
            }

            if class_name == "Attribute" {
                match method {
                    "set_build" => {
                        let build = args.first().cloned().ok_or_else(|| {
                            RuntimeError::new("Attribute.set_build expects a build callback")
                        })?;
                        let owner = attributes
                            .get("__mutsu_attr_owner")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let attr_name = attributes
                            .get("__mutsu_attr_name")
                            .or_else(|| attributes.get("name"))
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        if owner.is_empty() || attr_name.is_empty() {
                            return Err(RuntimeError::new(
                                "Attribute.set_build missing owner or attribute name",
                            ));
                        }
                        self.attribute_build_overrides
                            .insert((owner, attr_name), build);
                        return Ok(target.clone());
                    }
                    "name" => {
                        return Ok(attributes.get("name").cloned().unwrap_or(Value::Nil));
                    }
                    _ => {}
                }
            }

            // IO::Spec methods
            if class_name == "IO::Spec" {
                match method {
                    "catdir" => {
                        let parts: Vec<String> = args
                            .iter()
                            .map(|a| {
                                if let Value::Array(items, ..) = a {
                                    items
                                        .iter()
                                        .map(|v| v.to_string_value())
                                        .collect::<Vec<_>>()
                                        .join("/")
                                } else {
                                    a.to_string_value()
                                }
                            })
                            .collect();
                        let joined = parts.join("/");
                        return Ok(Value::str(joined));
                    }
                    "catfile" => {
                        let parts: Vec<String> = args
                            .iter()
                            .map(|a| {
                                if let Value::Array(items, ..) = a {
                                    items
                                        .iter()
                                        .map(|v| v.to_string_value())
                                        .collect::<Vec<_>>()
                                        .join("/")
                                } else {
                                    a.to_string_value()
                                }
                            })
                            .collect();
                        let joined = parts.join("/");
                        return Ok(Value::str(joined));
                    }
                    "catpath" => {
                        let vol = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let dir = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                        let file = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                        let mut result = String::new();
                        if !vol.is_empty() {
                            result.push_str(&vol);
                        }
                        if !dir.is_empty() {
                            if !result.is_empty() && !result.ends_with('/') {
                                result.push('/');
                            }
                            result.push_str(&dir);
                        }
                        if !file.is_empty() {
                            if !result.is_empty() && !result.ends_with('/') {
                                result.push('/');
                            }
                            result.push_str(&file);
                        }
                        return Ok(Value::str(result));
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit::Repository::FileSystem" {
                match method {
                    "install" => {
                        return Err(RuntimeError::new("Cannot install on CUR::FileSystem"));
                    }
                    "need" => {
                        let short_name = match args.first() {
                            Some(Value::CompUnitDepSpec { short_name }) => *short_name,
                            _ => return Ok(Value::Nil),
                        };
                        let short_name_str = short_name.resolve();
                        let prefix = attributes
                            .get("prefix")
                            .map(Value::to_string_value)
                            .unwrap_or_default();
                        let canonical_prefix = std::fs::canonicalize(&prefix)
                            .unwrap_or_else(|_| std::path::PathBuf::from(&prefix))
                            .to_string_lossy()
                            .to_string();
                        // Check cache first
                        let cache_key =
                            format!("__mutsu_compunit::{}::{}", canonical_prefix, short_name_str);
                        if let Some(existing) = self.env.get(&cache_key).cloned() {
                            return Ok(existing);
                        }
                        // Try to find the module file in the prefix directory
                        let relative = short_name_str.replace("::", "/");
                        let mut found_path = None;
                        for ext in [".rakumod", ".pm6", ".raku", ".pm"] {
                            let candidate = std::path::Path::new(&canonical_prefix)
                                .join(format!("{relative}{ext}"));
                            if candidate.exists() {
                                found_path = Some(candidate);
                                break;
                            }
                        }
                        // If not found in prefix, try the standard module resolution
                        if found_path.is_none() {
                            found_path = self.resolve_module_path(&short_name_str);
                        }
                        let Some(source_path) = found_path else {
                            return Ok(Value::Nil);
                        };
                        let repo_precomp_enabled = attributes
                            .get("__mutsu_precomp_enabled")
                            .is_none_or(Value::truthy);
                        // Load the module, using precompilation cache when available.
                        // Explicitly constructed FileSystem repositories default to
                        // precomp-disabled behavior.
                        let (stmts, precompiled) = if repo_precomp_enabled {
                            self.parse_module_source(&short_name_str, &source_path)?
                        } else {
                            let saved = self.precomp_enabled;
                            self.precomp_enabled = false;
                            let parsed = self.parse_module_source(&short_name_str, &source_path);
                            self.precomp_enabled = saved;
                            parsed?
                        };
                        let compile_time_only = !stmts.is_empty()
                            && stmts
                                .iter()
                                .all(|stmt| matches!(stmt, crate::ast::Stmt::Use { .. }));
                        let non_version_use_count = if compile_time_only {
                            stmts
                                .iter()
                                .filter_map(|stmt| match stmt {
                                    crate::ast::Stmt::Use { module, .. } => Some(module.as_str()),
                                    _ => None,
                                })
                                .filter(|module| *module != "v6")
                                .count()
                        } else {
                            0
                        };
                        let skip_runtime = compile_time_only && non_version_use_count > 1;
                        if !skip_runtime {
                            self.run_block(&stmts)?;
                        }
                        self.loaded_modules.insert(short_name_str.clone());
                        let mut attrs = HashMap::new();
                        attrs.insert("from".to_string(), Value::str_from("Raku"));
                        attrs.insert("short-name".to_string(), Value::str(short_name_str));
                        attrs.insert(
                            "precompiled".to_string(),
                            Value::Bool(precompiled && repo_precomp_enabled),
                        );
                        let compunit = Value::make_instance(Symbol::intern("CompUnit"), attrs);
                        self.env.insert(cache_key, compunit.clone());
                        return Ok(compunit);
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit" {
                match method {
                    // Rakudo exposes a handle object that can answer .globalish-package.
                    // For mutsu, the current process-global stash models this behavior.
                    "handle" => {
                        return Ok(Value::make_instance(
                            Symbol::intern("CompUnit::Handle"),
                            HashMap::new(),
                        ));
                    }
                    "globalish-package" => {
                        return Ok(Value::Package(Symbol::intern("GLOBAL")));
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit::Handle" && method == "globalish-package" {
                return Ok(Value::Package(Symbol::intern("GLOBAL")));
            }
            if method == "can" {
                let method_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if method_name.is_empty() {
                    return Ok(Value::Bool(false));
                }
                let can = self.class_has_method(&class_name.resolve(), &method_name)
                    || self.has_user_method(&class_name.resolve(), &method_name)
                    || matches!(
                        method_name.as_str(),
                        "isa" | "gist" | "raku" | "perl" | "name" | "clone"
                    );
                return Ok(Value::Bool(can));
            }
            if class_name == "Proc::Async" {
                if matches!(
                    method,
                    "start"
                        | "kill"
                        | "write"
                        | "close-stdin"
                        | "ready"
                        | "print"
                        | "say"
                        | "Supply"
                ) {
                    let (result, updated) = self.call_native_instance_method_mut(
                        &class_name.resolve(),
                        (**attributes).clone(),
                        method,
                        args,
                    )?;
                    self.overwrite_instance_bindings_by_identity(
                        &class_name.resolve(),
                        *target_id,
                        updated,
                    );
                    return Ok(result);
                }
                if matches!(
                    method,
                    "command" | "started" | "w" | "pid" | "stdout" | "stderr" | "Supply"
                ) {
                    return self.call_native_instance_method(
                        &class_name.resolve(),
                        attributes,
                        method,
                        args,
                    );
                }
            }
            if self.is_native_method(&class_name.resolve(), method) {
                return self.call_native_instance_method(
                    &class_name.resolve(),
                    attributes,
                    method,
                    args,
                );
            }
            if method == "isa" {
                let target_name = match args.first().cloned().unwrap_or(Value::Nil) {
                    Value::Package(name) => name.resolve(),
                    Value::Str(name) => name.to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    other => other.to_string_value(),
                };
                return Ok(Value::Bool(
                    self.class_mro(&class_name.resolve()).contains(&target_name),
                ));
            }
            if method == "gist"
                && args.is_empty()
                && (class_name.resolve().starts_with("X::")
                    || class_name == "Exception"
                    || class_name.resolve().ends_with("Exception"))
                && let Some(msg) = attributes.get("message")
            {
                return Ok(Value::str(msg.to_string_value()));
            }
            if (method == "raku" || method == "perl") && args.is_empty() {
                if class_name == "ObjAt" {
                    let which = attributes
                        .get("WHICH")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::str(format!("ObjAt.new(\"{}\")", which)));
                }
                return Ok(Value::str(format!("{}.new()", class_name)));
            }
            if method == "name" && args.is_empty() {
                return Ok(attributes.get("name").cloned().unwrap_or(Value::Nil));
            }
            if method == "clone" {
                let mut attrs: HashMap<String, Value> = (**attributes).clone();
                for arg in &args {
                    if let Value::Pair(key, boxed) = arg {
                        attrs.insert(key.clone(), *boxed.clone());
                    }
                }
                return Ok(Value::make_instance(*class_name, attrs));
            }
            if method == "Bool"
                && args.is_empty()
                && ((target.does_check("Real") || target.does_check("Numeric"))
                    || self.has_user_method(&class_name.resolve(), "Bridge"))
                && let Ok(coerced) = self
                    .call_method_with_values(target.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(target.clone(), "Bridge", vec![]))
            {
                return Ok(Value::Bool(coerced.truthy()));
            }
            if method == "Bridge"
                && args.is_empty()
                && target.does_check("Real")
                && !self.has_user_method(&class_name.resolve(), "Bridge")
            {
                if let Ok(coerced) = self.call_method_with_values(target.clone(), "Numeric", vec![])
                    && coerced != target
                {
                    return Ok(coerced);
                }
                if let Ok(coerced) = self.call_method_with_values(target.clone(), "Num", vec![])
                    && coerced != target
                {
                    return Ok(coerced);
                }
            }
            if method == "Bridge"
                && args.is_empty()
                && self.has_user_method(&class_name.resolve(), "Num")
                && !self.has_user_method(&class_name.resolve(), "Bridge")
                && let Ok(coerced) = self.call_method_with_values(target.clone(), "Num", vec![])
                && coerced != target
            {
                return Ok(coerced);
            }
            if method == "log"
                && args.len() == 1
                && self.has_user_method(&class_name.resolve(), "Bridge")
                && let Ok(bridged) = self.call_method_with_values(target.clone(), "Bridge", vec![])
            {
                let base = if let Some(arg) = args.first() {
                    if matches!(arg, Value::Instance { class_name, .. }
                        if self.has_user_method(&class_name.resolve(), "Bridge"))
                    {
                        self.call_method_with_values(arg.clone(), "Numeric", vec![])
                            .or_else(|_| {
                                self.call_method_with_values(arg.clone(), "Bridge", vec![])
                            })
                            .unwrap_or_else(|_| arg.clone())
                    } else {
                        arg.clone()
                    }
                } else {
                    Value::Nil
                };
                return self.call_method_with_values(bridged, "log", vec![base]);
            }
            // User-defined methods take priority over auto-generated accessors
            if self.has_user_method(&class_name.resolve(), method) {
                let (result, updated) = self.run_instance_method(
                    &class_name.resolve(),
                    (**attributes).clone(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                self.overwrite_instance_bindings_by_identity(
                    &class_name.resolve(),
                    *target_id,
                    updated.clone(),
                );
                // Auto-FETCH if the method returned a Proxy
                if let Value::Proxy { ref fetcher, .. } = result {
                    return self.proxy_fetch(
                        fetcher,
                        None,
                        &class_name.resolve(),
                        &updated,
                        *target_id,
                    );
                }
                return Ok(result);
            }
            // Fallback: auto-generated accessor for public attributes
            if args.is_empty() {
                let class_attrs = self.collect_class_attributes(&class_name.resolve());
                if class_attrs.is_empty() {
                    if let Some(val) = attributes.get(method) {
                        return Ok(val.clone());
                    }
                } else {
                    for (attr_name, is_public, ..) in &class_attrs {
                        if *is_public && attr_name == method {
                            return Ok(attributes.get(method).cloned().unwrap_or(Value::Nil));
                        }
                    }
                }
            }
        }

        // For user-defined numeric/real-like objects, delegate unknown methods through
        // their coercion bridge so default Real behavior is available.
        if matches!(target, Value::Instance { ref class_name, .. }
            if (target.does_check("Real") || target.does_check("Numeric"))
                || self.has_user_method(&class_name.resolve(), "Bridge"))
        {
            if matches!(method, "Bridge" | "Real")
                && let Ok(coerced) = self.call_method_with_values(target.clone(), "Numeric", vec![])
                && coerced != target
            {
                return Ok(coerced);
            }
            if !matches!(method, "Numeric" | "Real" | "Bridge")
                && let Ok(coerced) = self
                    .call_method_with_values(target.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(target.clone(), "Bridge", vec![]))
                && coerced != target
                && let Ok(result) = {
                    let mut delegated_args = Vec::with_capacity(args.len());
                    for arg in &args {
                        let coerced_arg = if matches!(arg, Value::Instance { class_name, .. }
                            if self.has_user_method(&class_name.resolve(), "Bridge")
                                || arg.does_check("Real")
                                || arg.does_check("Numeric"))
                        {
                            self.call_method_with_values(arg.clone(), "Numeric", vec![])
                                .or_else(|_| {
                                    self.call_method_with_values(arg.clone(), "Bridge", vec![])
                                })
                                .unwrap_or_else(|_| arg.clone())
                        } else {
                            arg.clone()
                        };
                        delegated_args.push(coerced_arg);
                    }
                    let method_sym = crate::symbol::Symbol::intern(method);
                    if delegated_args.is_empty()
                        && let Some(result) =
                            crate::builtins::native_method_0arg(&coerced, method_sym)
                    {
                        result
                    } else if delegated_args.len() == 1
                        && let Some(result) = crate::builtins::native_method_1arg(
                            &coerced,
                            method_sym,
                            &delegated_args[0],
                        )
                    {
                        result
                    } else if delegated_args.len() == 2
                        && let Some(result) = crate::builtins::native_method_2arg(
                            &coerced,
                            method_sym,
                            &delegated_args[0],
                            &delegated_args[1],
                        )
                    {
                        result
                    } else {
                        self.call_method_with_values(coerced, method, delegated_args)
                    }
                }
            {
                return Ok(result);
            }
        }

        // Package (type object) dispatch -- private method call
        if let Value::Package(ref name) = target {
            let normalized_method: String = method
                .chars()
                .map(|ch| {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        ch
                    } else {
                        '_'
                    }
                })
                .collect();
            if name == "Instant" && normalized_method == "from_posix" {
                let secs = args.first().and_then(to_float_value).unwrap_or(0.0);
                let tai = crate::builtins::methods_0arg::temporal::posix_to_instant(secs);
                let mut attrs = HashMap::new();
                attrs.insert("value".to_string(), Value::Num(tai));
                return Ok(Value::make_instance(Symbol::intern("Instant"), attrs));
            }
            if name == "Supply" && method == "interval" {
                let seconds = args.first().map_or(1.0, |value| match value {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    other => other.to_string_value().parse::<f64>().unwrap_or(1.0),
                });
                let period_secs = if seconds.is_finite() && seconds > 0.0 {
                    seconds
                } else {
                    0.001 // minimum timer resolution
                };
                // Second argument: initial delay before first emission (default 0)
                let initial_delay = args.get(1).map_or(0.0, |value| match value {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    other => other.to_string_value().parse::<f64>().unwrap_or(0.0),
                });
                let initial_delay = if initial_delay.is_finite() && initial_delay >= 0.0 {
                    initial_delay
                } else {
                    0.0
                };

                // Check for :scheduler named argument
                let scheduler = Self::named_value(&args, "scheduler");

                if let Some(sched) = scheduler {
                    // Scheduler-driven Supply.interval:
                    // Store scheduler info; the scheduler will drive emissions
                    // when progress-by is called.
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(Vec::new()));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    attrs.insert("scheduler".to_string(), sched);
                    attrs.insert("scheduler_interval".to_string(), Value::Num(period_secs));
                    attrs.insert("scheduler_delay".to_string(), Value::Num(initial_delay));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }

                let supply_id = super::native_methods::next_supply_id();
                let (tx, rx) = std::sync::mpsc::channel();
                if let Ok(mut map) = super::native_methods::supply_channel_map_pub().lock() {
                    map.insert(supply_id, rx);
                }

                std::thread::spawn(move || {
                    let period = std::time::Duration::from_secs_f64(period_secs);
                    let mut tick = 0i64;
                    // Initial delay before first emission (default 0 = immediate)
                    if initial_delay > 0.0 {
                        std::thread::sleep(std::time::Duration::from_secs_f64(initial_delay));
                    }
                    loop {
                        if tx
                            .send(super::native_methods::SupplyEvent::Emit(Value::Int(tick)))
                            .is_err()
                        {
                            break;
                        }
                        tick = tick.saturating_add(1);
                        std::thread::sleep(period);
                    }
                });

                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(Vec::new()));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("supply_id".to_string(), Value::Int(supply_id as i64));
                attrs.insert("live".to_string(), Value::Bool(false));
                return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
            }
            if let Some(private_rest) = method.strip_prefix('!') {
                let caller_class = self
                    .method_class_stack
                    .last()
                    .cloned()
                    .or_else(|| Some(self.current_package().to_string()));
                let (pm_name, resolved) = if let Some((owner_class, pm_name)) =
                    private_rest.split_once("::")
                {
                    let caller_allowed = caller_class.as_deref() == Some(owner_class)
                        || self.class_trusts.get(owner_class).is_some_and(|trusted| {
                            caller_class
                                .as_ref()
                                .is_some_and(|caller| trusted.contains(caller))
                        });
                    if !caller_allowed {
                        return Err(make_private_permission_error(pm_name, owner_class));
                    }
                    (
                        pm_name,
                        self.resolve_private_method_with_owner(
                            &name.resolve(),
                            owner_class,
                            pm_name,
                            &args,
                        ),
                    )
                } else {
                    (
                        private_rest,
                        self.resolve_private_method_any_owner(&name.resolve(), private_rest, &args),
                    )
                };
                if let Some((resolved_owner, method_def)) = resolved {
                    let caller_allowed = caller_class.as_deref() == Some(resolved_owner.as_str())
                        || self
                            .class_trusts
                            .get(&resolved_owner)
                            .is_some_and(|trusted| {
                                caller_class
                                    .as_ref()
                                    .is_some_and(|caller| trusted.contains(caller))
                            });
                    if !caller_allowed {
                        return Err(make_private_permission_error(pm_name, &name.resolve()));
                    }
                    let attrs = HashMap::new();
                    let (result, _updated) = self.run_instance_method_resolved(
                        &name.resolve(),
                        &resolved_owner,
                        method_def,
                        attrs,
                        args,
                        Some(target.clone()),
                    )?;
                    return Ok(result);
                }
                return Err(make_method_not_found_error(pm_name, &name.resolve(), true));
            }
            // Package (type object) dispatch -- check user-defined methods
            if self.has_user_method(&name.resolve(), method) {
                let attrs = HashMap::new();
                let (result, _updated) =
                    self.run_instance_method(&name.resolve(), attrs, method, args, None)?;
                return Ok(result);
            }
        }

        // Value-type dispatch for user-defined methods (e.g. `augment class Array/Hash/List`).
        // Non-instance values still need to find methods declared on their type object.
        if !matches!(target, Value::Instance { .. } | Value::Package(_)) {
            let class_name = crate::runtime::utils::value_type_name(&target);
            let dispatch_class = if self.has_user_method(class_name, method) {
                Some(class_name)
            } else if matches!(target, Value::Array(_, kind) if !kind.is_itemized())
                && self.has_user_method("Array", method)
            {
                // @-sigiled values are list-like internally, but augmenting Array methods
                // should still apply to them.
                Some("Array")
            } else {
                None
            };
            if let Some(dispatch_class) = dispatch_class {
                let attrs = HashMap::new();
                let (result, _updated) = self.run_instance_method(
                    dispatch_class,
                    attrs,
                    method,
                    args,
                    Some(target.clone()),
                )?;
                return Ok(result);
            }
        }

        if let Value::Package(type_name) = &target {
            match (type_name.resolve().as_str(), method) {
                ("CompUnit", "handle") => {
                    return Ok(Value::make_instance(
                        Symbol::intern("CompUnit::Handle"),
                        HashMap::new(),
                    ));
                }
                ("CompUnit", "globalish-package") | ("CompUnit::Handle", "globalish-package") => {
                    return Ok(Value::Package(Symbol::intern("GLOBAL")));
                }
                _ => {}
            }
        }

        // .can for Package values
        if method == "can"
            && !args.is_empty()
            && let Value::Package(ref class_name) = target
        {
            let method_name = args[0].to_string_value();
            if (self.class_has_method(&class_name.resolve(), &method_name)
                || self.has_user_method(&class_name.resolve(), &method_name))
                && let Some(val) = self.classhow_find_method(&target, &method_name)
            {
                return Ok(Value::array(vec![val]));
            }
            return Ok(Value::array(Vec::new()));
        }

        // Wildcard delegation (`handles *`) and FALLBACK method dispatch.
        // Dispatch order: wildcard delegation -> FALLBACK -> built-in fallbacks -> error.
        {
            let fallback_class = match &target {
                Value::Instance { class_name, .. } => Some(*class_name),
                Value::Package(name) => Some(*name),
                _ => None,
            };
            if let Some(ref class_name) = fallback_class {
                // Try wildcard delegation: forward to delegate attribute's object
                let wildcard_attrs = self.collect_wildcard_handles(&class_name.resolve());
                if let Value::Instance { attributes, .. } = &target {
                    for attr_var in &wildcard_attrs {
                        let attr_key = attr_var.trim_start_matches('!').trim_start_matches('.');
                        if let Some(delegate) = attributes.get(attr_key) {
                            // Try calling the method on the delegate; if it succeeds, return
                            match self.call_method_with_values(
                                delegate.clone(),
                                method,
                                args.clone(),
                            ) {
                                Ok(val) => return Ok(val),
                                Err(_) => continue, // delegate doesn't handle it either
                            }
                        }
                    }
                }

                // Try user-defined FALLBACK method
                if method != "FALLBACK" && self.has_user_method(&class_name.resolve(), "FALLBACK") {
                    let mut fallback_args = vec![Value::str(method.to_string())];
                    fallback_args.extend(args);
                    return self.call_method_with_values(target, "FALLBACK", fallback_args);
                }
            }
        }

        if let Some(callable) = self.env.get(&format!("&{}", method)).cloned()
            && matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            )
        {
            return self.call_sub_value(callable, args, true);
        }

        // Fallback methods
        match method {
            "DUMP" if args.is_empty() => match target {
                Value::Package(name) => Ok(Value::str(format!("{}()", name))),
                other => Ok(Value::str(other.to_string_value())),
            },
            "gist" if args.is_empty() => match target {
                Value::Package(name) => {
                    if crate::value::is_internal_anon_type_name(&name.resolve()) {
                        return Ok(Value::str_from("()"));
                    }
                    let resolved = name.resolve();
                    let short = resolved.split("::").last().unwrap_or(&resolved);
                    Ok(Value::str(format!("({})", short)))
                }
                other => Ok(Value::str(other.to_string_value())),
            },
            "WHERE" if args.is_empty() => {
                let type_obj_name = match &target {
                    Value::Package(name) => Some(name.resolve()),
                    Value::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                if let Some(name) = type_obj_name {
                    if !self.roles.contains_key(&name) {
                        return Err(RuntimeError::new(format!(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                            method
                        )));
                    }
                    Ok(Value::str(format!("{}|type-object", name)))
                } else {
                    Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                        method
                    )))
                }
            }
            "raku" | "perl" if args.is_empty() => match target {
                Value::Package(name) => Ok(Value::str(name.resolve())),
                Value::Junction { kind, values } => {
                    let kind_name = match kind {
                        JunctionKind::Any => "any",
                        JunctionKind::All => "all",
                        JunctionKind::One => "one",
                        JunctionKind::None => "none",
                    };
                    let mut parts = Vec::with_capacity(values.len());
                    for value in values.iter() {
                        if let Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } = value
                            && self
                                .resolve_method_with_owner(&class_name.resolve(), "raku", &[])
                                .is_some()
                        {
                            let attrs_map = (**attributes).clone();
                            if let Ok((rendered, _)) = self.run_instance_method(
                                &class_name.resolve(),
                                attrs_map,
                                "raku",
                                Vec::new(),
                                None,
                            ) {
                                parts.push(rendered.to_string_value());
                                continue;
                            }
                        }
                        let rendered = self
                            .call_method_with_values(value.clone(), "raku", Vec::new())
                            .unwrap_or_else(|_| Value::str(value.to_string_value()));
                        parts.push(rendered.to_string_value());
                    }
                    Ok(Value::str(format!("{}({})", kind_name, parts.join(", "))))
                }
                other => Ok(Value::str(other.to_string_value())),
            },
            "name" if args.is_empty() => match target {
                Value::Routine { name, .. } => Ok(Value::str(name.resolve())),
                Value::Package(name) => Ok(Value::str(name.resolve())),
                Value::Str(name) => Ok(Value::Str(name.clone())),
                Value::Sub(data) => Ok(Value::str(data.name.resolve())),
                _ => Ok(Value::Nil),
            },
            "package" if args.is_empty() => match target {
                Value::Sub(data) => Ok(Value::Package(data.package)),
                Value::Routine { package, .. } => Ok(Value::Package(package)),
                _ => Ok(Value::Nil),
            },
            "isa" if args.len() == 1 && matches!(&target, Value::Package(_)) => {
                let pkg_name = match &target {
                    Value::Package(name) => name.resolve(),
                    _ => unreachable!(),
                };
                let target_name = match args.first().cloned().unwrap_or(Value::Nil) {
                    Value::Package(name) => name.resolve(),
                    Value::Str(name) => name.to_string(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    other => other.to_string_value(),
                };
                if pkg_name == target_name {
                    return Ok(Value::Bool(true));
                }
                if let Some(mut base) = self.subsets.get(&pkg_name).map(|s| s.base.clone()) {
                    loop {
                        if base == target_name {
                            return Ok(Value::Bool(true));
                        }
                        let Some(parent_subset) = self.subsets.get(&base) else {
                            break;
                        };
                        if parent_subset.base == base {
                            break;
                        }
                        base = parent_subset.base.clone();
                    }
                }
                Ok(Value::Bool(
                    self.class_mro(&pkg_name).contains(&target_name),
                ))
            }
            "REPR" if args.is_empty() => match target {
                Value::CustomType { repr, .. } | Value::CustomTypeInstance { repr, .. } => {
                    Ok(Value::str(repr))
                }
                Value::Package(name) if self.classes.contains_key(&name.resolve()) => {
                    Ok(Value::str_from("P6opaque"))
                }
                _ => Ok(Value::str_from("P6opaque")),
            },
            "Str" | "Stringy" if args.is_empty() => match target {
                Value::Package(_) => Ok(Value::str(String::new())),
                _ => Ok(Value::str(target.to_string_value())),
            },
            "Numeric" | "Real" if args.is_empty() => {
                let num_name = match &target {
                    Value::Package(name) => Some(name.resolve()),
                    Value::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                if let Some(name) = num_name {
                    if self.roles.contains_key(&name) {
                        Ok(Value::Int(0))
                    } else {
                        Err(RuntimeError::new(format!(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                            method
                        )))
                    }
                } else {
                    Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                        method
                    )))
                }
            }
            "EVAL" if args.is_empty() => match target {
                Value::Str(code) => self.call_function("EVAL", vec![Value::Str(code)]),
                _ => Err(RuntimeError::new(
                    "X::Method::NotFound: Unknown method value dispatch (fallback disabled): EVAL",
                )),
            },
            // Metamodel::*HOW methods
            "new_type" if matches!(&target, Value::Package(n) if n.resolve().starts_with("Metamodel::")) =>
            {
                // Metamodel::PackageHOW.new_type(name => 'Foo')
                // Returns a type object (Package) with the given name
                let name = args
                    .iter()
                    .find_map(|a| {
                        if let Value::Pair(k, v) = a {
                            if k == "name" {
                                Some(v.to_string_value())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| "Anon".to_string());
                Ok(Value::Package(Symbol::intern(&name)))
            }
            // Metamodel::Primitives static methods
            _ if matches!(&target, Value::Package(n) if n == "Metamodel::Primitives") => {
                self.metamodel_primitives_dispatch(method, args)
            }
            _ => {
                // Method calls on callables compose by applying the method to the
                // callable's return value, e.g. `(*-*).abs`.
                if matches!(&target, Value::Sub(_) | Value::WeakSub(_)) {
                    use crate::ast::{Expr, Stmt};
                    use std::sync::atomic::{AtomicU64, Ordering};

                    static COMPOSE_METHOD_ID: AtomicU64 = AtomicU64::new(2_000_000);

                    let callable = match target {
                        Value::Sub(data) => Value::Sub(data),
                        Value::WeakSub(weak) => weak
                            .upgrade()
                            .map(Value::Sub)
                            .ok_or_else(|| RuntimeError::new("Callable has been freed"))?,
                        _ => Value::Nil,
                    };
                    let params = match &callable {
                        Value::Sub(data) if !data.params.is_empty() => data.params.clone(),
                        _ => vec!["_".to_string()],
                    };

                    let mut env = crate::env::Env::new();
                    env.insert("__method_compose_target__".to_string(), callable);
                    let call_args = params.iter().cloned().map(Expr::Var).collect();
                    let method_args = args.into_iter().map(Expr::Literal).collect();
                    let body = vec![Stmt::Expr(Expr::MethodCall {
                        target: Box::new(Expr::Call {
                            name: Symbol::intern("__method_compose_target__"),
                            args: call_args,
                        }),
                        name: Symbol::intern(method),
                        args: method_args,
                        modifier: None,
                        quoted: false,
                    })];
                    let id = COMPOSE_METHOD_ID.fetch_add(1, Ordering::Relaxed);
                    return Ok(Value::make_sub_with_id(
                        Symbol::intern(""),
                        Symbol::intern(&format!("<composed-method:{}>", method)),
                        params,
                        Vec::new(),
                        body,
                        false,
                        env,
                        id,
                    ));
                }

                // Before giving up, check if this is a mutating array method
                // and we have a pending Proxy subclass attribute reference.
                if matches!(
                    method,
                    "push" | "pop" | "shift" | "unshift" | "append" | "prepend"
                ) && matches!(&target, Value::Array(..))
                    && let Some((attrs_ref, attr_name)) = self.pending_proxy_subclass_attr.take()
                {
                    return self.proxy_subclass_array_mutate(&attrs_ref, &attr_name, method, &args);
                }

                Err(RuntimeError::new(format!(
                    "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                    method
                )))
            }
        }
    }

    /// Mutate an array attribute in a Proxy subclass's shared storage.
    fn proxy_subclass_array_mutate(
        &mut self,
        attrs_ref: &Arc<std::sync::Mutex<HashMap<String, Value>>>,
        attr_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut attrs = attrs_ref.lock().unwrap();
        let arr = attrs
            .entry(attr_name.to_string())
            .or_insert_with(|| Value::real_array(Vec::new()));
        if let Value::Array(items, _) = arr {
            let items = Arc::make_mut(items);
            match method {
                "push" => {
                    for arg in args {
                        items.push(arg.clone());
                    }
                }
                "pop" => {
                    if items.is_empty() {
                        return Ok(make_empty_array_failure("pop"));
                    }
                    return Ok(items.pop().unwrap_or(Value::Nil));
                }
                "shift" => {
                    return Ok(if items.is_empty() {
                        Value::Nil
                    } else {
                        items.remove(0)
                    });
                }
                "unshift" => {
                    for (i, arg) in args.iter().enumerate() {
                        items.insert(i, arg.clone());
                    }
                }
                "append" => {
                    items.extend(flatten_append_args(args.to_vec()));
                }
                "prepend" => {
                    let mut new_items: Vec<Value> = Vec::new();
                    for arg in args {
                        match arg {
                            Value::Array(vals, _) => new_items.extend(vals.iter().cloned()),
                            other => new_items.push(other.clone()),
                        }
                    }
                    new_items.append(items);
                    *items = new_items;
                }
                _ => {}
            }
            Ok(Value::real_array(items.clone()))
        } else {
            Err(RuntimeError::new(format!(
                "Cannot call '{}' on non-Array attribute",
                method
            )))
        }
    }

    pub(super) fn dispatch_are(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let values = Self::value_to_list(&target);
        match args {
            [] => {
                if values.is_empty() {
                    return Ok(Value::Nil);
                }
                let candidates = self.are_candidate_type_names(&values);
                for candidate in candidates {
                    if values
                        .iter()
                        .all(|value| self.are_value_matches_type(value, &candidate))
                    {
                        return Ok(Value::Package(Symbol::intern(&candidate)));
                    }
                }
                Ok(Value::Package(Symbol::intern("Any")))
            }
            [expected] => {
                let expected_type = self.are_expected_type_name(expected);
                for (idx, value) in values.iter().enumerate() {
                    if !self.are_value_matches_type(value, &expected_type) {
                        let actual = Self::are_actual_type_name(value);
                        let message = if values.len() == 1 {
                            format!("Expected '{}' but got '{}'", expected_type, actual)
                        } else {
                            format!(
                                "Expected '{}' but got '{}' in element {}",
                                expected_type, actual, idx
                            )
                        };
                        return Err(RuntimeError::new(message));
                    }
                }
                Ok(Value::Bool(true))
            }
            _ => Err(RuntimeError::new(
                "Method 'are' accepts zero or one argument",
            )),
        }
    }

    fn are_candidate_type_names(&mut self, values: &[Value]) -> Vec<String> {
        let mut names = Vec::new();
        for value in values {
            for candidate in self.are_specific_candidate_type_names(value) {
                if !names.contains(&candidate) {
                    names.push(candidate);
                }
            }
        }
        for fallback in ["Dateish", "Real", "Numeric", "Cool", "Any"] {
            let fallback = fallback.to_string();
            if !names.contains(&fallback) {
                names.push(fallback);
            }
        }
        names
    }

    fn are_specific_candidate_type_names(&mut self, value: &Value) -> Vec<String> {
        match value {
            Value::Package(name) => vec![name.resolve()],
            Value::Instance { class_name, .. } => self.class_mro(&class_name.resolve()),
            _ => vec![crate::runtime::utils::value_type_name(value).to_string()],
        }
    }

    fn are_expected_type_name(&self, value: &Value) -> String {
        match value {
            Value::Package(name) => name.resolve(),
            Value::Str(name) => name.to_string(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            _ => value.to_string_value(),
        }
    }

    fn are_value_matches_type(&mut self, value: &Value, expected_type: &str) -> bool {
        if expected_type == "Cool" {
            // `Cool` in are() should accept list/type-object values except clearly non-Cool ones.
            if let Value::Instance { class_name, .. } = value {
                let cls = class_name.resolve();
                if cls == "Date" || cls == "DateTime" || cls == "Mu" {
                    return false;
                }
            }
            if let Value::Package(name) = value {
                let cls = name.resolve();
                if cls == "Date" || cls == "DateTime" || cls == "Mu" {
                    return false;
                }
            }
            if matches!(value, Value::Array(_, _) | Value::Seq(_) | Value::Slip(_)) {
                return true;
            }
        }
        self.type_matches_value(expected_type, value)
    }

    fn are_actual_type_name(value: &Value) -> String {
        match value {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            _ => crate::runtime::utils::value_type_name(value).to_string(),
        }
    }
}
