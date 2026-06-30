use super::*;
use crate::symbol::Symbol;

fn is_datetime_constructor_named_arg(key: &str) -> bool {
    matches!(
        key,
        "year" | "month" | "day" | "hour" | "minute" | "second" | "timezone" | "date" | "formatter"
    )
}

fn is_normalized_datetime_subclass_ctor_args(args: &[Value]) -> bool {
    if !matches!(
        args.first(),
        Some(Value::Instance { class_name, .. }) if class_name == "DateTime"
    ) {
        return false;
    }
    args.iter()
        .skip(1)
        .all(|arg| matches!(arg, Value::Pair(key, _) if !is_datetime_constructor_named_arg(key)))
}

/// Build a typed `X::Constructor::Positional` for a default constructor that
/// was handed positional arguments (e.g. `Mu.new(1)`). The `type` attribute is
/// the type object so the test matcher `type => Foo` accepts it.
fn constructor_positional_error(class_name: &str) -> RuntimeError {
    let msg = format!(
        "Default constructor for '{}' only takes named arguments",
        class_name
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert(
        "type".to_string(),
        Value::Package(Symbol::intern(class_name)),
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::Constructor::Positional"),
        attrs,
    )));
    err
}

impl Interpreter {
    /// Materialize a user `message` *method* into the `message` attribute of a
    /// freshly-constructed exception. raku computes `Exception.message` lazily,
    /// but mutsu's interpreter-less stringification (`join`, `sprintf "%s"`, `~`,
    /// interpolation -> `to_string_value`) cannot dispatch a user method, so such
    /// an exception would stringify as "X::Foo with no message" outside an
    /// explicit `.Str`/`.gist` call. Running the (conventionally pure) message
    /// method once at construction and caching the result keeps every
    /// stringification path coherent. Scoped to exceptions that define `message`
    /// as a method and have no `message` attribute, so built-in and
    /// attribute-message exceptions are unaffected. Errors in the message method
    /// are swallowed (the exception is still returned un-materialized).
    pub(crate) fn materialize_exception_message_in_result(
        &mut self,
        result: Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        let Ok(Value::Instance {
            class_name,
            attributes,
            id,
        }) = result
        else {
            return result;
        };
        let cn = class_name.resolve();
        let is_exc = cn == "Exception" || cn.starts_with("X::") || cn.starts_with("CX::");
        if !is_exc
            || attributes.as_map().contains_key("message")
            || !self.has_user_method(&cn, "message")
        {
            return Ok(Value::Instance {
                class_name,
                attributes,
                id,
            });
        }
        let instance = Value::Instance {
            class_name,
            attributes: attributes.clone(),
            id,
        };
        if let Ok(msg) = self.call_method_with_values(instance.clone(), "message", vec![]) {
            let msg_str = msg.to_string_value();
            if !msg_str.is_empty() {
                let mut attrs = attributes.as_map().clone();
                attrs.insert("message".to_string(), Value::str(msg_str));
                return Ok(Value::make_instance(class_name, attrs));
            }
        }
        Ok(instance)
    }

    pub(super) fn dispatch_new(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Package(_) = &target {
            let materialized = self.materialize_default_parametric_role(target.clone())?;
            if materialized != target {
                return self.dispatch_new(materialized, args);
            }
        }
        // Collation.new — create a Collation instance with default settings
        if let Value::Package(name) = &target
            && name == "Collation"
        {
            return Ok(Self::make_collation_instance(1, 1, 1, 1));
        }
        // Calling .new() on an instance delegates to the class constructor
        if let Value::Instance { class_name, .. } = &target {
            return self.dispatch_new(Value::Package(*class_name), args);
        }
        // Calling .new() on a Mixin(Instance{..}, ..) delegates to the class constructor
        if let Value::Mixin(inner, _) = &target
            && let Value::Instance { class_name, .. } = inner.as_ref()
        {
            return self.dispatch_new(Value::Package(*class_name), args);
        }
        // Calling .new() on a concrete Array delegates to the type constructor.
        // If the array has type metadata (e.g. array[str]), use the declared type.
        if let Value::Array(..) = &target {
            let type_name = if let Some(info) = self.container_type_metadata(&target) {
                if let Some(ref dt) = info.declared_type {
                    dt.clone()
                } else if info.value_type != "Any" && info.value_type != "Mu" {
                    // Construct the parametric type name from value_type
                    format!("array[{}]", info.value_type)
                } else {
                    "Array".to_string()
                }
            } else {
                "Array".to_string()
            };
            return self.dispatch_new(Value::Package(Symbol::intern(&type_name)), args);
        }
        // Calling .new() on a concrete object hash (`%h{KeyType}`) produces a
        // new object hash of the same key/value type, not a plain Hash — mirror
        // the typed-Array `.new` path above.
        if let Value::Hash(_) = &target
            && let Some(info) = self.container_type_metadata(&target)
            && let Some(kt) = info.key_type.clone()
        {
            let vt = if info.value_type.is_empty() {
                "Any".to_string()
            } else {
                info.value_type.clone()
            };
            let pkg = format!("Hash[{},{}]", vt, kt);
            return self.dispatch_new(Value::Package(Symbol::intern(&pkg)), args);
        }
        // Calling .new() on a concrete Hash/Bag/Mix delegates to the type constructor
        {
            let type_pkg = match &target {
                Value::Hash(_) => Some("Hash"),
                Value::Bag(_, _) => Some("BagHash"),
                Value::Mix(_, _) => Some("MixHash"),
                _ => None,
            };
            if let Some(type_name) = type_pkg {
                return self.dispatch_new(Value::Package(Symbol::intern(type_name)), args);
            }
        }
        if let Value::Str(ref name) = target
            && self.registry().enum_types.contains_key(name.as_str())
        {
            let msg = format!(
                "Enum '{}' is insufficiently type-like to be instantiated.  Did you mean 'class'?",
                name
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Constructor::BadType"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        // Calling `.new(...)` on a concrete builtin scalar / Pair instance delegates
        // to that type's constructor (raku: `$pair.new(:key<k>, :value<v>)`,
        // `42.new` => Int.new => 0, `"x".new` => Str.new => ""). Placed after the
        // enum-name `Str` check above so a Str holding an enum type name still
        // throws X::Constructor::BadType rather than delegating to Str.new.
        {
            let type_pkg = match &target {
                Value::Pair(..) | Value::ValuePair(..) => Some("Pair"),
                Value::Int(_) | Value::BigInt(_) => Some("Int"),
                Value::Num(_) => Some("Num"),
                Value::Rat(..) => Some("Rat"),
                Value::FatRat(..) => Some("FatRat"),
                Value::Complex(..) => Some("Complex"),
                Value::Str(_) => Some("Str"),
                _ => None,
            };
            if let Some(type_name) = type_pkg {
                return self.dispatch_new(Value::Package(Symbol::intern(type_name)), args);
            }
        }
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = &target
        {
            let base_name_str = base_name.resolve();
            self.ensure_role_punned_to_class(&base_name_str);
            let mut selected_role = self.registry().roles.get(&base_name_str).cloned();
            let mut matched_lang_version: Option<String> = None;
            let mut selected_param_names = self
                .registry()
                .role_type_params
                .get(&base_name_str)
                .cloned()
                .unwrap_or_default();
            // Hoist clone to a `let` so the guard drops before the filter_map
            // closure re-enters (&mut self).
            let candidates = self.registry().role_candidates.get(&base_name_str).cloned();
            if let Some(candidates) = candidates {
                let mut matching: Vec<(super::RoleCandidateDef, i32, usize)> = candidates
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
                                && (has_positional_slurpy
                                    || type_args.len() <= positional_params.len())
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
                    matched_lang_version = Some(candidate.language_version.clone());
                    selected_role = Some(candidate.role_def.clone());
                }
            }
            if let Some(role) = selected_role {
                let role_id = role.role_id;
                let mut named_args: HashMap<String, Value> = HashMap::new();
                let mut positional_args: Vec<Value> = Vec::new();
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        named_args.insert(key.clone(), *value.clone());
                    } else {
                        positional_args.push(arg.clone());
                    }
                }

                let mut mixins = HashMap::new();
                mixins.insert(format!("__mutsu_role__{}", base_name), Value::Bool(true));
                mixins.insert(
                    format!("__mutsu_role_typeargs__{}", base_name),
                    Value::array(type_args.clone()),
                );
                if role_id != 0 {
                    mixins.insert(
                        format!("__mutsu_role_id__{}", base_name),
                        Value::Int(role_id as i64),
                    );
                }
                for (param_name, type_arg) in selected_param_names.iter().zip(type_args.iter()) {
                    mixins.insert(
                        format!("__mutsu_role_param__{}", param_name),
                        type_arg.clone(),
                    );
                }
                let saved_role_param_env = self.env.clone();
                for (param_name, type_arg) in selected_param_names.iter().zip(type_args.iter()) {
                    self.env.insert(param_name.clone(), type_arg.clone());
                }
                for (idx, (attr_name, _is_public, default_expr, _, _, _, _)) in
                    role.attributes.iter().enumerate()
                {
                    let value = if let Some(v) = named_args.get(attr_name) {
                        v.clone()
                    } else if let Some(v) = positional_args.get(idx) {
                        v.clone()
                    } else if let Some(expr) = default_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                    } else {
                        Value::Nil
                    };
                    mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
                }
                self.env = saved_role_param_env;
                // Embed language revision in mixin metadata so
                // ^language-revision on the punned instance returns
                // the revision of the matched candidate.
                if let Some(ref ver) = matched_lang_version {
                    let revision: String = if let Some(letter) = ver.strip_prefix("6.") {
                        letter.chars().next().unwrap_or('c').to_string()
                    } else {
                        "c".to_string()
                    };
                    mixins.insert(
                        "__mutsu_language_revision".to_string(),
                        Value::str(revision),
                    );
                }
                return Ok(Value::mixin(
                    Value::make_instance(*base_name, HashMap::new()),
                    mixins,
                ));
            }
        }

        if let Value::Package(class_name) = &target {
            let cn_resolved = class_name.resolve();
            // Fast path: user-defined class with no BUILD/TWEAK/custom new,
            // only simple parents (Any/Mu/Cool), only $-sigiled attributes,
            // and no native methods. Shared with the VM so `Foo.new(...)` for
            // such classes is constructed without entering the generic method
            // dispatch machinery (see `VM::try_native_default_construct`).
            if let Some(result) = self.try_native_default_construct(*class_name, &args) {
                return result;
            }
            let parametric = Self::parse_parametric_type_name(&cn_resolved);
            let (base_class_name, type_args) = if let Some((base, args)) = &parametric {
                (base.as_str(), Some(args.clone()))
            } else {
                (cn_resolved.as_str(), None)
            };
            if cn_resolved.starts_with("IO::Path::")
                && !self.registry().classes.contains_key(&cn_resolved)
            {
                self.registry_mut().classes.insert(
                    cn_resolved.clone(),
                    ClassDef {
                        parents: vec!["IO::Path".to_string()],
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        native_methods: std::collections::HashSet::new(),
                        mro: Vec::new(),
                        attribute_types: HashMap::new(),
                        attribute_smileys: HashMap::new(),
                        attribute_built: HashMap::new(),
                        wildcard_handles: Vec::new(),
                        alias_attributes: HashSet::new(),
                        class_level_attrs: HashMap::new(),
                    },
                );
            }
            // IO::Spec::* types: create an IO::Spec instance with the spec name
            if cn_resolved.starts_with("IO::Spec::") {
                let attrs = HashMap::new();
                return Ok(Value::make_instance(Symbol::intern(&cn_resolved), attrs));
            }
            let class_key = if self.registry().classes.contains_key(&cn_resolved) {
                cn_resolved.as_str()
            } else {
                base_class_name
            };
            let is_datetime_subclass = cn_resolved != "DateTime"
                && self
                    .class_mro(class_key)
                    .iter()
                    .any(|name| name == "DateTime");
            if is_datetime_subclass && !is_normalized_datetime_subclass_ctor_args(&args) {
                let positional_args: Vec<Value> = args
                    .iter()
                    .filter(|arg| !matches!(arg, Value::Pair(_, _)))
                    .cloned()
                    .collect();
                let mut datetime_ctor_args = Vec::new();
                if positional_args.len() >= 3 {
                    datetime_ctor_args.push(Value::Pair(
                        "year".to_string(),
                        Box::new(positional_args[0].clone()),
                    ));
                    datetime_ctor_args.push(Value::Pair(
                        "month".to_string(),
                        Box::new(positional_args[1].clone()),
                    ));
                    datetime_ctor_args.push(Value::Pair(
                        "day".to_string(),
                        Box::new(positional_args[2].clone()),
                    ));
                    if let Some(hour) = positional_args.get(3) {
                        datetime_ctor_args
                            .push(Value::Pair("hour".to_string(), Box::new(hour.clone())));
                    }
                    if let Some(minute) = positional_args.get(4) {
                        datetime_ctor_args
                            .push(Value::Pair("minute".to_string(), Box::new(minute.clone())));
                    }
                    if let Some(second) = positional_args.get(5) {
                        datetime_ctor_args
                            .push(Value::Pair("second".to_string(), Box::new(second.clone())));
                    }
                    for arg in &args {
                        if let Value::Pair(key, _) = arg
                            && is_datetime_constructor_named_arg(key)
                        {
                            datetime_ctor_args.push(arg.clone());
                        }
                    }
                } else {
                    datetime_ctor_args = args.clone();
                }
                let datetime = self.dispatch_new(
                    Value::Package(Symbol::intern("DateTime")),
                    datetime_ctor_args,
                )?;
                let mut normalized_args = vec![datetime];
                for arg in &args {
                    if let Value::Pair(key, _) = arg
                        && !is_datetime_constructor_named_arg(key)
                    {
                        normalized_args.push(arg.clone());
                    }
                }
                return self.dispatch_new(target.clone(), normalized_args);
            }
            // Date subclass handling: delegate Date-specific constructor logic
            // then merge custom attributes via the generic constructor.
            let is_date_subclass = cn_resolved != "Date"
                && self.class_mro(class_key).iter().any(|name| name == "Date");
            // Only delegate if the args don't already contain Date attrs
            // (prevents infinite recursion on second call)
            let has_date_named_args = args.iter().any(|a| {
                matches!(a, Value::Pair(k, _) if k == "year" || k == "month" || k == "day" || k == "days")
            });
            if is_date_subclass && !has_date_named_args && !self.has_user_method(class_key, "new") {
                // Build Date first to extract year/month/day/days/formatter
                let date =
                    self.dispatch_new(Value::Package(Symbol::intern("Date")), args.clone())?;
                if let Value::Instance { attributes, .. } = &date {
                    // Now build the subclass instance with all Date attrs plus any custom named attrs
                    let mut new_args = Vec::new();
                    for (k, v) in attributes.as_map().iter() {
                        new_args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                    }
                    // Add any non-Date named args from the original call
                    for arg in &args {
                        if let Value::Pair(key, _) = arg
                            && !attributes.contains_key(key.as_str())
                        {
                            new_args.push(arg.clone());
                        }
                    }
                    return self.dispatch_new(target.clone(), new_args);
                }
            }
            let is_io_path_like = base_class_name == "IO::Path"
                || self
                    .class_mro(class_key)
                    .iter()
                    .any(|name| name == "IO::Path");
            if is_io_path_like && !self.has_user_method(class_key, "new") {
                // Pure path-string assembly (registry reads only) — shared with the
                // VM's `try_native_io_path_construct`.
                return self.build_io_path_instance(*class_name, &cn_resolved, &args);
            }
            match base_class_name {
                "IO::CatHandle" if !self.has_user_method(class_key, "new") => {
                    return Ok(self.build_io_cathandle(&args));
                }
                "IterationBuffer" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_iterationbuffer_value(*class_name, &args));
                }
                "Array" | "List" | "Positional" | "array" => {
                    // Shared single implementation with the VM's native fast path.
                    return self.try_native_array_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        &args,
                    );
                }
                "Hash" | "Map" => {
                    // Shared single implementation with the VM's native fast path.
                    return self.try_native_hash_construct(*class_name, &type_args, &args);
                }
                "Uni" => {
                    // Shared with the VM's native fast path (pure codepoint build).
                    return Ok(Self::build_native_uni_value(&args));
                }
                "Seq" => {
                    // Shared single implementation with the VM's native fast path
                    // (`try_native_seq_construct`). Reads/writes only VM-owned state
                    // (the `predictive_seq_iters` carrier table + the deferred-iter
                    // side table keyed off the Seq's own Arc).
                    return Ok(self.try_native_seq_construct(&args));
                }
                "Version" => {
                    let arg = args.first().cloned().unwrap_or(Value::Nil);
                    return Ok(Self::version_from_value(arg));
                }
                "Duration" => {
                    // Shared with the VM's native fast path (pure Rational build;
                    // a bad string arg is the one fallible built-in builder).
                    return Self::build_native_duration_value(&args);
                }
                "StrDistance" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_strdistance_value(&args));
                }
                "Date" => {
                    // Shared with the VM's native fast path. Only the formatter
                    // case needs `self` (it renders a user Callable).
                    let (date, formatter) = Self::build_native_date(&args)?;
                    if let Some(formatter_value) = formatter {
                        return self.render_date_formatter(date, formatter_value);
                    }
                    return Ok(date);
                }
                "DateTime" => {
                    // Shared with the VM's native fast path. Only the
                    // `:formatter` case needs `self` (it renders a user
                    // Callable); the common case is built natively.
                    let (dt, formatter) = Self::build_native_datetime(&args)?;
                    if let Some(formatter_value) = formatter
                        && let Value::Instance {
                            class_name,
                            ref attributes,
                            id,
                        } = dt
                    {
                        let mut attrs = attributes.to_map();
                        attrs.insert("formatter".to_string(), formatter_value.clone());
                        let dt_with_formatter =
                            Value::write_back_sharing(attributes, class_name, attrs, id);
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
                            let mut updated = attributes.to_map();
                            updated
                                .insert("__formatter_rendered".to_string(), Value::str(rendered));
                            return Ok(Value::write_back_sharing(
                                &attributes,
                                class_name,
                                updated,
                                id,
                            ));
                        }
                    }
                    return Ok(dt);
                }
                "IO::Socket::INET" => {
                    // Shared single implementation with the VM's native fast
                    // path. The real bind/connect writes only VM-owned
                    // `io_handles` state (same shape as the native `IO::Path.open`).
                    return self.dispatch_socket_inet_new(&args);
                }
                "Promise" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    return Ok(Value::Promise(SharedPromise::new()));
                }
                "Channel" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    return Ok(Value::Channel(SharedChannel::new()));
                }
                "Stash" => {
                    // Stash is essentially a Hash but with type Stash
                    return Ok(Value::make_instance(
                        Symbol::intern("Stash"),
                        HashMap::new(),
                    ));
                }
                "Supply" => return Ok(self.make_supply_instance()),
                "Supplier" | "Supplier::Preserving" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    let mut attrs = HashMap::new();
                    attrs.insert("emitted".to_string(), Value::array(Vec::new()));
                    attrs.insert("done".to_string(), Value::Bool(false));
                    attrs.insert(
                        "supplier_id".to_string(),
                        Value::Int(super::native_methods::next_supplier_id() as i64),
                    );
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                "ThreadPoolScheduler" | "CurrentThreadScheduler" | "Tap" | "Cancellation" => {
                    return Ok(Value::make_instance(*class_name, HashMap::new()));
                }
                "FakeScheduler" => {
                    // Shared single implementation with the VM's native fast path.
                    return Ok(Self::build_native_fakescheduler_value());
                }
                "Proxy" => {
                    // Shared single implementation with the VM's native fast path.
                    return Ok(Self::build_native_proxy_value(&args));
                }
                "CompUnit::DependencySpecification" => {
                    let mut short_name: Option<String> = None;
                    let mut auth_matcher: Option<String> = None;
                    let mut version_matcher: Option<String> = None;
                    let mut api_matcher: Option<String> = None;
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            match key.as_str() {
                                "short-name" => {
                                    if let Value::Str(s) = value.as_ref() {
                                        short_name = Some(s.to_string());
                                    } else {
                                        return Err(RuntimeError::new(
                                            "CompUnit::DependencySpecification.new: :short-name must be a Str",
                                        ));
                                    }
                                }
                                "auth-matcher" => {
                                    if !matches!(value.as_ref(), Value::Bool(true)) {
                                        auth_matcher = Some(value.to_string_value());
                                    }
                                }
                                "version-matcher" => {
                                    if !matches!(value.as_ref(), Value::Bool(true)) {
                                        version_matcher = Some(value.to_string_value());
                                    }
                                }
                                "api-matcher" if !matches!(value.as_ref(), Value::Bool(true)) => {
                                    api_matcher = Some(value.to_string_value());
                                }
                                _ => {}
                            }
                        }
                    }
                    let short_name = short_name.ok_or_else(|| {
                        RuntimeError::new(
                            "CompUnit::DependencySpecification.new: :short-name is required",
                        )
                    })?;
                    if auth_matcher.is_some() || version_matcher.is_some() || api_matcher.is_some()
                    {
                        let mut attrs = HashMap::new();
                        attrs.insert("short-name".to_string(), Value::str(short_name));
                        if let Some(a) = auth_matcher {
                            attrs.insert("auth-matcher".to_string(), Value::str(a));
                        }
                        if let Some(v) = version_matcher {
                            attrs.insert("version-matcher".to_string(), Value::str(v));
                        }
                        if let Some(a) = api_matcher {
                            attrs.insert("api-matcher".to_string(), Value::str(a));
                        }
                        return Ok(Value::make_instance(
                            Symbol::intern("CompUnit::DependencySpecification"),
                            attrs,
                        ));
                    }
                    return Ok(Value::CompUnitDepSpec {
                        short_name: Symbol::intern(&short_name),
                    });
                }
                "Distribution::Path" => {
                    let dir_path = args
                        .first()
                        .map(Value::to_string_value)
                        .unwrap_or_else(|| ".".to_string());
                    let meta_path = std::path::Path::new(&dir_path).join("META6.json");
                    if !meta_path.exists() {
                        return Err(RuntimeError::new(format!(
                            "No meta file located at {}",
                            meta_path.display()
                        )));
                    }
                    let meta_json = std::fs::read_to_string(&meta_path).map_err(|e| {
                        RuntimeError::new(format!("Cannot read {}: {e}", meta_path.display()))
                    })?;
                    let meta_hash = self.parse_json_to_value(&meta_json)?;
                    let files_hash = self.build_dist_files_hash(&dir_path, &meta_hash);
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(&dir_path));
                    attrs.insert("meta".to_string(), meta_hash);
                    attrs.insert("files".to_string(), files_hash);
                    return Ok(Value::make_instance(
                        Symbol::intern("Distribution::Path"),
                        attrs,
                    ));
                }
                "Distribution::Hash" => {
                    let mut meta_hash = Value::Nil;
                    let mut prefix = String::new();
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "prefix" => {
                                prefix = value.to_string_value();
                            }
                            Value::Hash(_) => {
                                if meta_hash == Value::Nil {
                                    meta_hash = arg.clone();
                                }
                            }
                            _ => {
                                if meta_hash == Value::Nil {
                                    meta_hash = arg.clone();
                                }
                            }
                        }
                    }
                    let files_hash = self.build_dist_files_hash(&prefix, &meta_hash);
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
                    attrs.insert("meta".to_string(), meta_hash);
                    attrs.insert("files".to_string(), files_hash);
                    return Ok(Value::make_instance(
                        Symbol::intern("Distribution::Hash"),
                        attrs,
                    ));
                }
                "CompUnit::Repository::Installation" => {
                    let mut prefix = String::new();
                    for arg in &args {
                        if let Value::Pair(key, value) = arg
                            && key == "prefix"
                        {
                            prefix = value.to_string_value();
                        }
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert("prefix".to_string(), self.make_io_path_instance(&prefix));
                    attrs.insert("short-id".to_string(), Value::str_from("inst"));
                    return Ok(Value::make_instance(
                        Symbol::intern("CompUnit::Repository::Installation"),
                        attrs,
                    ));
                }
                "CompUnit::Repository::FileSystem" => {
                    let mut prefix = ".".to_string();
                    for arg in &args {
                        if let Value::Pair(key, value) = arg
                            && key == "prefix"
                        {
                            prefix = value.to_string_value();
                        }
                    }
                    let prefix_path = if prefix.is_empty() { "." } else { &prefix };
                    let canonical_prefix = std::fs::canonicalize(prefix_path)
                        .unwrap_or_else(|_| std::path::PathBuf::from(prefix_path))
                        .to_string_lossy()
                        .to_string();
                    let cache_key = format!("__mutsu_repo_fs::{}", canonical_prefix);
                    if let Some(existing) = self.env.get(&cache_key).cloned() {
                        return Ok(existing);
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "prefix".to_string(),
                        self.make_io_path_instance(&canonical_prefix),
                    );
                    attrs.insert("short-id".to_string(), Value::str_from("file"));
                    attrs.insert("__mutsu_precomp_enabled".to_string(), Value::Bool(false));
                    let repo = Value::make_instance(*class_name, attrs);
                    self.env.insert(cache_key, repo.clone());
                    return Ok(repo);
                }
                "Proc::Async" => {
                    // Shared single implementation with the VM's native fast
                    // path (`try_native_builtin_construct`). Pure data assembly:
                    // arg parsing + process-global supply ids + empty Supply
                    // attributes. The process is only spawned later by `.start`.
                    return Ok(Self::build_native_proc_async_value(*class_name, &args));
                }
                "utf8" | "utf16" => {
                    // Shared with the VM's native fast path (pure code-unit build).
                    return Ok(Self::build_native_utf_value(*class_name, &args));
                }
                "Buf" | "buf8" | "Buf[uint8]" | "Blob" | "blob8" | "Blob[uint8]" | "buf16"
                | "buf32" | "buf64" | "blob16" | "blob32" | "blob64" => {
                    // Shared with the VM's native fast path (the byte-overlay
                    // build is pure data; see `build_native_buf_value`).
                    return Ok(Self::build_native_buf_value(*class_name, &args));
                }
                "Rat" => {
                    // Shared with the VM's native fast path (pure component build).
                    return Ok(Self::build_native_rat_value(&args));
                }
                "FatRat" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_fatrat_value(&args));
                }
                "Pair" => {
                    // Shared with the VM's native fast path.
                    return Ok(Self::build_native_pair_value(&args));
                }
                "Set" | "SetHash" => {
                    // Native QuantHash construction — single impl shared with the
                    // VM's `.new` fast path (`try_native_quanthash_construct`).
                    return self.try_native_quanthash_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        args,
                    );
                }
                "Bag" | "BagHash" => {
                    // Native QuantHash construction — single impl shared with the
                    // VM's `.new` fast path (`try_native_quanthash_construct`).
                    return self.try_native_quanthash_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        args,
                    );
                }
                "Mix" | "MixHash" => {
                    // Native QuantHash construction — single impl shared with the
                    // VM's `.new` fast path (`try_native_quanthash_construct`).
                    return self.try_native_quanthash_construct(
                        *class_name,
                        base_class_name,
                        &type_args,
                        args,
                    );
                }
                "Complex" => {
                    // Shared with the VM's native fast path (pure component build).
                    return Ok(Self::build_native_complex_value(&args));
                }
                "Backtrace" => {
                    let file = self
                        .env
                        .get("?FILE")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let mut frame_attrs = HashMap::new();
                    frame_attrs.insert("file".to_string(), Value::str(file));
                    let frame =
                        Value::make_instance(Symbol::intern("Backtrace::Frame"), frame_attrs);
                    return Ok(Value::array(vec![frame]));
                }
                "Lock" | "Lock::Async" | "Lock::Soft" => {
                    // Shared with the VM's native fast path
                    // (`try_native_builtin_construct`).
                    let mut attrs = HashMap::new();
                    let lock_id = super::native_methods::next_lock_id() as i64;
                    attrs.insert("lock-id".to_string(), Value::Int(lock_id));
                    if class_name.resolve() == "Lock::Async" {
                        attrs.insert("async".to_string(), Value::Bool(true));
                    }
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                "Slip" => {
                    // Shared with the VM's native fast path.
                    return Ok(Value::slip(args.to_vec()));
                }
                "Match" => {
                    // Shared single implementation with the VM's native fast path.
                    return Ok(Self::build_native_match_value(&args));
                }
                // Types that cannot be instantiated with .new
                "HyperWhatever" | "Whatever" | "Instant" => {
                    return Err(RuntimeError::new(format!(
                        "X::Cannot::New: Cannot create new object of type {}",
                        class_name
                    )));
                }
                "Junction" => {
                    // Junction.new(values, :type<kind>)
                    // or Junction.new("kind", values)
                    // Extract named :type from Pair args
                    let mut type_str: Option<String> = None;
                    let mut positional: Vec<&Value> = Vec::new();
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            if key == "type" {
                                type_str = Some(value.to_string_value());
                            }
                        } else {
                            positional.push(arg);
                        }
                    }
                    // If no named :type, check if first positional is a type string
                    let type_name = if let Some(t) = type_str {
                        t
                    } else if positional.len() >= 2 {
                        if let Value::Str(s) = positional[0] {
                            let name = s.to_string();
                            positional.remove(0);
                            name
                        } else {
                            "any".to_string()
                        }
                    } else {
                        "any".to_string()
                    };
                    let kind = match type_name.as_str() {
                        "all" => JunctionKind::All,
                        "one" => JunctionKind::One,
                        "none" => JunctionKind::None,
                        _ => JunctionKind::Any,
                    };
                    let values_arg = positional.first().copied();
                    let elems: Vec<Value> = match values_arg {
                        Some(Value::Array(items, ..)) => items.to_vec(),
                        Some(Value::Seq(items)) | Some(Value::Slip(items)) => items.to_vec(),
                        Some(other) => vec![other.clone()],
                        None => vec![],
                    };
                    return Ok(Value::junction(kind, elems));
                }
                _ => {}
            }
            // Parametric package handling (e.g. Array[Int], Hash[Int,Str], A[Int]).
            if let Some(type_args) = type_args.as_ref() {
                if matches!(base_class_name, "Array" | "List" | "Positional" | "array") {
                    if let Some(dims) = self.shaped_dims_from_new_args(&args)? {
                        let data = args.iter().find_map(|arg| match arg {
                            Value::Pair(name, value) if name == "data" => {
                                Some(value.as_ref().clone())
                            }
                            _ => None,
                        });
                        // If no :data, collect positional (non-Pair) args as data
                        let data = if data.is_none() {
                            let positional: Vec<Value> = args
                                .iter()
                                .filter(|arg| !matches!(arg, Value::Pair(..)))
                                .cloned()
                                .collect();
                            if positional.is_empty() {
                                None
                            } else {
                                Some(Value::array(positional))
                            }
                        } else {
                            data
                        };
                        let shaped = Self::make_shaped_array(&dims)?;
                        let mut result = if let Some(data_val) = data {
                            let data_items = match data_val {
                                Value::Array(items, ..) => items.to_vec(),
                                Value::Seq(items) | Value::Slip(items) => items.to_vec(),
                                other => vec![other],
                            };
                            if let Value::Array(ref items, is_arr) = shaped {
                                let mut new_items = items.as_ref().clone();
                                for (i, val) in data_items.into_iter().enumerate() {
                                    if i < new_items.len() {
                                        new_items[i] = val;
                                    }
                                }
                                let result = Value::Array(std::sync::Arc::new(new_items), is_arr);
                                crate::runtime::utils::mark_shaped_array(&result, Some(&dims));
                                result
                            } else {
                                shaped
                            }
                        } else {
                            shaped
                        };
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        result = self.tag_container_metadata(
                            result,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(if base_class_name == "array" {
                                    format!("array[{}]", type_args[0])
                                } else {
                                    class_name.resolve()
                                }),
                            },
                        );
                        return Ok(result);
                    }
                    let mut items = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Slip(vals) => items.extend(vals.iter().cloned()),
                            other => items.push(other.clone()),
                        }
                    }
                    let mut result = if matches!(base_class_name, "Array" | "array") {
                        Value::real_array(items)
                    } else {
                        Value::array(items)
                    };
                    let value_type = type_args
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Any".to_string());
                    result = self.tag_container_metadata(
                        result,
                        crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type: None,
                            declared_type: Some(if base_class_name == "array" {
                                format!("array[{}]", type_args[0])
                            } else {
                                class_name.resolve()
                            }),
                        },
                    );
                    return Ok(result);
                }
                if matches!(base_class_name, "Hash" | "Map") {
                    let mut flat = Vec::new();
                    for arg in &args {
                        flat.extend(Self::value_to_list(arg));
                    }
                    let mut map = HashMap::new();
                    let mut iter = flat.into_iter();
                    while let Some(item) = iter.next() {
                        match item {
                            Value::Pair(k, v) => {
                                map.insert(k, *v);
                            }
                            Value::ValuePair(k, v) => {
                                map.insert(k.to_string_value(), *v);
                            }
                            other => {
                                let key = other.to_string_value();
                                let value = iter.next().unwrap_or(Value::Nil);
                                map.insert(key, value);
                            }
                        }
                    }
                    let result = Value::hash(map);
                    let value_type = type_args
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Any".to_string());
                    let key_type = type_args.get(1).cloned();
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type,
                        key_type,
                        declared_type: Some(class_name.resolve()),
                    };
                    return Ok(self.tag_container_metadata(result, info));
                }
            }
            // Hoist clone to a `let` so the guard drops before the body re-enters
            // (eval_block_value for attribute defaults).
            let role = self.registry().roles.get(&class_name.resolve()).cloned();
            if let Some(role) = role {
                // Check for attribute conflicts detected during role composition
                if let Some((attr_name, role_a, role_b)) = role.attribute_conflicts.first() {
                    return Err(RuntimeError::new(format!(
                        "Attribute '$!{}' conflicts in role '{}' composition: declared in both '{}' and '{}'",
                        attr_name,
                        class_name.resolve(),
                        role_a,
                        role_b
                    )));
                }
                let mut named_args: HashMap<String, Value> = HashMap::new();
                let mut positional_args: Vec<Value> = Vec::new();
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        named_args.insert(key.clone(), *value.clone());
                    } else {
                        positional_args.push(arg.clone());
                    }
                }

                // Collect attributes from this role and all composed parent roles
                let mut all_attributes = role.attributes.clone();
                if let Some(parent_names) = self
                    .registry()
                    .role_parents
                    .get(&class_name.resolve())
                    .cloned()
                {
                    let mut role_stack: Vec<String> = parent_names;
                    let mut visited = vec![class_name.resolve()];
                    while let Some(parent_role_name) = role_stack.pop() {
                        if !visited.contains(&parent_role_name) {
                            visited.push(parent_role_name.clone());
                            if let Some(parent_role) =
                                self.registry().roles.get(&parent_role_name).cloned()
                            {
                                for attr in &parent_role.attributes {
                                    if !all_attributes.iter().any(|a| a.0 == attr.0) {
                                        all_attributes.push(attr.clone());
                                    }
                                }
                            }
                            if let Some(grandparents) =
                                self.registry().role_parents.get(&parent_role_name).cloned()
                            {
                                for gp_name in &grandparents {
                                    if !visited.contains(gp_name) {
                                        role_stack.push(gp_name.clone());
                                    }
                                }
                            }
                        }
                    }
                }

                let mut mixins = HashMap::new();
                mixins.insert(format!("__mutsu_role__{}", class_name), Value::Bool(true));
                for (idx, (attr_name, _is_public, default_expr, _, _, _, _)) in
                    all_attributes.iter().enumerate()
                {
                    let value = if let Some(v) = named_args.get(attr_name) {
                        v.clone()
                    } else if let Some(v) = positional_args.get(idx) {
                        v.clone()
                    } else if let Some(expr) = default_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                    } else {
                        Value::Nil
                    };
                    mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
                }
                // Embed language revision from the matching candidate
                // (no-params for bare role punning) so ^language-revision
                // returns the correct value for this role's origin module.
                let cn_str = class_name.resolve();
                let bare_lang_ver = self
                    .registry()
                    .role_candidates
                    .get(&cn_str)
                    .and_then(|candidates| {
                        candidates
                            .iter()
                            .find(|c| c.type_params.is_empty())
                            .map(|c| c.language_version.clone())
                    })
                    .or_else(|| {
                        self.type_metadata
                            .get(&cn_str)
                            .and_then(|m| m.get("language-revision"))
                            .map(|v| format!("6.{}", v.to_string_value()))
                    });
                if let Some(ver) = bare_lang_ver {
                    let revision: String = if let Some(letter) = ver.strip_prefix("6.") {
                        letter.chars().next().unwrap_or('c').to_string()
                    } else {
                        "c".to_string()
                    };
                    mixins.insert(
                        "__mutsu_language_revision".to_string(),
                        Value::str(revision),
                    );
                }
                return Ok(Value::mixin(
                    Value::make_instance(*class_name, HashMap::new()),
                    mixins,
                ));
            }
            // CUnion repr classes use byte-overlay construction
            if self.registry().cunion_classes.contains(&cn_resolved) {
                return self.construct_cunion_instance(&cn_resolved, &args);
            }
            // Auto-pun role to class if needed (e.g., role COERCE calling self.new)
            if !self.registry().classes.contains_key(&cn_resolved)
                && self.registry().roles.contains_key(&cn_resolved)
            {
                self.ensure_role_punned_to_class(&cn_resolved);
            }
            if self.registry().classes.contains_key(&cn_resolved)
                || type_args
                    .as_ref()
                    .is_some_and(|_| self.registry().classes.contains_key(base_class_name))
            {
                let class_key = if self.registry().classes.contains_key(&cn_resolved) {
                    cn_resolved.as_str()
                } else {
                    base_class_name
                };
                // Check if this class is a Proxy subclass
                {
                    let mro = self.class_mro(class_key);
                    if mro.iter().any(|c| c == "Proxy") {
                        return self.construct_proxy_subclass(class_key, &args);
                    }
                }
                // Semaphore takes a positional permits argument; build the
                // instance directly using the semaphore registry.
                if class_key == "Semaphore" {
                    let permits = args
                        .first()
                        .map(|v| match v {
                            Value::Int(i) => *i,
                            Value::Num(f) => *f as i64,
                            other => other.to_string_value().parse::<i64>().unwrap_or(1),
                        })
                        .unwrap_or(1)
                        .max(0);
                    let mut attrs = HashMap::new();
                    attrs.insert("permits".to_string(), Value::Int(permits));
                    attrs.insert(
                        "semaphore-id".to_string(),
                        Value::Int(super::native_methods::next_semaphore_id(permits) as i64),
                    );
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                // An explicit `proto method new` owns dispatch. When there are
                // no multi candidates it is registered only as a proto (so
                // `has_user_method` is false), yet `.new` must still run the
                // proto body: `proto method new() { 42 }` returns 42, while a
                // pure forwarder `proto method new($) {*}` with no candidates
                // surfaces X::Multi::NoMatch. Route through the proto runner
                // instead of falling back to the default constructor.
                if !self.has_user_method(class_key, "new")
                    && let Some((owner, proto)) =
                        self.lookup_proto_method(&class_name.resolve(), "new")
                {
                    // A pure `{*}` forwarder proto with no multi candidates can
                    // never dispatch, so running it would recurse forever via
                    // `__PROTO_DISPATCH__` re-entering `.new`. Short-circuit to
                    // X::Multi::NoMatch. A proto with a real body (no `{*}`)
                    // runs normally and returns its value.
                    if proto
                        .body
                        .iter()
                        .any(|s| matches!(s, Stmt::Expr(Expr::Whatever)))
                    {
                        return Err(super::methods_signature_errors::make_multi_no_match_error(
                            "new",
                        ));
                    }
                    let cn = class_name.resolve();
                    return self.run_proto_method(
                        target.clone(),
                        &cn,
                        &owner,
                        "new",
                        args.clone(),
                        proto,
                    );
                }
                // Check for user-defined .new method first
                if self.has_user_method(class_key, "new") {
                    let empty_attrs = HashMap::new();
                    match self.run_instance_method(
                        &class_name.resolve(),
                        empty_attrs,
                        "new",
                        args.clone(),
                        None,
                    ) {
                        Ok((result, _updated)) => return Ok(result),
                        Err(e) => {
                            // If multi dispatch failed (no matching candidate),
                            // fall through to the built-in Mu.new default constructor.
                            // This matches Raku's behavior where Mu.new(*%attrinit) is
                            // always available as a fallback multi candidate.
                            if !e.is_multi_no_match() {
                                return Err(e);
                            }
                            // An explicit `proto method new` OVERRIDES the inherited
                            // Mu.new proto, so it owns dispatch entirely: a no-match
                            // must surface as X::Multi::NoMatch rather than falling
                            // back to the default constructor (which would wrongly
                            // give X::Constructor::Positional).
                            let cn = class_name.resolve();
                            if self.lookup_proto_method(&cn, "new").is_some() {
                                if e.exception.as_deref().is_some_and(|ex| {
                                    matches!(ex, Value::Instance { class_name, .. }
                                        if class_name.resolve() == "X::Multi::NoMatch")
                                }) {
                                    return Err(e);
                                }
                                return Err(
                                    super::methods_signature_errors::make_multi_no_match_error(
                                        "new",
                                    ),
                                );
                            }
                            // Mu.new only accepts named arguments. If the call
                            // had positional args and no multi candidate matched,
                            // die like Raku does.
                            let has_positional = args
                                .iter()
                                .any(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)));
                            if has_positional {
                                // Check if this class composes Baggy/Setty role;
                                // if so, redirect to Bag-like construction
                                let cn = class_name.resolve();
                                if self.class_does_baggy_or_setty(&cn) {
                                    return self.construct_baggy_instance(&cn, &args);
                                }
                                return Err(constructor_positional_error(&class_name.resolve()));
                            }
                            // Fall through to default constructor below
                        }
                    }
                }
                let mut attrs = HashMap::new();
                let mut positional_ctor_args: Vec<Value> = Vec::new();
                let saved_default_env = self.env.clone();
                let role_bindings = {
                    let registry = self.registry();
                    registry
                        .class_role_param_bindings
                        .get(class_key)
                        .or_else(|| {
                            registry
                                .class_role_param_bindings
                                .get(&class_name.resolve())
                        })
                        .cloned()
                };
                if let Some(role_bindings) = role_bindings {
                    for (name, value) in &role_bindings {
                        self.env.insert(name.clone(), value.clone());
                    }
                }
                let class_attrs_info = self.collect_class_attributes(class_key);
                // Check required attributes BEFORE evaluating defaults
                // (required attributes are checked before defaults run)
                let provided_attr_names: std::collections::HashSet<String> = args
                    .iter()
                    .filter_map(|v| match v {
                        Value::Pair(k, _) => Some(k.clone()),
                        _ => None,
                    })
                    .collect();
                for (attr_name, _is_public, _default, _is_rw, is_required, _sigil, _) in
                    &class_attrs_info
                {
                    if let Some(reason) = is_required {
                        let has_build = self.class_has_method(class_key, "BUILD");
                        // If class has BUILD, BUILD handles attribute setting,
                        // so we skip required check here (BUILD may set defaults)
                        if !has_build && !provided_attr_names.contains(attr_name.as_str()) {
                            let attr_full_name = format!("$!{}", attr_name);
                            return Err(RuntimeError::attribute_required(
                                &attr_full_name,
                                reason.as_deref(),
                            ));
                        }
                    }
                }
                // Build a sigil map for later coercion
                let sigil_map: HashMap<String, char> = class_attrs_info
                    .iter()
                    .map(|(name, _, _, _, _, sigil, _)| (name.clone(), *sigil))
                    .collect();
                // Attribute type constraints (MRO-wide), used to coerce a provided
                // value for a coercion-typed attribute (`has Int() $.x`).
                let attr_type_constraints = self.collect_attribute_type_constraints(class_key);
                // First, collect constructor args into attrs
                self.env = saved_default_env.clone();
                let class_mro = self.class_mro(class_key);
                // When BUILD is defined, it controls attribute initialization,
                // so we skip automatic named-arg-to-attribute mapping.
                let any_build = class_mro.iter().any(|cn| {
                    cn != "Any"
                        && cn != "Mu"
                        && self
                            .registry()
                            .classes
                            .get(cn)
                            .and_then(|def| def.methods.get("BUILD"))
                            .is_some()
                });
                for val in &args {
                    match val {
                        Value::Pair(k, v) => {
                            if !any_build && self.is_attribute_buildable(class_key, k) {
                                let sigil = sigil_map.get(k).copied().unwrap_or('$');
                                let mut value = *v.clone();
                                // A coercion-typed attribute (`has Int() $.x`)
                                // coerces its provided value through the target
                                // type (built-in coercion or a user COERCE method).
                                if sigil == '$'
                                    && let Some(tc) = attr_type_constraints.get(k)
                                    && crate::runtime::types::is_coercion_constraint(tc)
                                {
                                    value = self.coerce_value_for_constraint(tc, value);
                                }
                                let coerced = Self::coerce_attr_value_by_sigil(value, sigil);
                                // An `is Type` container attribute (`has @.a is
                                // Buf`) coerces its provided value to the declared
                                // container type (Buf, BagHash, Array[T], ...).
                                let coerced = if matches!(sigil, '@' | '%')
                                    && let Some(type_name) =
                                        self.attribute_is_type_in_mro(class_key, k)
                                {
                                    self.coerce_value_to_is_type(&type_name, sigil, coerced)?
                                } else {
                                    coerced
                                };
                                attrs.insert(k.clone(), coerced);
                            }
                            // When BUILD exists, named args are passed to BUILD
                            // which controls attribute initialization directly
                        }
                        Value::Instance {
                            class_name: src_class,
                            attributes: src_attrs,
                            ..
                        } if class_mro.iter().any(|name| name == &src_class.resolve()) => {
                            for (attr, value) in src_attrs.as_map().iter() {
                                attrs.insert(attr.clone(), value.clone());
                            }
                        }
                        _ => {
                            positional_ctor_args.push(val.clone());
                        }
                    }
                }
                // Mu.new only accepts named arguments. If positional args
                // were passed and this is not a subclass that accepts them, reject.
                if !positional_ctor_args.is_empty() {
                    // Check if this class composes Baggy/Setty role;
                    // if so, redirect to Bag-like construction
                    let cn = class_name.resolve();
                    if self.class_does_baggy_or_setty(&cn) {
                        return self.construct_baggy_instance(&cn, &args);
                    }
                    let accepts_positional = class_mro
                        .iter()
                        .any(|n| n == "Array" || n == "Int" || n == "Num" || n == "Hash");
                    if !accepts_positional {
                        return Err(constructor_positional_error(&class_name.resolve()));
                    }
                }
                // A `|@x` slip (or a `Seq`) passed to an `@`-sigiled attribute
                // (`:backends(|@x)`) arrives as a `Slip`/`Seq`. Assigning a list to a
                // `@` container flattens it (just like `my @a = |@x` yields an
                // `Array`, not a `Slip`), so materialize it into a plain mutable
                // `Array` here. Without this the attribute keeps a `Slip` whose
                // `.^name` is `Slip` and whose re-iteration differs from raku's `Array`.
                for (attr_name, _is_public, _default, _is_rw, _is_required, sigil, _) in
                    &class_attrs_info
                {
                    if *sigil == '@'
                        && let Some(Value::Slip(items) | Value::Seq(items)) = attrs.get(attr_name)
                    {
                        let flattened = Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new((**items).clone())),
                            ArrayKind::Array,
                        );
                        attrs.insert(attr_name.clone(), flattened);
                    }
                }
                // For @-sigiled attributes with shaped array declarations,
                // convert user-provided values to shaped arrays preserving shape.
                for (attr_name, _is_public, default, _is_rw, _is_required, sigil, _) in
                    &class_attrs_info
                {
                    if *sigil == '@'
                        && let Some(dims) = Self::extract_shape_from_default(default.as_ref())
                        && let Some(val) = attrs.get(attr_name)
                        && !matches!(val, Value::Array(_, ArrayKind::Shaped))
                    {
                        let items = match val {
                            Value::Array(items, _) => (**items).clone(),
                            _ => crate::value::ArrayData::new(vec![val.clone()]),
                        };
                        let shaped = Value::Array(std::sync::Arc::new(items), ArrayKind::Shaped);
                        crate::runtime::utils::mark_shaped_array(&shaped, Some(&dims));
                        attrs.insert(attr_name.clone(), shaped);
                    }
                }
                self.enforce_attribute_where_constraints(class_key, &class_attrs_info, &attrs)?;
                self.enforce_attribute_smiley_constraints(class_key, &attrs)?;
                let int_ctor_val =
                    if matches!(positional_ctor_args.first(), Some(Value::Package(_))) {
                        return Err(RuntimeError::new("Cannot convert type object to Int"));
                    } else {
                        positional_ctor_args
                            .first()
                            .map_or(0, crate::runtime::to_int)
                    };
                if class_mro.iter().any(|name| name == "Array")
                    && !attrs.contains_key("__array_items")
                    && !positional_ctor_args.is_empty()
                {
                    attrs.insert(
                        "__array_items".to_string(),
                        Value::array(positional_ctor_args),
                    );
                }
                if class_mro.iter().any(|name| name == "Int")
                    && !attrs.contains_key("__mutsu_int_value")
                {
                    attrs.insert("__mutsu_int_value".to_string(), Value::Int(int_ctor_val));
                }
                // Then evaluate defaults for attributes not provided by args,
                // binding `self` so default expressions like `self.x` work.
                // Restore role parameter bindings so that default expressions
                // referencing role type parameters (e.g., `has $.x = $a`) work.
                // Also add class-qualified versions (e.g., `AP_2::a`) so that
                // bindings resolve correctly when current_package is the class.
                let role_bindings = {
                    let registry = self.registry();
                    registry
                        .class_role_param_bindings
                        .get(class_key)
                        .or_else(|| {
                            registry
                                .class_role_param_bindings
                                .get(&class_name.resolve())
                        })
                        .cloned()
                };
                if let Some(role_bindings) = role_bindings {
                    for (name, value) in &role_bindings {
                        self.env.insert(name.clone(), value.clone());
                        self.env
                            .insert(format!("{}::{}", class_key, name), value.clone());
                    }
                }
                for (attr_name, _is_public, default, _is_rw, _is_required, sigil, _) in
                    class_attrs_info.clone()
                {
                    if attrs.contains_key(&attr_name) {
                        continue;
                    }
                    // Clone the override out and drop the registry guard before
                    // call_sub_value re-enters user code (RwLock is not reentrant).
                    let build_override = self
                        .registry()
                        .attribute_build_overrides
                        .get(&(class_key.to_string(), attr_name.clone()))
                        .cloned();
                    let val = if let Some(build_override) = build_override {
                        let val = self.call_sub_value(build_override, Vec::new(), false)?;
                        Self::coerce_attr_value_by_sigil(val, sigil)
                    } else if let Some(expr) = default {
                        // Fast path: simple literal defaults (e.g. from native types
                        // like `has uint32 $.a` which generate `default: Int(0)`)
                        // don't need env manipulation or self-binding.
                        if let Expr::Literal(ref lit_val) = expr {
                            Self::coerce_attr_value_by_sigil(lit_val.clone(), sigil)
                        } else {
                            let temp_self = Value::make_instance(*class_name, attrs.clone());
                            let old_self = self.env.get("self").cloned();
                            self.env.insert("self".to_string(), temp_self);
                            // `::?CLASS` in a default (e.g. `has $.Version =
                            // ::?CLASS.^ver` composed from a role) resolves through
                            // `?CLASS`; bind it to the class being built.
                            let old_class = self.env.get("?CLASS").cloned();
                            self.env
                                .insert("?CLASS".to_string(), Value::Package(*class_name));
                            // Set !attr_name and .attr_name in env so that $!a / $.a
                            // references in default expressions resolve to already-
                            // initialized attributes (e.g. `has $.c = $!a + $!b`).
                            let mut saved_attr_env: Vec<(String, Option<Value>)> = Vec::new();
                            for (a_name, a_val) in &attrs {
                                let bang = format!("!{}", a_name);
                                let dot = format!(".{}", a_name);
                                saved_attr_env.push((bang.clone(), self.env.get(&bang).cloned()));
                                saved_attr_env.push((dot.clone(), self.env.get(&dot).cloned()));
                                self.env.insert(bang, a_val.clone());
                                self.env.insert(dot, a_val.clone());
                            }
                            // Temporarily switch to the class package so that
                            // class-scoped subs (e.g. `sub inner`) are found
                            // when evaluating attribute default expressions.
                            let saved_package = self.current_package();
                            self.set_current_package(class_key.to_string());
                            let result = self.eval_block_value(&[Stmt::Expr(expr)]);
                            self.set_current_package(saved_package);
                            // Restore previous env state for attribute variables
                            for (key, old_val) in saved_attr_env {
                                if let Some(v) = old_val {
                                    self.env.insert(key, v);
                                } else {
                                    self.env.remove(&key);
                                }
                            }
                            if let Some(old) = old_self {
                                self.env.insert("self".to_string(), old);
                            } else {
                                self.env.remove("self");
                            }
                            if let Some(old) = old_class {
                                self.env.insert("?CLASS".to_string(), old);
                            } else {
                                self.env.remove("?CLASS");
                            }
                            let val = result?;
                            Self::coerce_attr_value_by_sigil(val, sigil)
                        }
                    } else {
                        match sigil {
                            '@' => {
                                // Check for `is Type` trait (e.g. `has @.a is Buf`)
                                let is_type = self
                                    .registry()
                                    .class_attribute_is_types
                                    .get(&(class_key.to_string(), attr_name.clone()))
                                    .cloned();
                                if let Some(type_name) = is_type {
                                    // For a parameterized container type (`is Array[Rat]`),
                                    // build the empty array directly with type metadata so
                                    // `.WHAT` / `~~ Array[Rat]` see the declared element type
                                    // (a `Package` built from the string name loses its
                                    // type parameter when `.new` is dispatched).
                                    if let Some(inner) = type_name
                                        .strip_prefix("Array[")
                                        .or_else(|| type_name.strip_prefix("array["))
                                        .and_then(|s| s.strip_suffix(']'))
                                    {
                                        let mut arr = Value::real_array(Vec::new());
                                        arr = self.tag_container_metadata(
                                            arr,
                                            super::ContainerTypeInfo {
                                                value_type: inner.trim().to_string(),
                                                key_type: None,
                                                declared_type: Some(type_name.clone()),
                                            },
                                        );
                                        arr
                                    } else {
                                        let type_obj = Value::Package(
                                            crate::symbol::Symbol::intern(&type_name),
                                        );
                                        match self.call_method_with_values(type_obj, "new", vec![])
                                        {
                                            Ok(v) => v,
                                            Err(_) => Value::real_array(Vec::new()),
                                        }
                                    }
                                } else {
                                    let mut arr = Value::real_array(Vec::new());
                                    // Register element type constraint for typed array attributes
                                    let tc = self
                                        .registry()
                                        .classes
                                        .get(class_key)
                                        .and_then(|cd| cd.attribute_types.get(&attr_name))
                                        .cloned();
                                    if let Some(tc) = tc {
                                        arr = self.tag_container_metadata(
                                            arr,
                                            super::ContainerTypeInfo {
                                                value_type: tc,
                                                key_type: None,
                                                declared_type: None,
                                            },
                                        );
                                    }
                                    arr
                                }
                            }
                            '%' => {
                                // Check for `is Type` trait (e.g. `has %.h is BagHash`)
                                let is_type = self
                                    .registry()
                                    .class_attribute_is_types
                                    .get(&(class_key.to_string(), attr_name.clone()))
                                    .cloned();
                                if let Some(type_name) = is_type {
                                    let type_obj =
                                        Value::Package(crate::symbol::Symbol::intern(&type_name));
                                    match self.call_method_with_values(type_obj, "new", vec![]) {
                                        Ok(v) => v,
                                        Err(_) => Value::hash(HashMap::new()),
                                    }
                                } else {
                                    let h = Value::hash(HashMap::new());
                                    // Register value type constraint for typed hash attributes
                                    let tc = self
                                        .registry()
                                        .classes
                                        .get(class_key)
                                        .and_then(|cd| cd.attribute_types.get(&attr_name))
                                        .cloned();
                                    if let Some(tc) = tc {
                                        self.tag_container_metadata(
                                            h,
                                            super::ContainerTypeInfo {
                                                value_type: tc,
                                                key_type: None,
                                                declared_type: None,
                                            },
                                        )
                                    } else {
                                        h
                                    }
                                }
                            }
                            _ => Value::Nil,
                        }
                    };
                    // A coercion-typed attribute (`has Int() $.x = "42"`) coerces
                    // its evaluated default through the target type, just like a
                    // provided value. (The bare type-object default for an
                    // uninitialized coercion attribute coerces to itself.)
                    let val = if sigil == '$'
                        && let Some(tc) = attr_type_constraints.get(&attr_name)
                        && crate::runtime::types::is_coercion_constraint(tc)
                    {
                        self.coerce_value_for_constraint(tc, val)
                    } else {
                        val
                    };
                    attrs.insert(attr_name, val);
                }
                // Embed `is default(...)` element defaults into `@`/`%` containers
                // (evaluating any role-deferred expression while type params are
                // still bound in `self.env`).
                self.apply_container_attribute_defaults(class_key, &mut attrs);
                // Add alias metadata for `has $x` (no twigil) attributes
                self.add_alias_attribute_metadata(class_key, &mut attrs);
                self.enforce_attribute_where_constraints(class_key, &class_attrs_info, &attrs)?;
                self.enforce_attribute_smiley_constraints(class_key, &attrs)?;
                // Restore env after default evaluation, but preserve side effects
                // on variables that already existed in the caller environment.
                let mut restored_env = saved_default_env.clone();
                for (key, value) in self.env.iter() {
                    if restored_env.contains_key_sym(*key) {
                        restored_env.insert_sym(*key, value.clone());
                    }
                }
                self.env = restored_env;
                // Walk MRO in reverse (base-first) and call BUILD/TWEAK
                // submethods defined directly on each class. Submethods are
                // NOT inherited, so each class's own BUILD/TWEAK is called
                // independently with the construction args.
                // Under v6.e.PREVIEW+, role submethods are also called
                // (roles' BUILD/TWEAK before the class's own).
                let mro = self.class_mro(class_key);
                // Determine the class's language revision for submethod dispatch rules.
                let class_lang_rev = self
                    .type_metadata
                    .get(&class_name.resolve())
                    .and_then(|m| m.get("language-revision"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| {
                        let version = crate::parser::current_language_version();
                        if let Some(rest) = version.strip_prefix("6.") {
                            rest.chars().next().unwrap_or('c').to_string()
                        } else {
                            "c".to_string()
                        }
                    });
                let class_is_6e = class_lang_rev != "c";
                for mro_class in mro.iter().rev() {
                    if mro_class == "Any" || mro_class == "Mu" {
                        continue;
                    }
                    // Skip role entries in MRO — they are handled separately below
                    if self.registry().roles.contains_key(mro_class)
                        && !self.registry().classes.contains_key(mro_class)
                    {
                        continue;
                    }
                    // Check if the class has its own BUILD (not from a role)
                    let class_has_own_build = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("BUILD"))
                        .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                        .unwrap_or(false);
                    // Call BUILD submethods from composed roles
                    let role_order = self.ordered_role_submethods_for_class(mro_class, "BUILD");
                    for (role_name, method_def) in role_order {
                        let role_base = role_name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(&role_name);
                        let role_lang_rev = self
                            .type_metadata
                            .get(role_base)
                            .and_then(|m| m.get("language-revision"))
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "c".to_string());
                        // In 6.c class with own BUILD: skip 6.c role submethods
                        if !class_is_6e && class_has_own_build && role_lang_rev == "c" {
                            continue;
                        }
                        let (_v, updated) = self.run_resolved_method_compiled_or_treewalk(
                            &class_name.resolve(),
                            &role_name,
                            "BUILD",
                            method_def,
                            attrs.clone(),
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        )?;
                        attrs = updated;
                    }
                    // Call the class's BUILD if it has one that wasn't already handled
                    // by ordered_role_submethods_for_class. Role submethods (is_my=true,
                    // role_origin=Some) were already called above.
                    let has_non_submethod_build = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("BUILD"))
                        .map(|overloads| {
                            overloads
                                .iter()
                                .any(|md| md.role_origin.is_none() || !md.is_my)
                        })
                        .unwrap_or(false);
                    if has_non_submethod_build {
                        match self.run_instance_method(
                            mro_class,
                            attrs.clone(),
                            "BUILD",
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        ) {
                            Ok((_v, updated)) => {
                                attrs = updated;
                            }
                            Err(err) if err.is_fail() => {
                                // fail in BUILD: return a Failure wrapping the exception
                                let ex = if let Some(exception) = err.exception {
                                    *exception
                                } else {
                                    let mut ex_attrs = HashMap::new();
                                    ex_attrs.insert("message".to_string(), Value::str(err.message));
                                    Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs)
                                };
                                let mut failure_attrs = HashMap::new();
                                failure_attrs.insert("exception".to_string(), ex);
                                return Ok(Value::make_instance(
                                    Symbol::intern("Failure"),
                                    failure_attrs,
                                ));
                            }
                            Err(err) => return Err(err),
                        }
                    }
                }
                // Check required attributes after all BUILDs have run
                let any_build = mro.iter().any(|cn| {
                    cn != "Any"
                        && cn != "Mu"
                        && self
                            .registry()
                            .classes
                            .get(cn)
                            .and_then(|def| def.methods.get("BUILD"))
                            .is_some()
                });
                // Also check role BUILD submethods for required attribute enforcement
                let any_role_build = mro.iter().any(|cn| {
                    cn != "Any"
                        && cn != "Mu"
                        && !self
                            .ordered_role_submethods_for_class(cn, "BUILD")
                            .is_empty()
                });
                if any_build || any_role_build {
                    for (attr_name, _is_public, _default, _is_rw, is_required, _sigil, _) in
                        &class_attrs_info
                    {
                        if let Some(reason) = is_required {
                            let attr_val = attrs.get(attr_name.as_str());
                            let is_set = !matches!(attr_val, Some(Value::Nil) | None);
                            if !is_set {
                                let attr_full_name = format!("$!{}", attr_name);
                                return Err(RuntimeError::attribute_required(
                                    &attr_full_name,
                                    reason.as_deref(),
                                ));
                            }
                        }
                    }
                    self.enforce_attribute_where_constraints(class_key, &class_attrs_info, &attrs)?;
                    self.enforce_attribute_smiley_constraints(class_key, &attrs)?;
                }
                // Walk MRO in reverse for TWEAK as well
                for mro_class in mro.iter().rev() {
                    if mro_class == "Any" || mro_class == "Mu" {
                        continue;
                    }
                    // Skip role entries in MRO
                    if self.registry().roles.contains_key(mro_class)
                        && !self.registry().classes.contains_key(mro_class)
                    {
                        continue;
                    }
                    // Check if the class has its own TWEAK (not from a role)
                    let class_has_own_tweak = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("TWEAK"))
                        .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                        .unwrap_or(false);
                    // Call TWEAK submethods from composed roles (same rules as BUILD)
                    let role_order = self.ordered_role_submethods_for_class(mro_class, "TWEAK");
                    for (role_name, method_def) in role_order {
                        let role_base = role_name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(&role_name);
                        let role_lang_rev = self
                            .type_metadata
                            .get(role_base)
                            .and_then(|m| m.get("language-revision"))
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "c".to_string());
                        // In 6.c class with own TWEAK: skip 6.c role submethods
                        if !class_is_6e && class_has_own_tweak && role_lang_rev == "c" {
                            continue;
                        }
                        let (_v, updated) = self.run_resolved_method_compiled_or_treewalk(
                            &class_name.resolve(),
                            &role_name,
                            "TWEAK",
                            method_def,
                            attrs.clone(),
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        )?;
                        attrs = updated;
                        self.enforce_attribute_where_constraints(
                            class_key,
                            &class_attrs_info,
                            &attrs,
                        )?;
                    }
                    // Only call the class's own TWEAK (not role-composed ones).
                    let has_own_tweak = self
                        .registry()
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("TWEAK"))
                        .map(|overloads| {
                            overloads
                                .iter()
                                .any(|md| md.role_origin.is_none() || !md.is_my)
                        })
                        .unwrap_or(false);
                    if has_own_tweak {
                        let (_v, updated) = self.run_instance_method(
                            mro_class,
                            attrs.clone(),
                            "TWEAK",
                            args.clone(),
                            Some(Value::make_instance(*class_name, attrs.clone())),
                        )?;
                        attrs = updated;
                        self.enforce_attribute_where_constraints(
                            class_key,
                            &class_attrs_info,
                            &attrs,
                        )?;
                    }
                }
                // Initialize per-class private attributes: when a parent and child
                // both declare an attribute with the same name, each class gets its
                // own copy stored under a qualified key ("ClassName\0attrName").
                // Methods access $!attr via the qualified key for their owner class.
                {
                    let per_class_attrs = self.collect_per_class_attrs(class_key);
                    // Named-arg keys explicitly passed to the constructor. In Raku
                    // each class's BUILD binds its own same-named attribute from the
                    // named args (a provided named arg wins over the class's own
                    // default), so EVERY declaring class's private copy gets that
                    // value — not just the most-derived one. The most-derived value
                    // already lives in the bare key, so reuse it.
                    let provided_keys: std::collections::HashSet<String> = args
                        .iter()
                        .filter_map(|a| match a {
                            Value::Pair(k, _) => Some(k.clone()),
                            _ => None,
                        })
                        .collect();
                    for (
                        declaring_class,
                        (attr_name, _is_public, default, _is_rw, _is_required, sigil, _),
                    ) in per_class_attrs
                    {
                        let qualified_key = format!("{}\0{}", declaring_class, attr_name);
                        if attrs.contains_key(&qualified_key) {
                            continue;
                        }
                        // Constructor named arg provided → initialize this class's
                        // copy from it (per-class BUILD), regardless of which class
                        // in the hierarchy declared it.
                        if provided_keys.contains(&attr_name)
                            && let Some(val) = attrs.get(&attr_name)
                        {
                            attrs.insert(qualified_key, val.clone());
                            continue;
                        }
                        // Use the unqualified value if it was provided via constructor args
                        // and this is the most-derived class declaring this attribute
                        if declaring_class == class_key
                            && let Some(val) = attrs.get(&attr_name)
                        {
                            attrs.insert(qualified_key, val.clone());
                            continue;
                        }
                        // Evaluate the default expression for this class's attribute
                        let val = if let Some(Expr::Literal(ref lit_val)) = default {
                            // Fast path: simple literal defaults
                            Self::coerce_attr_value_by_sigil(lit_val.clone(), sigil)
                        } else if let Some(expr) = default {
                            let temp_self = Value::make_instance(*class_name, attrs.clone());
                            let old_self = self.env.get("self").cloned();
                            self.env.insert("self".to_string(), temp_self);
                            let result = self.eval_block_value(&[Stmt::Expr(expr)]);
                            if let Some(old) = old_self {
                                self.env.insert("self".to_string(), old);
                            } else {
                                self.env.remove("self");
                            }
                            match result {
                                Ok(v) => Self::coerce_attr_value_by_sigil(v, sigil),
                                Err(_) => Value::Nil,
                            }
                        } else {
                            // Check for `is Type` trait on this attribute
                            let is_type_val = self
                                .registry()
                                .class_attribute_is_types
                                .get(&(declaring_class.clone(), attr_name.clone()))
                                .cloned();
                            if let Some(type_name) = is_type_val {
                                // `is Array[T]`: build the empty array with type
                                // metadata directly (a Package built from the string
                                // name loses its type parameter on `.new`).
                                if let Some(inner) = type_name
                                    .strip_prefix("Array[")
                                    .or_else(|| type_name.strip_prefix("array["))
                                    .and_then(|s| s.strip_suffix(']'))
                                {
                                    let mut arr = Value::real_array(Vec::new());
                                    arr = self.tag_container_metadata(
                                        arr,
                                        super::ContainerTypeInfo {
                                            value_type: inner.trim().to_string(),
                                            key_type: None,
                                            declared_type: Some(type_name.clone()),
                                        },
                                    );
                                    arr
                                } else {
                                    let type_obj =
                                        Value::Package(crate::symbol::Symbol::intern(&type_name));
                                    self.call_method_with_values(type_obj, "new", vec![])
                                        .unwrap_or_else(|_| match sigil {
                                            '@' => Value::real_array(Vec::new()),
                                            '%' => Value::hash(HashMap::new()),
                                            _ => Value::Nil,
                                        })
                                }
                            } else {
                                match sigil {
                                    '@' => Value::real_array(Vec::new()),
                                    '%' => Value::hash(HashMap::new()),
                                    _ => Value::Nil,
                                }
                            }
                        };
                        attrs.insert(qualified_key, val);
                    }
                }
                // If the class inherits from Array, add backing array storage
                if self.class_mro(class_key).iter().any(|n| n == "Array")
                    && !attrs.contains_key("__mutsu_array_storage")
                {
                    attrs.insert(
                        "__mutsu_array_storage".to_string(),
                        Value::real_array(Vec::new()),
                    );
                }
                // Tag typed `@`/`%` attributes (`has Int @.nums`) with
                // element-type metadata and type-check their elements (the
                // variable path `my Int @a` does the same). The element type
                // lives in `attribute_types`; `is Type` containers carry their
                // own metadata and are skipped. Done here against the final
                // `attrs` so the Arc-pointer-keyed metadata survives into the
                // instance (a clone shares the same backing Arc).
                for (attr_name, _, _, _, _, sigil, _) in &class_attrs_info {
                    if !matches!(sigil, '@' | '%') {
                        continue;
                    }
                    let Some(elem_type) = attr_type_constraints.get(attr_name).cloned() else {
                        continue;
                    };
                    // Only plain class element types (`has Int @.nums`). Native
                    // (`has int @.x` -> packed `array[int]`), coercion, and
                    // parametric elements keep their pre-existing construction
                    // (the uninit branch already tags native packed arrays).
                    if !Self::is_simple_native_ctor_constraint(&elem_type) {
                        continue;
                    }
                    if self
                        .registry()
                        .class_attribute_is_types
                        .contains_key(&(class_key.to_string(), attr_name.clone()))
                    {
                        continue;
                    }
                    if let Some(val) = attrs.get(attr_name).cloned() {
                        let tagged =
                            self.finalize_typed_container_attr(attr_name, *sigil, &elem_type, val)?;
                        attrs.insert(attr_name.clone(), tagged);
                    }
                }
                // Apply `has $.x does Role` attribute traits (shared with the
                // native default constructor).
                self.apply_attribute_does_role_mixins(class_key, &mut attrs);
                let mut instance = Value::make_instance(*class_name, attrs);
                if let Some(type_args) = type_args.as_ref() {
                    if self.class_mro(class_key).iter().any(|n| n == "Array") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        instance = self.tag_container_metadata(
                            instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(class_name.resolve()),
                            },
                        );
                    } else if self.class_mro(class_key).iter().any(|n| n == "Hash") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        let key_type = type_args.get(1).cloned();
                        instance = self.tag_container_metadata(
                            instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type,
                                declared_type: Some(class_name.resolve()),
                            },
                        );
                    }
                }
                return Ok(instance);
            }
        }
        // Fallback .new on basic types
        match target {
            Value::Package(name) if name == "CallFrame" => {
                // CallFrame.new(depth) — equivalent to callframe(depth)
                let depth = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i as usize),
                        Value::Num(f) => Some(*f as usize),
                        _ => None,
                    })
                    .unwrap_or(0);
                self.builtin_callframe(&args, depth)
            }
            Value::Package(name) => {
                // Built-in type objects: .new creates a default defined instance
                match name.resolve().as_str() {
                    // Shared with the VM's native fast path.
                    "Int" => Self::build_native_int_value(&args),
                    "Str" => Ok(Value::str(String::new())),
                    "Num" => Self::build_native_num_value(&args),
                    "Bool" => Ok(Value::Bool(false)),
                    "Attribute" => {
                        // Attribute.new(:name<...>, :type(Int), :package<Foo>)
                        let mut attrs = HashMap::new();
                        for arg in &args {
                            if let Value::Pair(key, value) = arg {
                                match key.as_str() {
                                    "name" => {
                                        let n = value.to_string_value();
                                        attrs.insert("name".to_string(), (**value).clone());
                                        attrs
                                            .insert("__mutsu_attr_name".to_string(), Value::str(n));
                                    }
                                    "type" => {
                                        attrs.insert("type".to_string(), (**value).clone());
                                    }
                                    "package" => {
                                        attrs.insert(
                                            "__mutsu_attr_owner".to_string(),
                                            (**value).clone(),
                                        );
                                    }
                                    other => {
                                        attrs.insert(other.to_string(), (**value).clone());
                                    }
                                }
                            }
                        }
                        Ok(Value::make_instance(Symbol::intern("Attribute"), attrs))
                    }
                    "Semaphore" => {
                        let permits = args
                            .first()
                            .map(|v| match v {
                                Value::Int(i) => *i,
                                Value::Num(f) => *f as i64,
                                other => other.to_string_value().parse::<i64>().unwrap_or(1),
                            })
                            .unwrap_or(1)
                            .max(0);
                        let mut attrs = HashMap::new();
                        attrs.insert("permits".to_string(), Value::Int(permits));
                        attrs.insert(
                            "semaphore-id".to_string(),
                            Value::Int(super::native_methods::next_semaphore_id(permits) as i64),
                        );
                        Ok(Value::make_instance(name, attrs))
                    }
                    _ => Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on {}",
                        name
                    ))),
                }
            }
            Value::Str(_) => Ok(Value::str(String::new())),
            Value::Int(_) => Ok(Value::Int(0)),
            Value::Num(_) => Ok(Value::Num(0.0)),
            Value::Bool(_) => Ok(Value::Bool(false)),
            Value::Nil => Ok(Value::Nil),
            _ => Err(RuntimeError::new(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): new",
            )),
        }
    }
}
