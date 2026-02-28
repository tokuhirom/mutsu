use super::*;

impl Interpreter {
    pub(super) fn dispatch_new(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Str(ref name) = target
            && self.enum_types.contains_key(name.as_str())
        {
            let msg = format!(
                "Enum '{}' is insufficiently type-like to be instantiated.  Did you mean 'class'?",
                name
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::Str(msg.clone()));
            let ex = Value::make_instance("X::Constructor::BadType".to_string(), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = &target
        {
            let mut selected_role = self.roles.get(base_name).cloned();
            let mut selected_param_names = self
                .role_type_params
                .get(base_name)
                .cloned()
                .unwrap_or_default();
            if let Some(candidates) = self.role_candidates.get(base_name).cloned() {
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
                    selected_role = Some(candidate.role_def.clone());
                }
            }
            if let Some(role) = selected_role {
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
                for (idx, (attr_name, _is_public, default_expr, _is_rw)) in
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
                return Ok(Value::Mixin(
                    Box::new(Value::make_instance(base_name.clone(), HashMap::new())),
                    mixins,
                ));
            }
        }

        if let Value::Package(class_name) = &target {
            let parametric = Self::parse_parametric_type_name(class_name);
            let (base_class_name, type_args) = if let Some((base, args)) = &parametric {
                (base.as_str(), Some(args.clone()))
            } else {
                (class_name.as_str(), None)
            };
            match base_class_name {
                "Array" | "List" | "Positional" | "array" => {
                    if let Some(dims) = self.shaped_dims_from_new_args(&args) {
                        // Check for :data argument to populate the shaped array
                        let data = args.iter().find_map(|arg| match arg {
                            Value::Pair(name, value) if name == "data" => {
                                Some(value.as_ref().clone())
                            }
                            _ => None,
                        });
                        let shaped = Self::make_shaped_array(&dims);
                        if let Some(data_val) = data {
                            // Populate the shaped array with data
                            let data_items = match data_val {
                                Value::Array(items, ..)
                                | Value::Seq(items)
                                | Value::Slip(items) => items.to_vec(),
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
                                return Ok(result);
                            }
                        }
                        return Ok(shaped);
                    }
                    let mut items = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Slip(vals) => items.extend(vals.iter().cloned()),
                            other => items.push(other.clone()),
                        }
                    }
                    // Type check for typed arrays (e.g. Array[Int].new(...))
                    // Skip for native types (int8, num32, etc.) which coerce rather than check
                    if let Some(ref ta) = type_args
                        && let Some(constraint) = ta.first()
                        && constraint.starts_with(char::is_uppercase)
                    {
                        for item in &items {
                            if !self.type_matches_value(constraint, item) {
                                let got_type = crate::value::what_type_name(item);
                                let got_repr = item.to_string_value();
                                let msg = format!(
                                    "Type check failed in assignment to ; expected {} but got {} ({})",
                                    constraint, got_type, got_repr,
                                );
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert("message".to_string(), Value::Str(msg.clone()));
                                attrs.insert(
                                    "operation".to_string(),
                                    Value::Str("assignment".to_string()),
                                );
                                attrs.insert("got".to_string(), item.clone());
                                attrs.insert(
                                    "expected".to_string(),
                                    Value::Package(constraint.clone()),
                                );
                                let ex = Value::make_instance(
                                    "X::TypeCheck::Assignment".to_string(),
                                    attrs,
                                );
                                let mut err = RuntimeError::new(msg);
                                err.exception = Some(Box::new(ex));
                                return Err(err);
                            }
                        }
                    }
                    let result = if matches!(base_class_name, "Array" | "array") {
                        Value::real_array(items)
                    } else {
                        Value::array(items)
                    };
                    // Register type metadata for typed arrays (e.g. Array[Int].new)
                    if let Some(ref ta) = type_args
                        && let Some(constraint) = ta.first()
                    {
                        let info = crate::runtime::ContainerTypeInfo {
                            value_type: constraint.clone(),
                            key_type: None,
                            declared_type: Some(class_name.clone()),
                        };
                        self.register_container_type_metadata(&result, info);
                    }
                    return Ok(result);
                }
                "Hash" | "Map" => {
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
                    // Register type metadata for typed hashes (e.g. Hash[Int].new)
                    if let Some(ref ta) = type_args {
                        let value_type = ta.first().cloned().unwrap_or_default();
                        let key_type = ta.get(1).cloned();
                        let info = crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type,
                            declared_type: Some(class_name.clone()),
                        };
                        self.register_container_type_metadata(&result, info);
                    }
                    return Ok(result);
                }
                "Uni" => {
                    let codepoints: Vec<Value> = args
                        .iter()
                        .map(|a| match a {
                            Value::Int(i) => Value::Int(*i),
                            Value::Num(f) => Value::Int(*f as i64),
                            other => {
                                Value::Int(other.to_string_value().parse::<i64>().unwrap_or(0))
                            }
                        })
                        .collect();
                    return Ok(Value::array(codepoints));
                }
                "Seq" => {
                    // Seq.new(iterator) — pull all items from the iterator
                    if let Some(iterator) = args.first() {
                        let mut items = Vec::new();
                        loop {
                            let val =
                                self.call_method_with_values(iterator.clone(), "pull-one", vec![])?;
                            if matches!(&val, Value::Str(s) if s == "IterationEnd") {
                                break;
                            }
                            items.push(val);
                        }
                        return Ok(Value::Seq(std::sync::Arc::new(items)));
                    }
                    return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
                }
                "Version" => {
                    let arg = args.first().cloned().unwrap_or(Value::Nil);
                    return Ok(Self::version_from_value(arg));
                }
                "Duration" => {
                    let secs = args.first().map(to_float_value).unwrap_or(Some(0.0));
                    return Ok(Value::Num(secs.unwrap_or(0.0)));
                }
                "Date" => {
                    let mut year: i64 = 1970;
                    let mut month: i64 = 1;
                    let mut day: i64 = 1;
                    let mut positional = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) => match key.as_str() {
                                "year" => year = to_int(value),
                                "month" => month = to_int(value),
                                "day" => day = to_int(value),
                                _ => {}
                            },
                            other => positional.push(other),
                        }
                    }
                    if let Some(v) = positional.first() {
                        year = to_int(v);
                    }
                    if let Some(v) = positional.get(1) {
                        month = to_int(v);
                    }
                    if let Some(v) = positional.get(2) {
                        day = to_int(v);
                    }
                    if !(1..=12).contains(&month) || !(1..=31).contains(&day) {
                        return Err(RuntimeError::new("Date.new: invalid month/day"));
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "days".to_string(),
                        Value::Int(Self::civil_to_epoch_days(year, month, day)),
                    );
                    return Ok(Value::make_instance("Date".to_string(), attrs));
                }
                "Promise" => {
                    return Ok(Value::Promise(SharedPromise::new()));
                }
                "Channel" => {
                    return Ok(Value::Channel(SharedChannel::new()));
                }
                "Stash" => {
                    // Stash is essentially a Hash but with type Stash
                    return Ok(Value::make_instance("Stash".to_string(), HashMap::new()));
                }
                "Supply" => return Ok(self.make_supply_instance()),
                "Supplier" => {
                    let mut attrs = HashMap::new();
                    attrs.insert("emitted".to_string(), Value::array(Vec::new()));
                    attrs.insert("done".to_string(), Value::Bool(false));
                    attrs.insert(
                        "supplier_id".to_string(),
                        Value::Int(super::native_methods::next_supplier_id() as i64),
                    );
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "ThreadPoolScheduler" | "CurrentThreadScheduler" | "Tap" | "Cancellation" => {
                    return Ok(Value::make_instance(class_name.clone(), HashMap::new()));
                }
                "Proxy" => {
                    let mut fetcher = Value::Nil;
                    let mut storer = Value::Nil;
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            match key.as_str() {
                                "FETCH" => fetcher = *value.clone(),
                                "STORE" => storer = *value.clone(),
                                _ => {}
                            }
                        }
                    }
                    return Ok(Value::Proxy {
                        fetcher: Box::new(fetcher),
                        storer: Box::new(storer),
                    });
                }
                "CompUnit::DependencySpecification" => {
                    // Extract :short-name from named args
                    let mut short_name: Option<String> = None;
                    for arg in &args {
                        if let Value::Pair(key, value) = arg
                            && key == "short-name"
                        {
                            if let Value::Str(s) = value.as_ref() {
                                short_name = Some(s.clone());
                            } else {
                                return Err(RuntimeError::new(
                                    "CompUnit::DependencySpecification.new: :short-name must be a Str",
                                ));
                            }
                        }
                    }
                    let short_name = short_name.ok_or_else(|| {
                        RuntimeError::new(
                            "CompUnit::DependencySpecification.new: :short-name is required",
                        )
                    })?;
                    return Ok(Value::CompUnitDepSpec { short_name });
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
                    attrs.insert("short-id".to_string(), Value::Str("file".to_string()));
                    let repo = Value::make_instance(class_name.clone(), attrs);
                    self.env.insert(cache_key, repo.clone());
                    return Ok(repo);
                }
                "Proc::Async" => {
                    let mut positional = Vec::new();
                    let mut w_flag = false;
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "w" => {
                                w_flag = value.truthy();
                            }
                            _ => positional.push(arg.clone()),
                        }
                    }
                    let stdout_id = super::native_methods::next_supply_id();
                    let stderr_id = super::native_methods::next_supply_id();
                    let mut stdout_supply_attrs = HashMap::new();
                    stdout_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stdout_id as i64));
                    let mut stderr_supply_attrs = HashMap::new();
                    stderr_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stderr_id as i64));

                    let mut attrs = HashMap::new();
                    attrs.insert("cmd".to_string(), Value::array(positional));
                    attrs.insert("started".to_string(), Value::Bool(false));
                    attrs.insert(
                        "stdout".to_string(),
                        Value::make_instance("Supply".to_string(), stdout_supply_attrs),
                    );
                    attrs.insert(
                        "stderr".to_string(),
                        Value::make_instance("Supply".to_string(), stderr_supply_attrs),
                    );
                    if w_flag {
                        attrs.insert("w".to_string(), Value::Bool(true));
                    }
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "IO::Path" => {
                    let mut path = String::new();
                    let mut cwd_attr: Option<String> = None;
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "CWD" => {
                                cwd_attr = Some(value.to_string_value());
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if path.is_empty() && class_name == "IO::Path" => {
                                path = attributes
                                    .get("path")
                                    .map(|v| v.to_string_value())
                                    .unwrap_or_default();
                                if cwd_attr.is_none() {
                                    cwd_attr = attributes.get("cwd").map(|v| v.to_string_value());
                                }
                            }
                            _ if path.is_empty() => {
                                path = arg.to_string_value();
                            }
                            _ => {}
                        }
                    }
                    if path.contains('\0') {
                        return Err(RuntimeError::new(
                            "X::IO::Null: Found null byte in pathname",
                        ));
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert("path".to_string(), Value::Str(path));
                    if let Some(cwd) = cwd_attr {
                        attrs.insert("cwd".to_string(), Value::Str(cwd));
                    }
                    return Ok(Value::make_instance("IO::Path".to_string(), attrs));
                }
                "utf8" | "utf16" => {
                    let elems: Vec<Value> = args
                        .iter()
                        .flat_map(|a| match a {
                            Value::Int(i) => vec![Value::Int(*i)],
                            Value::Array(items, ..) => items.to_vec(),
                            Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                            Value::RangeExcl(start, end) => {
                                (*start..*end).map(Value::Int).collect()
                            }
                            _ => vec![],
                        })
                        .collect();
                    let mut attrs = HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(elems));
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "Buf" | "buf8" | "Buf[uint8]" | "Blob" | "blob8" | "Blob[uint8]" | "buf16"
                | "buf32" | "buf64" | "blob16" | "blob32" | "blob64" => {
                    let byte_vals: Vec<Value> = args
                        .iter()
                        .flat_map(|a| match a {
                            Value::Int(i) => vec![Value::Int(*i)],
                            Value::Array(items, ..) => items.to_vec(),
                            Value::Seq(items) => items.to_vec(),
                            Value::Slip(items) => items.to_vec(),
                            Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                            Value::RangeExcl(start, end) => {
                                (*start..*end).map(Value::Int).collect()
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "Buf"
                                || class_name == "Blob"
                                || class_name.starts_with("Buf[")
                                || class_name.starts_with("Blob[")
                                || class_name.starts_with("buf")
                                || class_name.starts_with("blob") =>
                            {
                                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                                    items.to_vec()
                                } else {
                                    Vec::new()
                                }
                            }
                            _ => vec![],
                        })
                        .collect();
                    let is_blob = class_name.starts_with("Blob") || class_name.starts_with("blob");
                    let type_name = if is_blob { "Blob" } else { "Buf" }.to_string();
                    let mut attrs = HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    return Ok(Value::make_instance(type_name, attrs));
                }
                "Rat" => {
                    let a = match args.first() {
                        Some(v) => to_int(v),
                        None => 0,
                    };
                    let b = match args.get(1) {
                        Some(v) => to_int(v),
                        None => 1,
                    };
                    return Ok(make_rat(a, b));
                }
                "FatRat" => {
                    let a = match args.first() {
                        Some(v) => to_int(v),
                        None => 0,
                    };
                    let b = match args.get(1) {
                        Some(v) => to_int(v),
                        None => 1,
                    };
                    return Ok(Value::FatRat(a, b));
                }
                "Set" | "SetHash" => {
                    let mut elems = HashSet::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            elems.insert(item.to_string_value());
                        }
                    }
                    return Ok(Value::set(elems));
                }
                "Bag" | "BagHash" => {
                    let mut counts: HashMap<String, i64> = HashMap::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            *counts.entry(item.to_string_value()).or_insert(0) += 1;
                        }
                    }
                    return Ok(Value::bag(counts));
                }
                "Mix" | "MixHash" => {
                    let mut weights: HashMap<String, f64> = HashMap::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            match &item {
                                Value::Pair(k, v) => {
                                    let w = match v.as_ref() {
                                        Value::Int(i) => *i as f64,
                                        Value::Num(n) => *n,
                                        Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                        _ => 1.0,
                                    };
                                    *weights.entry(k.clone()).or_insert(0.0) += w;
                                }
                                _ => {
                                    *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                                }
                            }
                        }
                    }
                    return Ok(Value::mix(weights));
                }
                "Complex" => {
                    let re = match args.first() {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        _ => 0.0,
                    };
                    let im = match args.get(1) {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        _ => 0.0,
                    };
                    return Ok(Value::Complex(re, im));
                }
                "Backtrace" => {
                    let file = self
                        .env
                        .get("?FILE")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let mut frame_attrs = HashMap::new();
                    frame_attrs.insert("file".to_string(), Value::Str(file));
                    let frame = Value::make_instance("Backtrace::Frame".to_string(), frame_attrs);
                    return Ok(Value::array(vec![frame]));
                }
                "Lock" | "Lock::Async" => {
                    let mut attrs = HashMap::new();
                    let lock_id = super::native_methods::next_lock_id() as i64;
                    attrs.insert("lock-id".to_string(), Value::Int(lock_id));
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "Slip" => {
                    return Ok(Value::slip(args.clone()));
                }
                "Match" => {
                    // Match.new(:orig("..."), :from(N), :pos(N), :list(...), :hash(...))
                    let mut orig = String::new();
                    let mut from: i64 = 0;
                    let mut to: i64 = 0;
                    let mut list = Value::array(Vec::new());
                    let mut hash = Value::hash(HashMap::new());
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            match key.as_str() {
                                "orig" => orig = value.to_string_value(),
                                "from" => from = to_int(value),
                                "pos" | "to" => to = to_int(value),
                                "list" => list = *value.clone(),
                                "hash" => hash = *value.clone(),
                                _ => {}
                            }
                        }
                    }
                    // Compute matched string from orig[from..pos]
                    let matched: String = orig
                        .chars()
                        .skip(from as usize)
                        .take((to - from) as usize)
                        .collect();
                    let mut attrs = HashMap::new();
                    attrs.insert("str".to_string(), Value::Str(matched));
                    attrs.insert("from".to_string(), Value::Int(from));
                    attrs.insert("to".to_string(), Value::Int(to));
                    attrs.insert("orig".to_string(), Value::Str(orig));
                    // Convert list to positional captures
                    if let Value::Array(items, ..) = &list {
                        attrs.insert("list".to_string(), Value::array(items.to_vec()));
                    } else {
                        attrs.insert("list".to_string(), Value::array(Vec::new()));
                    }
                    // Convert hash (Map) to named captures
                    if let Value::Hash(map, ..) = &hash {
                        attrs.insert("named".to_string(), Value::hash(map.as_ref().clone()));
                    } else {
                        attrs.insert("named".to_string(), Value::hash(HashMap::new()));
                    }
                    return Ok(Value::make_instance("Match".to_string(), attrs));
                }
                // Types that cannot be instantiated with .new
                "HyperWhatever" | "Whatever" | "Junction" => {
                    return Err(RuntimeError::new(format!(
                        "X::Cannot::New: Cannot create new object of type {}",
                        class_name
                    )));
                }
                _ => {}
            }
            // Parametric package handling (e.g. Array[Int], Hash[Int,Str], A[Int]).
            if let Some(type_args) = type_args.as_ref() {
                if matches!(base_class_name, "Array" | "List" | "Positional" | "array") {
                    if let Some(dims) = self.shaped_dims_from_new_args(&args) {
                        let data = args.iter().find_map(|arg| match arg {
                            Value::Pair(name, value) if name == "data" => {
                                Some(value.as_ref().clone())
                            }
                            _ => None,
                        });
                        let shaped = Self::make_shaped_array(&dims);
                        let result = if let Some(data_val) = data {
                            let data_items = match data_val {
                                Value::Array(items, ..)
                                | Value::Seq(items)
                                | Value::Slip(items) => items.to_vec(),
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
                        self.register_container_type_metadata(
                            &result,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(class_name.clone()),
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
                    let result = if matches!(base_class_name, "Array" | "array") {
                        Value::real_array(items)
                    } else {
                        Value::array(items)
                    };
                    let value_type = type_args
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Any".to_string());
                    self.register_container_type_metadata(
                        &result,
                        crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type: None,
                            declared_type: Some(class_name.clone()),
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
                    self.register_container_type_metadata(
                        &result,
                        crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type,
                            declared_type: Some(class_name.clone()),
                        },
                    );
                    return Ok(result);
                }
            }
            if let Some(role) = self.roles.get(class_name).cloned() {
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
                mixins.insert(format!("__mutsu_role__{}", class_name), Value::Bool(true));
                for (idx, (attr_name, _is_public, default_expr, _is_rw)) in
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
                return Ok(Value::Mixin(
                    Box::new(Value::make_instance(class_name.clone(), HashMap::new())),
                    mixins,
                ));
            }
            // CUnion repr classes use byte-overlay construction
            if self.cunion_classes.contains(class_name) {
                return self.construct_cunion_instance(class_name, &args);
            }
            if self.classes.contains_key(class_name)
                || type_args
                    .as_ref()
                    .is_some_and(|_| self.classes.contains_key(base_class_name))
            {
                let class_key = if self.classes.contains_key(class_name) {
                    class_name.as_str()
                } else {
                    base_class_name
                };
                // Check for user-defined .new method first
                if self.has_user_method(class_key, "new") {
                    let empty_attrs = HashMap::new();
                    let (result, _updated) =
                        self.run_instance_method(class_name, empty_attrs, "new", args, None)?;
                    return Ok(result);
                }
                let mut attrs = HashMap::new();
                let mut positional_ctor_args: Vec<Value> = Vec::new();
                let saved_default_env = self.env.clone();
                if let Some(role_bindings) = self.class_role_param_bindings.get(class_key) {
                    for (name, value) in role_bindings {
                        self.env.insert(name.clone(), value.clone());
                    }
                } else if let Some(role_bindings) = self.class_role_param_bindings.get(class_name) {
                    for (name, value) in role_bindings {
                        self.env.insert(name.clone(), value.clone());
                    }
                }
                for (attr_name, _is_public, default, _is_rw) in
                    self.collect_class_attributes(class_key)
                {
                    let val = if let Some(expr) = default {
                        self.eval_block_value(&[Stmt::Expr(expr)])?
                    } else {
                        Value::Nil
                    };
                    attrs.insert(attr_name, val);
                }
                self.env = saved_default_env;
                let class_mro = self.class_mro(class_key);
                for val in &args {
                    match val {
                        Value::Pair(k, v) => {
                            attrs.insert(k.clone(), *v.clone());
                        }
                        Value::Instance {
                            class_name: src_class,
                            attributes: src_attrs,
                            ..
                        } if class_mro.iter().any(|name| name == src_class) => {
                            for (attr, value) in src_attrs.iter() {
                                if attrs.contains_key(attr) {
                                    attrs.insert(attr.clone(), value.clone());
                                }
                            }
                        }
                        _ => {
                            positional_ctor_args.push(val.clone());
                        }
                    }
                }
                if class_mro.iter().any(|name| name == "Array")
                    && !attrs.contains_key("__array_items")
                    && !positional_ctor_args.is_empty()
                {
                    attrs.insert(
                        "__array_items".to_string(),
                        Value::array(positional_ctor_args),
                    );
                }
                let class_def = self.classes.get(class_key);
                let has_direct_build = class_def.and_then(|def| def.methods.get("BUILD")).is_some();
                let has_direct_tweak = class_def.and_then(|def| def.methods.get("TWEAK")).is_some();
                if self.class_has_method(class_name, "BUILD") {
                    let build_args = if has_direct_build {
                        args.clone()
                    } else {
                        Vec::new()
                    };
                    let (_v, updated) = self.run_instance_method(
                        class_name,
                        attrs.clone(),
                        "BUILD",
                        build_args,
                        Some(Value::make_instance(class_name.clone(), attrs.clone())),
                    )?;
                    attrs = updated;
                }
                if self.class_has_method(class_name, "TWEAK") {
                    let tweak_args = if has_direct_tweak {
                        args.clone()
                    } else {
                        Vec::new()
                    };
                    let (_v, updated) = self.run_instance_method(
                        class_name,
                        attrs.clone(),
                        "TWEAK",
                        tweak_args,
                        Some(Value::make_instance(class_name.clone(), attrs.clone())),
                    )?;
                    attrs = updated;
                }
                let instance = Value::make_instance(class_name.clone(), attrs);
                if let Some(type_args) = type_args.as_ref() {
                    if self.class_mro(class_key).iter().any(|n| n == "Array") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        self.register_container_type_metadata(
                            &instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(class_name.clone()),
                            },
                        );
                    } else if self.class_mro(class_key).iter().any(|n| n == "Hash") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        let key_type = type_args.get(1).cloned();
                        self.register_container_type_metadata(
                            &instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type,
                                declared_type: Some(class_name.clone()),
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
            Value::Package(name) => Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on {}",
                name
            ))),
            Value::Str(_) => Ok(Value::Str(String::new())),
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
