use super::*;
use crate::symbol::Symbol;
use num_traits::ToPrimitive;

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

impl Interpreter {
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
            && self.enum_types.contains_key(name.as_str())
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
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = &target
        {
            let base_name_str = base_name.resolve();
            self.ensure_role_punned_to_class(&base_name_str);
            let mut selected_role = self.roles.get(&base_name_str).cloned();
            let mut selected_param_names = self
                .role_type_params
                .get(&base_name_str)
                .cloned()
                .unwrap_or_default();
            if let Some(candidates) = self.role_candidates.get(&base_name_str).cloned() {
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
                return Ok(Value::mixin(
                    Value::make_instance(*base_name, HashMap::new()),
                    mixins,
                ));
            }
        }

        if let Value::Package(class_name) = &target {
            let cn_resolved = class_name.resolve();
            let parametric = Self::parse_parametric_type_name(&cn_resolved);
            let (base_class_name, type_args) = if let Some((base, args)) = &parametric {
                (base.as_str(), Some(args.clone()))
            } else {
                (cn_resolved.as_str(), None)
            };
            if cn_resolved.starts_with("IO::Path::") && !self.classes.contains_key(&cn_resolved) {
                self.classes.insert(
                    cn_resolved.clone(),
                    ClassDef {
                        parents: vec!["IO::Path".to_string()],
                        attributes: Vec::new(),
                        methods: HashMap::new(),
                        native_methods: std::collections::HashSet::new(),
                        mro: Vec::new(),
                        attribute_types: HashMap::new(),
                        attribute_smileys: HashMap::new(),
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
            let class_key = if self.classes.contains_key(&cn_resolved) {
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
                    for (k, v) in attributes.iter() {
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
                let mut positional_path: Option<String> = None;
                let mut basename_part: Option<String> = None;
                let mut dirname_part: Option<String> = None;
                let mut volume_part: Option<String> = None;
                let mut cwd_attr: Option<String> = None;
                let mut spec_attr: Option<Value> = None;
                for arg in &args {
                    match arg {
                        Value::Pair(key, value) if key == "CWD" => {
                            cwd_attr = Some(value.to_string_value());
                        }
                        Value::Pair(key, value) if key == "SPEC" => {
                            spec_attr = Some((**value).clone());
                        }
                        Value::Pair(key, value) if key == "basename" => {
                            basename_part = Some(value.to_string_value());
                        }
                        Value::Pair(key, value) if key == "dirname" => {
                            dirname_part = Some(value.to_string_value());
                        }
                        Value::Pair(key, value) if key == "volume" => {
                            volume_part = Some(value.to_string_value());
                        }
                        Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } if positional_path.is_none()
                            && self
                                .class_mro(&class_name.resolve())
                                .iter()
                                .any(|n| n == "IO::Path") =>
                        {
                            positional_path = Some(
                                attributes
                                    .get("path")
                                    .map(|v| v.to_string_value())
                                    .unwrap_or_default(),
                            );
                            if cwd_attr.is_none() {
                                cwd_attr = attributes.get("cwd").map(|v| v.to_string_value());
                            }
                        }
                        Value::Pair(_, _) => {}
                        _ if positional_path.is_none() => {
                            positional_path = Some(arg.to_string_value());
                        }
                        _ => {}
                    }
                }
                // Determine dir separator from SPEC (Win32 uses '\', others use '/')
                let is_win32_spec = spec_attr
                    .as_ref()
                    .map(|s| {
                        let name = match s {
                            Value::Package(n) => n.resolve().to_string(),
                            Value::Instance { class_name, .. } => class_name.resolve().to_string(),
                            _ => String::new(),
                        };
                        name == "IO::Spec::Win32" || name.ends_with("Win32")
                    })
                    .unwrap_or(false);
                let dir_sep = if is_win32_spec { '\\' } else { '/' };
                let path = if let Some(positional) = positional_path {
                    positional
                } else if let Some(basename) = basename_part {
                    let mut built = match dirname_part {
                        Some(dirname) if !dirname.is_empty() => {
                            if dirname.ends_with('/') || dirname.ends_with('\\') {
                                format!("{dirname}{basename}")
                            } else {
                                format!("{dirname}{dir_sep}{basename}")
                            }
                        }
                        _ => basename,
                    };
                    if let Some(volume) = volume_part
                        && !volume.is_empty()
                    {
                        if volume.ends_with('/') || volume.ends_with('\\') {
                            built = format!("{volume}{built}");
                        } else {
                            built = format!("{volume}{dir_sep}{built}");
                        }
                    }
                    built
                } else {
                    String::new()
                };
                if path.is_empty() {
                    return Err(RuntimeError::new(
                        "Must specify a non-empty string as a path",
                    ));
                }
                if path.contains('\0') {
                    return Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    ));
                }
                let mut attrs = HashMap::new();
                attrs.insert("path".to_string(), Value::str(path));
                if let Some(cwd) = cwd_attr {
                    attrs.insert("cwd".to_string(), Value::str(cwd));
                }
                if cn_resolved.starts_with("IO::Path::") {
                    let spec_name =
                        format!("IO::Spec::{}", cn_resolved.trim_start_matches("IO::Path::"));
                    attrs.insert(
                        "SPEC".to_string(),
                        Value::Package(Symbol::intern(&spec_name)),
                    );
                } else if let Some(spec) = spec_attr {
                    attrs.insert("SPEC".to_string(), spec);
                }
                return Ok(Value::make_instance(*class_name, attrs));
            }
            match base_class_name {
                "IterationBuffer" => {
                    let mut items = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Array(vals, ..) | Value::Seq(vals) | Value::Slip(vals) => {
                                items.extend(vals.iter().cloned())
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "IterationBuffer" => {
                                if let Some(
                                    Value::Array(vals, ..) | Value::Seq(vals) | Value::Slip(vals),
                                ) = attributes.get("__mutsu_iterationbuffer_items")
                                {
                                    items.extend(vals.iter().cloned());
                                }
                            }
                            other => items.push(other.clone()),
                        }
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "__mutsu_iterationbuffer_items".to_string(),
                        Value::real_array(items),
                    );
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                "Array" | "List" | "Positional" | "array" => {
                    if let Some(dims) = self.shaped_dims_from_new_args(&args) {
                        // Check for :data argument to populate the shaped array
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
                        let shaped = Self::make_shaped_array(&dims);
                        if let Some(ref data_val) = data
                            && let Some(source_shape) =
                                crate::runtime::utils::shaped_array_shape(data_val)
                            && source_shape != dims
                        {
                            return Err(RuntimeError::new(
                                "X::Assignment::ArrayShapeMismatch: Cannot assign a shaped array to another shaped array with a different shape",
                            ));
                        }
                        if let Some(data_val) = data {
                            // Populate the shaped array with data
                            let data_items = match data_val {
                                Value::Array(items, ..)
                                | Value::Seq(items)
                                | Value::Slip(items) => items.to_vec(),
                                Value::Range(..)
                                | Value::RangeExcl(..)
                                | Value::RangeExclStart(..)
                                | Value::RangeExclBoth(..)
                                | Value::GenericRange { .. } => {
                                    crate::runtime::value_to_list(&data_val)
                                }
                                other => vec![other],
                            };
                            // For 1D shaped arrays, flatten nested arrays in the data
                            // e.g. Array.new(:shape(5,), [1,2,3,4,0]) should produce [1,2,3,4,0]
                            let data_items = if dims.len() == 1 {
                                let mut flat = Vec::new();
                                for item in data_items {
                                    match item {
                                        Value::Array(inner, ..)
                                        | Value::Seq(inner)
                                        | Value::Slip(inner) => {
                                            flat.extend(inner.iter().cloned());
                                        }
                                        Value::Range(..)
                                        | Value::RangeExcl(..)
                                        | Value::RangeExclStart(..)
                                        | Value::RangeExclBoth(..)
                                        | Value::GenericRange { .. } => {
                                            flat.extend(crate::runtime::value_to_list(&item));
                                        }
                                        other => flat.push(other),
                                    }
                                }
                                flat
                            } else {
                                data_items
                            };
                            if let Value::Array(ref items, is_arr) = shaped {
                                let dim_size = dims[0];
                                // For multi-dim arrays, check if data has flat items
                                // (X::Assignment::ToShaped) or too many items for the
                                // first dimension.
                                if dims.len() > 1
                                    && data_items
                                        .iter()
                                        .any(|v| !matches!(v, Value::Array(..) | Value::Nil))
                                {
                                    return Err(RuntimeError::new(
                                        "X::Assignment::ToShaped: Cannot assign a flat list to a shaped array",
                                    ));
                                }
                                if data_items.len() > dim_size {
                                    return Err(RuntimeError::new(format!(
                                        "Index {} for dimension 1 out of range (must be 0..{})",
                                        data_items.len() - 1,
                                        dim_size - 1
                                    )));
                                }
                                let mut new_items = items.as_ref().clone();
                                for (i, val) in data_items.into_iter().enumerate() {
                                    // For multi-dim: check sub-array size
                                    if dims.len() > 1
                                        && let Value::Array(sub_items, ..) = &val
                                        && sub_items.len() > dims[1]
                                    {
                                        return Err(RuntimeError::new(format!(
                                            "Index {} for dimension 2 out of range (must be 0..{})",
                                            sub_items.len() - 1,
                                            dims[1] - 1
                                        )));
                                    }
                                    if i < new_items.len() {
                                        new_items[i] = val;
                                    }
                                }
                                let result = Value::Array(std::sync::Arc::new(new_items), is_arr);
                                crate::runtime::utils::mark_shaped_array(&result, Some(&dims));
                                // Register type metadata for typed shaped arrays (e.g. array[str].new(:shape(5), ...))
                                if let Some(ref ta) = type_args
                                    && let Some(constraint) = ta.first()
                                {
                                    let info = crate::runtime::ContainerTypeInfo {
                                        value_type: constraint.clone(),
                                        key_type: None,
                                        declared_type: Some(class_name.resolve()),
                                    };
                                    self.register_container_type_metadata(&result, info);
                                }
                                return Ok(result);
                            }
                        }
                        // Register type metadata for typed shaped arrays (e.g. array[str].new(:shape(5)))
                        if let Some(ref ta) = type_args
                            && let Some(constraint) = ta.first()
                        {
                            let info = crate::runtime::ContainerTypeInfo {
                                value_type: constraint.clone(),
                                key_type: None,
                                declared_type: Some(class_name.resolve()),
                            };
                            self.register_container_type_metadata(&shaped, info);
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
                                attrs.insert("message".to_string(), Value::str(msg.clone()));
                                attrs
                                    .insert("operation".to_string(), Value::str_from("assignment"));
                                attrs.insert("got".to_string(), item.clone());
                                attrs.insert(
                                    "expected".to_string(),
                                    Value::Package(Symbol::intern(constraint)),
                                );
                                let ex = Value::make_instance(
                                    Symbol::intern("X::TypeCheck::Assignment"),
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
                            declared_type: Some(class_name.resolve()),
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
                    // or Map.new (always register Map declared_type)
                    let is_map = class_name.resolve() == "Map";
                    if type_args.is_some() || is_map {
                        let (value_type, key_type) = if let Some(ref ta) = type_args {
                            (ta.first().cloned().unwrap_or_default(), ta.get(1).cloned())
                        } else {
                            (String::new(), None)
                        };
                        let info = crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type,
                            declared_type: Some(class_name.resolve()),
                        };
                        self.register_container_type_metadata(&result, info);
                    }
                    return Ok(result);
                }
                "Uni" => {
                    // Flatten array arguments so Uni.new(@codes) works
                    let mut flat_args: Vec<&Value> = Vec::new();
                    for a in &args {
                        match a {
                            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                                for item in items.iter() {
                                    flat_args.push(item);
                                }
                            }
                            other => flat_args.push(other),
                        }
                    }
                    let text: String = flat_args
                        .iter()
                        .filter_map(|a| {
                            let cp = match a {
                                Value::Int(i) => *i as u32,
                                Value::Num(f) => *f as u32,
                                other => other.to_string_value().parse::<u32>().unwrap_or(0),
                            };
                            char::from_u32(cp)
                        })
                        .collect();
                    return Ok(Value::Uni {
                        form: String::new(),
                        text,
                    });
                }
                "Seq" => {
                    // Seq.new(iterator) — pull all items from the iterator
                    if let Some(iterator) = args.first() {
                        if matches!(iterator, Value::Instance { .. })
                            && self.type_matches_value("PredictiveIterator", iterator)
                        {
                            let seq = Value::Seq(std::sync::Arc::new(Vec::new()));
                            if let Value::Seq(items) = &seq {
                                let seq_id = std::sync::Arc::as_ptr(items) as usize;
                                self.env.insert(
                                    format!("__mutsu_predictive_seq_iter::{seq_id}"),
                                    iterator.clone(),
                                );
                            }
                            return Ok(seq);
                        }
                        if let Value::Instance { attributes, .. } = iterator
                            && let Some(Value::Array(items, ..)) =
                                attributes.get("items").or_else(|| attributes.get("stuff"))
                        {
                            return Ok(Value::Seq(std::sync::Arc::new(items.to_vec())));
                        }
                        let mut items = Vec::new();
                        let iter_slot = "$mutsu_seq_new_iterator";
                        let saved_iter = self.env.get(iter_slot).cloned();
                        self.env.insert(iter_slot.to_string(), iterator.clone());
                        let mut iterations = 0usize;
                        loop {
                            iterations += 1;
                            let current_iter =
                                self.env.get(iter_slot).cloned().unwrap_or(Value::Nil);
                            let val =
                                self.call_method_with_values(current_iter, "pull-one", vec![])?;
                            if iterations > 10_000 {
                                return Err(RuntimeError::new(format!(
                                    "Seq.new iterator did not terminate (last value: {})",
                                    val.to_string_value()
                                )));
                            }
                            if matches!(&val, Value::Str(s) if s.as_str() == "IterationEnd")
                                || matches!(&val, Value::Package(name) if *name == Symbol::intern("IterationEnd"))
                            {
                                break;
                            }
                            items.push(val);
                        }
                        if let Some(prev) = saved_iter {
                            self.env.insert(iter_slot.to_string(), prev);
                        } else {
                            self.env.remove(iter_slot);
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
                    let secs = args
                        .first()
                        .map(to_float_value)
                        .unwrap_or(Some(0.0))
                        .unwrap_or(0.0);
                    let mut attrs = HashMap::new();
                    attrs.insert("value".to_string(), Value::Num(secs));
                    return Ok(Value::make_instance(Symbol::intern("Duration"), attrs));
                }
                "Date" => {
                    use crate::builtins::methods_0arg::temporal;
                    let mut year: i64 = 1970;
                    let mut month: i64 = 1;
                    let mut day: i64 = 1;
                    let mut positional = Vec::new();
                    let mut has_named = false;
                    let mut formatter: Option<Value> = None;
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) => match key.as_str() {
                                "year" => {
                                    year = to_int(value);
                                    has_named = true;
                                }
                                "month" => {
                                    month = to_int(value);
                                    has_named = true;
                                }
                                "day" => {
                                    day = to_int(value);
                                    has_named = true;
                                }
                                "formatter" => {
                                    formatter = Some(*value.clone());
                                }
                                _ => {}
                            },
                            other => positional.push(other),
                        }
                    }
                    // Handle positional args: can be string, DateTime, or y/m/d
                    if let Some(v) = positional.first() {
                        match v {
                            // Single string arg: parse as date string
                            Value::Str(s) if positional.len() == 1 => {
                                let (y, m, d) = temporal::parse_date_string(s)?;
                                year = y;
                                month = m;
                                day = d;
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "DateTime" => {
                                let (y, m, d, _, _, _, _) = temporal::datetime_attrs(attributes);
                                year = y;
                                month = m;
                                day = d;
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "Instant" => {
                                // Convert Instant to Date via POSIX
                                let tai = match attributes.get("value") {
                                    Some(Value::Num(v)) => *v,
                                    Some(Value::Int(v)) => *v as f64,
                                    _ => 0.0,
                                };
                                let posix = temporal::instant_to_posix(tai);
                                let epoch_days = (posix / 86400.0).floor() as i64;
                                let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                                year = y;
                                month = m;
                                day = d;
                            }
                            _ => {
                                year = to_int(v);
                                if let Some(v2) = positional.get(1) {
                                    month = to_int(v2);
                                }
                                if let Some(v3) = positional.get(2) {
                                    day = to_int(v3);
                                }
                            }
                        }
                    } else if !has_named {
                        return Err(RuntimeError::new("Date.new requires arguments"));
                    }
                    temporal::validate_date(year, month, day)?;
                    let date =
                        temporal::make_date_with_formatter(year, month, day, formatter.clone());
                    if let Some(formatter_value) = formatter {
                        return self.render_date_formatter(date, formatter_value);
                    }
                    return Ok(date);
                }
                "DateTime" => {
                    use crate::builtins::methods_0arg::temporal;
                    let mut year: i64 = 1970;
                    let mut month: i64 = 1;
                    let mut day: i64 = 1;
                    let mut hour: i64 = 0;
                    let mut minute: i64 = 0;
                    let mut second: f64 = 0.0;
                    let mut timezone: i64 = 0;
                    let mut timezone_set = false;
                    let mut formatter: Option<Value> = None;
                    let mut has_component_named = false;
                    let mut positional = Vec::new();
                    let mut has_named = false;
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) => match key.as_str() {
                                "year" => {
                                    year = to_int(value);
                                    has_named = true;
                                    has_component_named = true;
                                }
                                "month" => {
                                    month = to_int(value);
                                    has_named = true;
                                    has_component_named = true;
                                }
                                "day" => {
                                    day = to_int(value);
                                    has_named = true;
                                    has_component_named = true;
                                }
                                "hour" => {
                                    hour = to_int(value);
                                    has_named = true;
                                    has_component_named = true;
                                }
                                "minute" => {
                                    minute = to_int(value);
                                    has_named = true;
                                    has_component_named = true;
                                }
                                "second" => {
                                    second = to_float_value(value).unwrap_or(0.0);
                                    has_named = true;
                                    has_component_named = true;
                                }
                                "timezone" => {
                                    timezone = to_int(value);
                                    timezone_set = true;
                                    has_named = true;
                                }
                                "date" => {
                                    if let Value::Instance {
                                        class_name,
                                        attributes,
                                        ..
                                    } = value.as_ref()
                                        && class_name == "Date"
                                    {
                                        let (y, m, d) = temporal::date_attrs(attributes);
                                        year = y;
                                        month = m;
                                        day = d;
                                        has_named = true;
                                        has_component_named = true;
                                    }
                                }
                                "formatter" => {
                                    formatter = Some(*value.clone());
                                    has_named = true;
                                }
                                _ => {}
                            },
                            other => positional.push(other),
                        }
                    }
                    if has_component_named && !positional.is_empty() {
                        return Err(RuntimeError::new(
                            "DateTime.new cannot mix positional and component named arguments",
                        ));
                    }
                    if let Some(v) = positional.first() {
                        match v {
                            Value::Str(s) => {
                                let (y, mo, d, h, mi, sec, tz) =
                                    temporal::parse_datetime_string(s)?;
                                year = y;
                                month = mo;
                                day = d;
                                hour = h;
                                minute = mi;
                                second = sec;
                                if !timezone_set {
                                    timezone = tz;
                                }
                                has_named = true;
                            }
                            Value::Int(epoch) => {
                                // DateTime.new(posix-timestamp)
                                let total = *epoch as f64 + timezone as f64;
                                let total_i = total.floor() as i64;
                                let frac = total - total_i as f64;
                                let day_secs = total_i.rem_euclid(86400);
                                let epoch_days = (total_i - day_secs) / 86400;
                                let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                                year = y;
                                month = m;
                                day = d;
                                hour = day_secs / 3600;
                                minute = (day_secs % 3600) / 60;
                                second = (day_secs % 60) as f64 + frac;
                                has_named = true;
                            }
                            Value::BigInt(epoch) => {
                                let total =
                                    epoch.as_ref().clone() + num_bigint::BigInt::from(timezone);
                                let secs_per_day = num_bigint::BigInt::from(86_400i64);
                                let day_secs_big =
                                    ((&total % &secs_per_day) + &secs_per_day) % &secs_per_day;
                                let epoch_days_big = (&total - &day_secs_big) / &secs_per_day;
                                let epoch_days = epoch_days_big.to_i64().ok_or_else(|| {
                                    RuntimeError::new(
                                        "X::DateTime::Range: epoch day out of range".to_string(),
                                    )
                                })?;
                                let day_secs = day_secs_big.to_i64().ok_or_else(|| {
                                    RuntimeError::new(
                                        "X::DateTime::Range: day second out of range".to_string(),
                                    )
                                })?;
                                let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                                year = y;
                                month = m;
                                day = d;
                                hour = day_secs / 3600;
                                minute = (day_secs % 3600) / 60;
                                second = (day_secs % 60) as f64;
                                has_named = true;
                            }
                            Value::Num(epoch) => {
                                let total = *epoch + timezone as f64;
                                let total_i = total.floor() as i64;
                                let frac = total - total_i as f64;
                                let day_secs = total_i.rem_euclid(86400);
                                let epoch_days = (total_i - day_secs) / 86400;
                                let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                                year = y;
                                month = m;
                                day = d;
                                hour = day_secs / 3600;
                                minute = (day_secs % 3600) / 60;
                                second = (day_secs % 60) as f64 + frac;
                                has_named = true;
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "Date" => {
                                let (y, m, d) = temporal::date_attrs(attributes);
                                year = y;
                                month = m;
                                day = d;
                                hour = 0;
                                minute = 0;
                                second = 0.0;
                                has_named = true;
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "Instant" => {
                                let tai = match attributes.get("value") {
                                    Some(Value::Num(n)) => *n,
                                    Some(Value::Int(n)) => *n as f64,
                                    _ => 0.0,
                                };
                                // Convert from TAI (Instant) to POSIX
                                let epoch = temporal::instant_to_posix(tai);
                                let total_i = epoch.floor() as i64;
                                let frac = epoch - total_i as f64;
                                let day_secs = total_i.rem_euclid(86400);
                                let epoch_days = (total_i - day_secs) / 86400;
                                let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                                year = y;
                                month = m;
                                day = d;
                                hour = day_secs / 3600;
                                minute = (day_secs % 3600) / 60;
                                second = (day_secs % 60) as f64 + frac;
                                has_named = true;
                            }
                            other if other.is_numeric() => {
                                // Rat, BigInt, etc. - coerce to f64
                                let epoch = other.to_f64() + timezone as f64;
                                let total_i = epoch.floor() as i64;
                                let frac = epoch - total_i as f64;
                                let day_secs = total_i.rem_euclid(86400);
                                let epoch_days = (total_i - day_secs) / 86400;
                                let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                                year = y;
                                month = m;
                                day = d;
                                hour = day_secs / 3600;
                                minute = (day_secs % 3600) / 60;
                                second = (day_secs % 60) as f64 + frac;
                                has_named = true;
                            }
                            _ => {}
                        }
                    }
                    if has_named {
                        temporal::validate_datetime(
                            year, month, day, hour, minute, second, timezone,
                        )?;
                        let dt = temporal::make_datetime(
                            year, month, day, hour, minute, second, timezone,
                        );
                        if let Some(formatter_value) = formatter
                            && let Value::Instance {
                                class_name,
                                ref attributes,
                                id,
                            } = dt
                        {
                            let mut attrs = (**attributes).clone();
                            attrs.insert("formatter".to_string(), formatter_value.clone());
                            let dt_with_formatter =
                                Value::make_instance_with_id(class_name, attrs, id);
                            let saved_env = self.env().clone();
                            let saved_readonly = self.save_readonly_vars();
                            let rendered = self
                                .eval_call_on_value(
                                    formatter_value,
                                    vec![dt_with_formatter.clone()],
                                )?
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
                                updated.insert(
                                    "__formatter_rendered".to_string(),
                                    Value::str(rendered),
                                );
                                return Ok(Value::make_instance_with_id(class_name, updated, id));
                            }
                        }
                        return Ok(dt);
                    }
                    return Err(RuntimeError::new("DateTime.new requires arguments"));
                }
                "IO::Socket::INET" => {
                    return self.dispatch_socket_inet_new(&args);
                }
                "Promise" => {
                    return Ok(Value::Promise(SharedPromise::new()));
                }
                "Channel" => {
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
                    let sched_id = super::native_methods::next_fake_scheduler_id();
                    // Initialize with current time (use 0.0 as base for
                    // virtual time)
                    super::native_methods::fake_scheduler_init(sched_id, 0.0);
                    let mut attrs = HashMap::new();
                    attrs.insert("scheduler_id".to_string(), Value::Int(sched_id as i64));
                    return Ok(Value::make_instance(Symbol::intern("FakeScheduler"), attrs));
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
                        subclass: None,
                        decontainerized: false,
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
                                short_name = Some(s.to_string());
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
                    return Ok(Value::CompUnitDepSpec {
                        short_name: Symbol::intern(&short_name),
                    });
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
                    let mut positional = Vec::new();
                    let mut w_flag = false;
                    let mut enc = Value::str_from("utf-8");
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "w" => {
                                w_flag = value.truthy();
                            }
                            Value::Pair(key, _value) if key == "out" => {}
                            Value::Pair(key, value) if key == "enc" => {
                                enc = Value::str(value.to_string_value());
                            }
                            _ => positional.push(arg.clone()),
                        }
                    }
                    let stdout_id = super::native_methods::next_supply_id();
                    let stderr_id = super::native_methods::next_supply_id();
                    let supply_id = super::native_methods::next_supply_id();
                    let stdout_descriptor = SharedPromise::new();
                    let stderr_descriptor = SharedPromise::new();
                    let mut stdout_supply_attrs = HashMap::new();
                    stdout_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stdout_id as i64));
                    stdout_supply_attrs.insert("enc".to_string(), enc.clone());
                    stdout_supply_attrs.insert(
                        "native_descriptor_promise".to_string(),
                        Value::Promise(stdout_descriptor),
                    );
                    let mut stderr_supply_attrs = HashMap::new();
                    stderr_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stderr_id as i64));
                    stderr_supply_attrs.insert("enc".to_string(), enc.clone());
                    stderr_supply_attrs.insert(
                        "native_descriptor_promise".to_string(),
                        Value::Promise(stderr_descriptor),
                    );
                    let mut merged_supply_attrs = HashMap::new();
                    merged_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    merged_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    merged_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(supply_id as i64));

                    let mut attrs = HashMap::new();
                    attrs.insert("cmd".to_string(), Value::array(positional));
                    attrs.insert("started".to_string(), Value::Bool(false));
                    attrs.insert("enc".to_string(), enc);
                    attrs.insert(
                        "stdout".to_string(),
                        Value::make_instance(Symbol::intern("Supply"), stdout_supply_attrs),
                    );
                    attrs.insert(
                        "stderr".to_string(),
                        Value::make_instance(Symbol::intern("Supply"), stderr_supply_attrs),
                    );
                    attrs.insert(
                        "supply".to_string(),
                        Value::make_instance(Symbol::intern("Supply"), merged_supply_attrs),
                    );
                    if w_flag {
                        attrs.insert("w".to_string(), Value::Bool(true));
                    }
                    return Ok(Value::make_instance(*class_name, attrs));
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
                    return Ok(Value::make_instance(*class_name, attrs));
                }
                "Buf" | "buf8" | "Buf[uint8]" | "Blob" | "blob8" | "Blob[uint8]" | "buf16"
                | "buf32" | "buf64" | "blob16" | "blob32" | "blob64" => {
                    let cn = class_name.resolve();
                    let raw_vals: Vec<Value> = args
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
                                || class_name == "utf8"
                                || class_name == "utf16"
                                || class_name.resolve().starts_with("Buf[")
                                || class_name.resolve().starts_with("Blob[")
                                || class_name.resolve().starts_with("buf")
                                || class_name.resolve().starts_with("blob") =>
                            {
                                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                                    items.to_vec()
                                } else {
                                    Vec::new()
                                }
                            }
                            other => vec![Value::Int(to_int(other))],
                        })
                        .collect();
                    // Mask values to unsigned range based on element size
                    let byte_vals: Vec<Value> = raw_vals
                        .into_iter()
                        .map(|v| {
                            let i = to_int(&v);
                            if cn.contains("64") {
                                Value::Int(i as u64 as i64)
                            } else if cn.contains("32") {
                                Value::Int(i as u32 as i64)
                            } else if cn.contains("16") {
                                Value::Int(i as u16 as i64)
                            } else {
                                Value::Int(i as u8 as i64)
                            }
                        })
                        .collect();
                    let mut attrs = HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    // Normalize short buf/blob names to canonical forms
                    let canonical_name = match cn.as_str() {
                        "buf8" => Symbol::intern("Buf[uint8]"),
                        "buf16" => Symbol::intern("Buf[uint16]"),
                        "buf32" => Symbol::intern("Buf[uint32]"),
                        "buf64" => Symbol::intern("Buf[uint64]"),
                        "blob8" => Symbol::intern("Blob[uint8]"),
                        "blob16" => Symbol::intern("Blob[uint16]"),
                        "blob32" => Symbol::intern("Blob[uint32]"),
                        "blob64" => Symbol::intern("Blob[uint64]"),
                        _ => *class_name,
                    };
                    return Ok(Value::make_instance(canonical_name, attrs));
                }
                "Rat" => {
                    use num_bigint::BigInt;
                    // Handle BigInt args to avoid truncation
                    let has_big = args.iter().take(2).any(|v| matches!(v, Value::BigInt(_)));
                    if has_big {
                        let a = match args.first() {
                            Some(Value::BigInt(bi)) => (**bi).clone(),
                            Some(v) => BigInt::from(to_int(v)),
                            None => BigInt::from(0),
                        };
                        let b = match args.get(1) {
                            Some(Value::BigInt(bi)) => (**bi).clone(),
                            Some(v) => BigInt::from(to_int(v)),
                            None => BigInt::from(1),
                        };
                        return Ok(crate::value::make_big_rat(a, b));
                    }
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
                    use crate::value::make_big_rat;
                    use num_bigint::BigInt;
                    let a = match args.first() {
                        Some(Value::BigInt(bi)) => (**bi).clone(),
                        Some(v) => BigInt::from(to_int(v)),
                        None => BigInt::from(0),
                    };
                    let b = match args.get(1) {
                        Some(Value::BigInt(bi)) => (**bi).clone(),
                        Some(v) => BigInt::from(to_int(v)),
                        None => BigInt::from(1),
                    };
                    return Ok(match make_big_rat(a, b) {
                        Value::Rat(n, d) => Value::FatRat(n, d),
                        Value::BigRat(n, d) => Value::BigRat(n, d),
                        other => other,
                    });
                }
                "Pair" => {
                    // Pair.new(key, value)
                    let key = args.first().cloned().unwrap_or(Value::Nil);
                    let value = args.get(1).cloned().unwrap_or(Value::Nil);
                    return Ok(Value::ValuePair(Box::new(key), Box::new(value)));
                }
                "Set" | "SetHash" => {
                    // Set/SetHash.new treats each element as an opaque value
                    // (does NOT decompose Pairs like the .Set/.SetHash coercion does).
                    // Single arg: if QuantHash, treat as single element; otherwise iterate.
                    // Multiple args: each is a single element.
                    let mut elems = HashSet::new();
                    let mut original_keys: HashMap<String, Value> = HashMap::new();
                    let mut has_non_str_keys = false;
                    let add_item = |elems: &mut HashSet<String>,
                                    original_keys: &mut HashMap<String, Value>,
                                    has_non_str: &mut bool,
                                    item: &Value| {
                        let str_key = item.to_string_value();
                        if !matches!(item, Value::Str(_)) {
                            *has_non_str = true;
                            original_keys
                                .entry(str_key.clone())
                                .or_insert_with(|| item.clone());
                        }
                        elems.insert(str_key);
                    };
                    if args.len() == 1 {
                        let arg = &args[0];
                        // QuantHash types are always single elements
                        if matches!(arg, Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)) {
                            add_item(&mut elems, &mut original_keys, &mut has_non_str_keys, arg);
                        } else {
                            for item in Self::value_to_list(arg) {
                                add_item(
                                    &mut elems,
                                    &mut original_keys,
                                    &mut has_non_str_keys,
                                    &item,
                                );
                            }
                        }
                    } else {
                        for arg in &args {
                            add_item(&mut elems, &mut original_keys, &mut has_non_str_keys, arg);
                        }
                    }
                    let is_mutable = base_class_name == "SetHash";
                    return Ok(if has_non_str_keys {
                        if is_mutable {
                            Value::set_hash_typed(elems, original_keys)
                        } else {
                            Value::set_typed(elems, original_keys)
                        }
                    } else if is_mutable {
                        Value::set_hash(elems)
                    } else {
                        Value::set(elems)
                    });
                }
                "Bag" | "BagHash" => {
                    // Check for lazy inputs
                    for a in &args {
                        if Self::is_lazy_for_coerce(a) {
                            return Err(RuntimeError::cannot_lazy_what(base_class_name));
                        }
                    }
                    // BagHash.new(|c) takes a Capture:
                    // - Single arg: iterate over it (flatten lists/arrays/hashes,
                    //   but NOT QuantHash types which are single elements)
                    // - Multiple args: each arg is a single element (no flattening)
                    let mut counts: HashMap<String, i64> = HashMap::new();
                    let mut original_keys: HashMap<String, Value> = HashMap::new();
                    let mut has_non_str_keys = false;
                    let add_item = |counts: &mut HashMap<String, i64>,
                                    original_keys: &mut HashMap<String, Value>,
                                    has_non_str: &mut bool,
                                    item: &Value| {
                        let str_key = item.to_string_value();
                        if !matches!(item, Value::Str(_)) {
                            *has_non_str = true;
                            original_keys
                                .entry(str_key.clone())
                                .or_insert_with(|| item.clone());
                        }
                        *counts.entry(str_key).or_insert(0) += 1;
                    };
                    if args.len() == 1 {
                        let arg = &args[0];
                        // QuantHash types are always single elements
                        if matches!(arg, Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)) {
                            add_item(&mut counts, &mut original_keys, &mut has_non_str_keys, arg);
                        } else {
                            for item in Self::value_to_list(arg) {
                                add_item(
                                    &mut counts,
                                    &mut original_keys,
                                    &mut has_non_str_keys,
                                    &item,
                                );
                            }
                        }
                    } else {
                        for arg in &args {
                            add_item(&mut counts, &mut original_keys, &mut has_non_str_keys, arg);
                        }
                    }
                    return Ok(if has_non_str_keys {
                        if base_class_name == "BagHash" {
                            Value::bag_hash_typed(counts, original_keys)
                        } else {
                            Value::bag_typed(counts, original_keys)
                        }
                    } else if base_class_name == "BagHash" {
                        Value::bag_hash(counts)
                    } else {
                        Value::bag(counts)
                    });
                }
                "Mix" | "MixHash" => {
                    // MixHash.new treats all positional arguments as elements
                    // to count. Pairs are NOT decomposed into key=>weight;
                    // they are stringified and treated as individual elements.
                    // Only .MixHash coercion decomposes pairs.
                    // QuantHash types (Set, Bag, Mix) are treated as single
                    // elements, not flattened.
                    let mut weights: HashMap<String, f64> = HashMap::new();
                    let mut original_keys: HashMap<String, Value> = HashMap::new();
                    let mut has_non_str_keys = false;
                    let add_item = |weights: &mut HashMap<String, f64>,
                                    original_keys: &mut HashMap<String, Value>,
                                    has_non_str: &mut bool,
                                    item: &Value| {
                        let str_key = item.to_string_value();
                        if !matches!(item, Value::Str(_)) {
                            *has_non_str = true;
                            original_keys
                                .entry(str_key.clone())
                                .or_insert_with(|| item.clone());
                        }
                        *weights.entry(str_key).or_insert(0.0) += 1.0;
                    };
                    // Check for lazy iterables
                    for arg in &args {
                        if Self::is_lazy_for_set_ops(arg) {
                            let mut err = RuntimeError::new("Cannot .Mix a lazy list");
                            err.exception = Some(Box::new(Value::make_instance(
                                Symbol::intern("X::Cannot::Lazy"),
                                [("what".to_string(), Value::str_from("Mix"))]
                                    .into_iter()
                                    .collect(),
                            )));
                            return Err(err);
                        }
                    }
                    if args.len() == 1 {
                        let arg = &args[0];
                        // Single arg: QuantHash types are single elements, others flatten
                        if matches!(arg, Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)) {
                            add_item(&mut weights, &mut original_keys, &mut has_non_str_keys, arg);
                        } else {
                            for item in Self::value_to_list(arg) {
                                add_item(
                                    &mut weights,
                                    &mut original_keys,
                                    &mut has_non_str_keys,
                                    &item,
                                );
                            }
                        }
                    } else {
                        // Multiple args: each arg is a single element (capture semantics)
                        for arg in &args {
                            add_item(&mut weights, &mut original_keys, &mut has_non_str_keys, arg);
                        }
                    }
                    let is_hash_variant = class_name.resolve() == "MixHash";
                    let result = if has_non_str_keys {
                        if is_hash_variant {
                            Value::mix_hash_with_original_keys(weights, original_keys)
                        } else {
                            Value::mix_with_original_keys(weights, original_keys)
                        }
                    } else if is_hash_variant {
                        Value::mix_hash(weights)
                    } else {
                        Value::mix(weights)
                    };
                    if is_hash_variant {
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
                "Complex" => {
                    let re = match args.first() {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        Some(Value::Rat(n, d)) if *d != 0 => *n as f64 / *d as f64,
                        Some(v) => to_float_value(v).unwrap_or(0.0),
                        _ => 0.0,
                    };
                    let im = match args.get(1) {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        Some(Value::Rat(n, d)) if *d != 0 => *n as f64 / *d as f64,
                        Some(v) => to_float_value(v).unwrap_or(0.0),
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
                    frame_attrs.insert("file".to_string(), Value::str(file));
                    let frame =
                        Value::make_instance(Symbol::intern("Backtrace::Frame"), frame_attrs);
                    return Ok(Value::array(vec![frame]));
                }
                "Lock" | "Lock::Async" | "Lock::Soft" => {
                    let mut attrs = HashMap::new();
                    let lock_id = super::native_methods::next_lock_id() as i64;
                    attrs.insert("lock-id".to_string(), Value::Int(lock_id));
                    return Ok(Value::make_instance(*class_name, attrs));
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
                    attrs.insert("str".to_string(), Value::str(matched));
                    attrs.insert("from".to_string(), Value::Int(from));
                    attrs.insert("to".to_string(), Value::Int(to));
                    attrs.insert("orig".to_string(), Value::str(orig));
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
                    return Ok(Value::make_instance(Symbol::intern("Match"), attrs));
                }
                // Types that cannot be instantiated with .new
                "HyperWhatever" | "Whatever" => {
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
                    if let Some(dims) = self.shaped_dims_from_new_args(&args) {
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
                                declared_type: Some(class_name.resolve()),
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
                            declared_type: Some(class_name.resolve()),
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
                            declared_type: Some(class_name.resolve()),
                        },
                    );
                    return Ok(result);
                }
            }
            if let Some(role) = self.roles.get(&class_name.resolve()).cloned() {
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
                if let Some(parent_names) = self.role_parents.get(&class_name.resolve()).cloned() {
                    let mut role_stack: Vec<String> = parent_names;
                    let mut visited = vec![class_name.resolve()];
                    while let Some(parent_role_name) = role_stack.pop() {
                        if !visited.contains(&parent_role_name) {
                            visited.push(parent_role_name.clone());
                            if let Some(parent_role) = self.roles.get(&parent_role_name).cloned() {
                                for attr in &parent_role.attributes {
                                    if !all_attributes.iter().any(|a| a.0 == attr.0) {
                                        all_attributes.push(attr.clone());
                                    }
                                }
                            }
                            if let Some(grandparents) =
                                self.role_parents.get(&parent_role_name).cloned()
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
                return Ok(Value::mixin(
                    Value::make_instance(*class_name, HashMap::new()),
                    mixins,
                ));
            }
            // CUnion repr classes use byte-overlay construction
            if self.cunion_classes.contains(&cn_resolved) {
                return self.construct_cunion_instance(&cn_resolved, &args);
            }
            // Auto-pun role to class if needed (e.g., role COERCE calling self.new)
            if !self.classes.contains_key(&cn_resolved) && self.roles.contains_key(&cn_resolved) {
                self.ensure_role_punned_to_class(&cn_resolved);
            }
            if self.classes.contains_key(&cn_resolved)
                || type_args
                    .as_ref()
                    .is_some_and(|_| self.classes.contains_key(base_class_name))
            {
                let class_key = if self.classes.contains_key(&cn_resolved) {
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
                            let msg = e.message.to_lowercase();
                            if !msg.contains("no matching candidates") {
                                return Err(e);
                            }
                            // Mu.new only accepts named arguments. If the call
                            // had positional args and no multi candidate matched,
                            // die like Raku does.
                            let has_positional = args
                                .iter()
                                .any(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)));
                            if has_positional {
                                return Err(RuntimeError::new(format!(
                                    "Default constructor for '{}' only takes named arguments",
                                    class_name.resolve()
                                )));
                            }
                            // Fall through to default constructor below
                        }
                    }
                }
                let mut attrs = HashMap::new();
                let mut positional_ctor_args: Vec<Value> = Vec::new();
                let saved_default_env = self.env.clone();
                if let Some(role_bindings) = self.class_role_param_bindings.get(class_key) {
                    for (name, value) in role_bindings {
                        self.env.insert(name.clone(), value.clone());
                    }
                } else if let Some(role_bindings) =
                    self.class_role_param_bindings.get(&class_name.resolve())
                {
                    for (name, value) in role_bindings {
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
                // First, collect constructor args into attrs
                self.env = saved_default_env.clone();
                let class_mro = self.class_mro(class_key);
                // When BUILD is defined, it controls attribute initialization,
                // so we skip automatic named-arg-to-attribute mapping.
                let any_build = class_mro.iter().any(|cn| {
                    cn != "Any"
                        && cn != "Mu"
                        && self
                            .classes
                            .get(cn)
                            .and_then(|def| def.methods.get("BUILD"))
                            .is_some()
                });
                for val in &args {
                    match val {
                        Value::Pair(k, v) => {
                            if !any_build {
                                let coerced = Self::coerce_attr_value_by_sigil(
                                    *v.clone(),
                                    sigil_map.get(k).copied().unwrap_or('$'),
                                );
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
                            for (attr, value) in src_attrs.iter() {
                                attrs.insert(attr.clone(), value.clone());
                            }
                        }
                        _ => {
                            positional_ctor_args.push(val.clone());
                        }
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
                            _ => vec![val.clone()],
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
                if let Some(role_bindings) = self.class_role_param_bindings.get(class_key) {
                    for (name, value) in role_bindings {
                        self.env.insert(name.clone(), value.clone());
                    }
                } else if let Some(role_bindings) =
                    self.class_role_param_bindings.get(&class_name.resolve())
                {
                    for (name, value) in role_bindings {
                        self.env.insert(name.clone(), value.clone());
                    }
                }
                for (attr_name, _is_public, default, _is_rw, _is_required, sigil, _) in
                    class_attrs_info.clone()
                {
                    if attrs.contains_key(&attr_name) {
                        continue;
                    }
                    let val = if let Some(build_override) = self
                        .attribute_build_overrides
                        .get(&(class_key.to_string(), attr_name.clone()))
                        .cloned()
                    {
                        let val = self.call_sub_value(build_override, Vec::new(), false)?;
                        Self::coerce_attr_value_by_sigil(val, sigil)
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
                        let val = result?;
                        Self::coerce_attr_value_by_sigil(val, sigil)
                    } else {
                        match sigil {
                            '@' => {
                                // Check for `is Type` trait (e.g. `has @.a is Buf`)
                                let is_type = self
                                    .class_attribute_is_types
                                    .get(&(class_key.to_string(), attr_name.clone()))
                                    .cloned();
                                if let Some(type_name) = is_type {
                                    let type_obj =
                                        Value::Package(crate::symbol::Symbol::intern(&type_name));
                                    match self.call_method_with_values(type_obj, "new", vec![]) {
                                        Ok(v) => v,
                                        Err(_) => Value::real_array(Vec::new()),
                                    }
                                } else {
                                    let arr = Value::real_array(Vec::new());
                                    // Register element type constraint for typed array attributes
                                    if let Some(tc) = self
                                        .classes
                                        .get(class_key)
                                        .and_then(|cd| cd.attribute_types.get(&attr_name))
                                        .cloned()
                                    {
                                        self.register_container_type_metadata(
                                            &arr,
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
                                    if let Some(tc) = self
                                        .classes
                                        .get(class_key)
                                        .and_then(|cd| cd.attribute_types.get(&attr_name))
                                        .cloned()
                                    {
                                        self.register_container_type_metadata(
                                            &h,
                                            super::ContainerTypeInfo {
                                                value_type: tc,
                                                key_type: None,
                                                declared_type: None,
                                            },
                                        );
                                    }
                                    h
                                }
                            }
                            _ => Value::Nil,
                        }
                    };
                    attrs.insert(attr_name, val);
                }
                // Add alias metadata for `has $x` (no twigil) attributes
                self.add_alias_attribute_metadata(class_key, &mut attrs);
                self.enforce_attribute_where_constraints(class_key, &class_attrs_info, &attrs)?;
                self.enforce_attribute_smiley_constraints(class_key, &attrs)?;
                // Restore env after default evaluation, but preserve side effects
                // on variables that already existed in the caller environment.
                let mut restored_env = saved_default_env.clone();
                for (key, value) in self.env.iter() {
                    if restored_env.contains_key(key) {
                        restored_env.insert(key.clone(), value.clone());
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
                let is_6e = crate::parser::current_language_version().starts_with("6.e");
                for mro_class in mro.iter().rev() {
                    if mro_class == "Any" || mro_class == "Mu" {
                        continue;
                    }
                    // Skip role entries in MRO — they are handled separately below
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
                                attrs.clone(),
                                args.clone(),
                                Some(Value::make_instance(*class_name, attrs.clone())),
                            )?;
                            attrs = updated;
                        }
                    }
                    let has_build = self
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("BUILD"))
                        .is_some();
                    if has_build {
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
                            Err(err) if err.is_fail => {
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
                            .classes
                            .get(cn)
                            .and_then(|def| def.methods.get("BUILD"))
                            .is_some()
                });
                // Also check role BUILD submethods for required attribute enforcement
                let any_role_build = is_6e
                    && mro.iter().any(|cn| {
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
                    }
                    let has_tweak = self
                        .classes
                        .get(mro_class)
                        .and_then(|def| def.methods.get("TWEAK"))
                        .is_some();
                    if has_tweak {
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
                    for (
                        declaring_class,
                        (attr_name, _is_public, default, _is_rw, _is_required, sigil, _),
                    ) in per_class_attrs
                    {
                        let qualified_key = format!("{}\0{}", declaring_class, attr_name);
                        if attrs.contains_key(&qualified_key) {
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
                        let val = if let Some(expr) = default {
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
                                .class_attribute_is_types
                                .get(&(declaring_class.clone(), attr_name.clone()))
                                .cloned();
                            if let Some(type_name) = is_type_val {
                                let type_obj =
                                    Value::Package(crate::symbol::Symbol::intern(&type_name));
                                self.call_method_with_values(type_obj, "new", vec![])
                                    .unwrap_or_else(|_| match sigil {
                                        '@' => Value::real_array(Vec::new()),
                                        '%' => Value::hash(HashMap::new()),
                                        _ => Value::Nil,
                                    })
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
                let instance = Value::make_instance(*class_name, attrs);
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
                                declared_type: Some(class_name.resolve()),
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
                    "Int" => {
                        if matches!(args.first(), Some(Value::Package(_))) {
                            return Err(RuntimeError::new("Cannot convert type object to Int"));
                        }
                        let int_val = args.first().map_or(0, crate::runtime::to_int);
                        Ok(Value::Int(int_val))
                    }
                    "Str" => Ok(Value::str(String::new())),
                    "Num" => {
                        if let Some(arg) = args.first() {
                            // Type objects (e.g. anonymous class {}) cannot coerce to Num
                            if matches!(arg, Value::Package(_)) {
                                return Err(RuntimeError::new(
                                    "Cannot coerce to Num: no .Num method found",
                                ));
                            }
                            match crate::runtime::to_float_value(arg) {
                                Some(f) => Ok(Value::Num(f)),
                                None => Err(RuntimeError::new(
                                    "Cannot coerce to Num: no .Num method found",
                                )),
                            }
                        } else {
                            Ok(Value::Num(0.0))
                        }
                    }
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
                            "lock-id".to_string(),
                            Value::Int(super::native_methods::next_lock_id() as i64),
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

    fn check_attribute_where_constraint(&mut self, pred: &Expr, value: &Value) -> bool {
        let pred_val = match self.eval_block_value(&[Stmt::Expr(pred.clone())]) {
            Ok(v) => v,
            Err(_) => return false,
        };
        match self.call_sub_value(pred_val, vec![value.clone()], false) {
            Ok(result) => result.truthy(),
            Err(_) => false,
        }
    }

    /// For a given class, return the ordered list of (role_name, MethodDef) pairs
    /// for role submethods with the given name (e.g. "BUILD", "TWEAK", "DESTROY").
    /// The order respects role composition: sub-roles come before the role that
    /// composes them. Only submethods (is_my == true) are included; regular methods
    /// in roles are skipped.
    pub(super) fn ordered_role_submethods_for_class(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Vec<(String, MethodDef)> {
        let composed = match self.class_composed_roles.get(class_name) {
            Some(roles) => roles.clone(),
            None => return Vec::new(),
        };
        // Build the correct order: for each directly composed role (in order),
        // recursively include parent roles (depth-first) before the role itself.
        // Deduplicate to avoid calling the same role submethod twice for the same class.
        let mut ordered = Vec::new();
        let mut seen = std::collections::HashSet::new();
        // Figure out which roles are "direct" (from `does` declarations) vs transitive.
        // Direct roles are those not reachable through another direct role's parents.
        // However, `class_composed_roles` includes both direct and transitive roles in
        // an unspecified order. We need to reconstruct the proper depth-first order.
        //
        // Strategy: for each role in composed list, expand it depth-first (parents first).
        // The composed list may have the order [R1, R0, R2] where R0 is a parent of R1.
        // We want [R0, R1, R2]. We achieve this by expanding each role and skipping
        // already-seen roles.
        for role in &composed {
            let role_base = role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(role.as_str());
            self.expand_role_depth_first(role_base, &mut ordered, &mut seen);
        }
        // Now filter to only roles that have the requested submethod
        let mut result = Vec::new();
        for role_name in &ordered {
            if let Some(role_def) = self.roles.get(role_name)
                && let Some(overloads) = role_def.methods.get(method_name)
            {
                for md in overloads {
                    if md.is_my {
                        result.push((role_name.clone(), md.clone()));
                    }
                }
            }
        }
        result
    }

    /// Recursively expand a role and its parent roles in depth-first order
    /// (parent roles first, then the role itself).
    fn expand_role_depth_first(
        &self,
        role_name: &str,
        ordered: &mut Vec<String>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        if !seen.insert(role_name.to_string()) {
            return;
        }
        // First, expand parent roles
        if let Some(parents) = self.role_parents.get(role_name) {
            for parent in parents {
                let parent_base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.roles.contains_key(parent_base) {
                    self.expand_role_depth_first(parent_base, ordered, seen);
                }
            }
        }
        // Then add the role itself
        ordered.push(role_name.to_string());
    }

    fn collect_attribute_type_constraints(&mut self, class_name: &str) -> HashMap<String, String> {
        let mut constraints = HashMap::new();
        for owner in self.class_mro(class_name) {
            if let Some(class_def) = self.classes.get(&owner) {
                for (attr_name, tc) in &class_def.attribute_types {
                    constraints
                        .entry(attr_name.clone())
                        .or_insert_with(|| tc.clone());
                }
            }
        }
        constraints
    }

    fn enforce_attribute_where_constraints(
        &mut self,
        class_name: &str,
        class_attrs_info: &[ClassAttributeDef],
        attrs: &HashMap<String, Value>,
    ) -> Result<(), RuntimeError> {
        let type_constraints = self.collect_attribute_type_constraints(class_name);
        for (attr_name, _is_public, _default, _is_rw, _is_required, sigil, where_constraint) in
            class_attrs_info
        {
            if let Some(constraint) = type_constraints.get(attr_name)
                && (constraint.starts_with(char::is_uppercase) || constraint.starts_with("::"))
                && let Some(value) = attrs.get(attr_name)
                && !matches!(value, Value::Nil)
            {
                // For array/hash attributes, the type constraint applies to
                // elements/values, not to the container itself.
                if *sigil == '@' || *sigil == '%' {
                    // Skip container-level type check for @ and % attributes;
                    // element-level checking happens at assignment time.
                } else if !self.type_matches_value(constraint, value) {
                    return Err(RuntimeError::new(format!(
                        "Type check failed in assignment to $!{}; expected {}, got {}",
                        attr_name,
                        constraint,
                        super::value_type_name(value)
                    )));
                }
            }
            let Some(pred) = where_constraint else {
                continue;
            };
            let Some(value) = attrs.get(attr_name) else {
                continue;
            };
            if matches!(value, Value::Nil) {
                continue;
            }
            if !self.check_attribute_where_constraint(pred, value) {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to $!{}; where constraint failed",
                    attr_name
                )));
            }
        }
        Ok(())
    }

    /// Enforce type smiley constraints (`:U`, `:D`) on attribute values during `.new`.
    fn enforce_attribute_smiley_constraints(
        &mut self,
        class_name: &str,
        attrs: &HashMap<String, Value>,
    ) -> Result<(), RuntimeError> {
        // Collect smileys and required status from this class and all parent classes in the MRO
        let mut smileys: HashMap<String, String> = HashMap::new();
        let mut required_attrs: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        let mro = self.class_mro(class_name);
        for mro_class in &mro {
            if let Some(class_def) = self.classes.get(mro_class) {
                for (attr_name, smiley) in &class_def.attribute_smileys {
                    smileys
                        .entry(attr_name.clone())
                        .or_insert_with(|| smiley.clone());
                }
                for (attr_name, _, _, _, is_required, _, _) in &class_def.attributes {
                    if is_required.is_some() {
                        required_attrs.insert(attr_name.clone());
                    }
                }
            }
        }

        for (attr_name, smiley) in &smileys {
            // Skip smiley check for required attributes — the required check
            // should take priority (it fires separately and produces a better error)
            if required_attrs.contains(attr_name) {
                continue;
            }
            let Some(value) = attrs.get(attr_name) else {
                continue;
            };
            match smiley.as_str() {
                "U" => {
                    // :U means the value must be undefined (type object)
                    if super::types::value_is_defined(value) {
                        let mut ex_attrs = HashMap::new();
                        ex_attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Type check failed in default value of attribute $!{}; expected {}, got {}",
                                attr_name,
                                self.classes.get(class_name)
                                    .and_then(|cd| cd.attribute_types.get(attr_name))
                                    .map(|t| format!("{}:U", t))
                                    .unwrap_or_else(|| "Any:U".to_string()),
                                super::value_type_name(value),
                            )),
                        );
                        let ex = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Attribute::Default"),
                            ex_attrs,
                        );
                        let mut err = RuntimeError::new(format!(
                            "Type check failed in default value of attribute $!{}",
                            attr_name
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
                "D" => {
                    // :D means the value must be defined
                    if !super::types::value_is_defined(value) {
                        let mut ex_attrs = HashMap::new();
                        ex_attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Type check failed in default value of attribute $!{}; expected {}, got {}",
                                attr_name,
                                self.classes.get(class_name)
                                    .and_then(|cd| cd.attribute_types.get(attr_name))
                                    .map(|t| format!("{}:D", t))
                                    .unwrap_or_else(|| "Any:D".to_string()),
                                super::value_type_name(value),
                            )),
                        );
                        let ex = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Attribute::Default"),
                            ex_attrs,
                        );
                        let mut err = RuntimeError::new(format!(
                            "Type check failed in default value of attribute $!{}",
                            attr_name
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
                _ => {} // "_" or anything else: no constraint
            }
        }
        Ok(())
    }

    /// Construct a Proxy subclass instance: extracts FETCH/STORE from args,
    /// initializes subclass attributes (with defaults), and returns a Proxy
    /// with shared mutable subclass attrs.
    fn construct_proxy_subclass(
        &mut self,
        class_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut fetcher = Value::Nil;
        let mut storer = Value::Nil;
        let mut extra_attrs = HashMap::new();

        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "FETCH" => fetcher = *value.clone(),
                    "STORE" => storer = *value.clone(),
                    _ => {
                        extra_attrs.insert(key.clone(), *value.clone());
                    }
                }
            }
        }

        // Initialize subclass attributes with defaults
        let class_attrs_info = self.collect_class_attributes(class_name);
        for (attr_name, _is_public, default_expr, _is_rw, _is_required, sigil, _) in
            &class_attrs_info
        {
            if !extra_attrs.contains_key(attr_name) {
                let default_val = if let Some(expr) = default_expr {
                    let result = self.eval_block_value(&[crate::ast::Stmt::Expr(expr.clone())])?;
                    Self::coerce_attr_value_by_sigil(result, *sigil)
                } else {
                    match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash(HashMap::new()),
                        _ => Value::Nil,
                    }
                };
                extra_attrs.insert(attr_name.clone(), default_val);
            }
        }
        self.enforce_attribute_where_constraints(class_name, &class_attrs_info, &extra_attrs)?;

        let subclass_attrs = std::sync::Arc::new(std::sync::Mutex::new(extra_attrs));
        Ok(Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass: Some((Symbol::intern(class_name), subclass_attrs)),
            decontainerized: false,
        })
    }

    /// Call a Date formatter and store the rendered result in `__formatter_rendered`.
    pub(super) fn render_date_formatter(
        &mut self,
        date: Value,
        formatter_value: Value,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            class_name,
            ref attributes,
            id,
        } = date
        {
            let saved_env = self.env().clone();
            let saved_readonly = self.save_readonly_vars();
            let rendered = self
                .eval_call_on_value(formatter_value, vec![date.clone()])?
                .to_string_value();
            *self.env_mut() = saved_env;
            self.restore_readonly_vars(saved_readonly);
            let mut updated = (**attributes).clone();
            updated.insert("__formatter_rendered".to_string(), Value::str(rendered));
            Ok(Value::make_instance_with_id(class_name, updated, id))
        } else {
            Ok(date)
        }
    }

    /// Create a Collation instance with the given level settings.
    pub(super) fn make_collation_instance(
        primary: i64,
        secondary: i64,
        tertiary: i64,
        quaternary: i64,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("primary".to_string(), Value::Int(primary));
        attrs.insert("secondary".to_string(), Value::Int(secondary));
        attrs.insert("tertiary".to_string(), Value::Int(tertiary));
        attrs.insert("quaternary".to_string(), Value::Int(quaternary));
        Value::make_instance(Symbol::intern("Collation"), attrs)
    }
}
