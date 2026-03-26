use super::*;

pub(in crate::runtime) fn positional_values_from_unpack_target(value: &Value) -> Vec<Value> {
    match value {
        Value::Capture { positional, .. } => positional.clone(),
        other => crate::runtime::value_to_list(other),
    }
}

pub(in crate::runtime) fn varref_from_value(value: &Value) -> Option<(String, Value)> {
    indexed_varref_from_value(value).map(|(name, inner, _)| (name, inner))
}

pub(in crate::runtime) fn indexed_varref_from_value(
    value: &Value,
) -> Option<(String, Value, Option<usize>)> {
    if let Value::Capture { positional, named } = value
        && positional.is_empty()
        && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
        && let Some(inner) = named.get("__mutsu_varref_value")
    {
        let source_index = match named.get("__mutsu_varref_index") {
            Some(Value::Int(i)) if *i >= 0 => Some(*i as usize),
            _ => None,
        };
        return Some((name.to_string(), inner.clone(), source_index));
    }
    None
}

/// Wrap an integer value to fit within a native integer type's range
/// for function parameter binding.
/// For full-width native types (int/int64/uint/uint64), out-of-range values
/// cause an error (cannot unbox too-wide bigint into native integer).
pub(in crate::runtime) fn wrap_native_int_for_binding(
    constraint: &str,
    val: Value,
) -> Result<Value, crate::value::RuntimeError> {
    use crate::runtime::native_types;
    use num_bigint::BigInt as NumBigInt;
    use num_traits::ToPrimitive;

    let (base, _) = strip_type_smiley(constraint);
    if !native_types::is_native_int_type(base) {
        return Ok(val);
    }
    let big_val = match &val {
        Value::Int(n) => NumBigInt::from(*n),
        Value::BigInt(n) => (**n).clone(),
        _ => return Ok(val),
    };
    if native_types::is_in_native_range(base, &big_val) {
        return Ok(val);
    }
    // Full-width native types don't wrap — they should throw on overflow.
    if matches!(base, "int" | "int64" | "uint" | "uint64") {
        return Err(crate::value::RuntimeError::new(format!(
            "Cannot unbox {} bit wide bigint into native integer",
            big_val.bits()
        )));
    }
    let wrapped = native_types::wrap_native_int(base, &big_val);
    Ok(wrapped
        .to_i64()
        .map(Value::Int)
        .unwrap_or_else(|| Value::bigint(wrapped)))
}

pub(in crate::runtime) fn unwrap_varref_value(value: Value) -> Value {
    if let Some((_, inner)) = varref_from_value(&value) {
        inner
    } else {
        value
    }
}

/// Recursively flatten a list of values for `*@` (flattening slurpy) parameter binding.
/// Non-itemized Array/List elements are flattened recursively; itemized containers
/// (`$(...)`, `$[...]`) are preserved as single elements.
pub(in crate::runtime) fn flatten_into_slurpy(values: &[Value], out: &mut Vec<Value>) {
    for val in values {
        match val {
            Value::Array(arr, kind) if !kind.is_itemized() => {
                flatten_into_slurpy(arr, out);
            }
            Value::Seq(items) | Value::Slip(items) => {
                flatten_into_slurpy(items, out);
            }
            Value::Range(a, b) => {
                if *b >= *a {
                    for i in *a..=*b {
                        out.push(Value::Int(i));
                    }
                }
            }
            Value::RangeExcl(a, b) => {
                if *b > *a {
                    for i in *a..*b {
                        out.push(Value::Int(i));
                    }
                }
            }
            Value::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                if *b >= start {
                    for i in start..=*b {
                        out.push(Value::Int(i));
                    }
                }
            }
            Value::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                if *b > start {
                    for i in start..*b {
                        out.push(Value::Int(i));
                    }
                }
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let a = crate::runtime::to_int(start);
                let b = crate::runtime::to_int(end);
                let s = if *excl_start { a + 1 } else { a };
                let e = if *excl_end { b } else { b + 1 };
                for i in s..e {
                    out.push(Value::Int(i));
                }
            }
            other => {
                out.push(other.clone());
            }
        }
    }
}

pub(in crate::runtime) fn make_varref_value(
    name: String,
    value: Value,
    source_index: Option<usize>,
) -> Value {
    let mut named = std::collections::HashMap::new();
    named.insert("__mutsu_varref_name".to_string(), Value::str(name));
    named.insert("__mutsu_varref_value".to_string(), value);
    if let Some(i) = source_index {
        named.insert("__mutsu_varref_index".to_string(), Value::Int(i as i64));
    }
    Value::Capture {
        positional: Vec::new(),
        named,
    }
}

pub(in crate::runtime) fn sigilless_alias_key(name: &str) -> String {
    format!("__mutsu_sigilless_alias::{}", name)
}

pub(in crate::runtime) fn sigilless_readonly_key(name: &str) -> String {
    format!("__mutsu_sigilless_readonly::{}", name)
}

pub(in crate::runtime) fn named_values_from_unpack_target(
    value: &Value,
) -> std::collections::HashMap<String, Value> {
    match value {
        Value::Capture { named, .. } => named.clone(),
        Value::Hash(map) => (**map).clone(),
        Value::Pair(key, val) => {
            let mut out = std::collections::HashMap::new();
            out.insert(key.clone(), *val.clone());
            out.insert("key".to_string(), Value::str(key.clone()));
            out.insert("value".to_string(), *val.clone());
            out
        }
        Value::Instance { attributes, .. } => (**attributes).clone(),
        _ => std::collections::HashMap::new(),
    }
}

pub(in crate::runtime) fn extract_named_from_unpack_target(
    interpreter: &mut Interpreter,
    value: &Value,
    name: &str,
) -> Option<Value> {
    let named = named_values_from_unpack_target(value);
    let lookup_name = name
        .strip_prefix('$')
        .or_else(|| name.strip_prefix('@'))
        .or_else(|| name.strip_prefix('%'))
        .unwrap_or(name);
    if let Some(v) = named.get(name) {
        return Some(v.clone());
    }
    if let Some(v) = named.get(lookup_name) {
        return Some(v.clone());
    }
    interpreter
        .call_method_with_values(value.clone(), name, Vec::new())
        .ok()
}

pub(in crate::runtime) fn sub_signature_matches_value(
    interpreter: &mut Interpreter,
    sub_params: &[ParamDef],
    value: &Value,
) -> bool {
    let positional = positional_values_from_unpack_target(value);
    let mut positional_idx = 0usize;
    for pd in sub_params {
        if pd.slurpy {
            continue;
        }
        let mut candidate = if pd.named {
            extract_named_from_unpack_target(interpreter, value, &pd.name)
        } else if positional_idx < positional.len() {
            let v = positional[positional_idx].clone();
            positional_idx += 1;
            Some(v)
        } else {
            None
        };
        if candidate.is_none()
            && let Some(default) = &pd.default
        {
            candidate = interpreter
                .eval_block_value(&[Stmt::Expr(default.clone())])
                .ok();
        }
        let Some(candidate) = candidate else {
            // Optional params are OK without a value
            if pd.optional_marker || pd.default.is_some() {
                continue;
            }
            return false;
        };
        if let Some(constraint) = &pd.type_constraint
            && !interpreter.type_matches_value(constraint, &candidate)
        {
            return false;
        }
        // Implicit Any constraint: untyped $ parameters reject Junction type objects
        if pd.type_constraint.is_none()
            && !pd.name.starts_with('@')
            && !pd.name.starts_with('%')
            && !pd.name.starts_with('&')
            && !pd.slurpy
            && !interpreter.type_matches_value("Any", &candidate)
        {
            return false;
        }
        if let Some(sub) = &pd.sub_signature
            && !sub_signature_matches_value(interpreter, sub, &candidate)
        {
            return false;
        }
    }
    // If there are unconsumed positional elements and no slurpy param, the match fails
    let has_slurpy = sub_params.iter().any(|p| p.slurpy);
    if !has_slurpy && positional_idx < positional.len() {
        return false;
    }
    true
}

/// Bind a named parameter's sub-signature for renaming (e.g., `:a(:$b)`).
/// Unlike destructuring, the value is bound directly to each inner param.
pub(in crate::runtime) fn bind_named_rename_sub_signature(
    interpreter: &mut Interpreter,
    sub_params: &[ParamDef],
    value: &Value,
) -> Result<(), RuntimeError> {
    for sub_pd in sub_params {
        if sub_pd.slurpy {
            continue;
        }
        let bind_name = &sub_pd.name;
        if bind_name.is_empty() {
            continue;
        }
        if let Some(tc) = &sub_pd.type_constraint
            && !interpreter.type_matches_value(tc, value)
        {
            return Err(RuntimeError::new(format!(
                "Type check failed in binding to parameter '{}'; expected {}, got {}",
                bind_name,
                tc,
                crate::runtime::value_type_name(value)
            )));
        }
        // Sigil-based type check: %param requires Associative, @param requires Positional
        if bind_name.starts_with('%') && !matches!(value, Value::Hash(..)) {
            return Err(RuntimeError::new(format!(
                "Type check failed in binding to parameter '{}'; expected Associative, got {}",
                bind_name,
                crate::runtime::value_type_name(value)
            )));
        }
        if bind_name.starts_with('@') && !matches!(value, Value::Array(..) | Value::Nil) {
            return Err(RuntimeError::new(format!(
                "Type check failed in binding to parameter '{}'; expected Positional, got {}",
                bind_name,
                crate::runtime::value_type_name(value)
            )));
        }
        interpreter.bind_param_value(bind_name, value.clone());
        interpreter.set_var_type_constraint(bind_name, sub_pd.type_constraint.clone());
    }
    Ok(())
}

pub(in crate::runtime) fn bind_sub_signature_from_value(
    interpreter: &mut Interpreter,
    sub_params: &[ParamDef],
    value: &Value,
) -> Result<(), RuntimeError> {
    let positional = positional_values_from_unpack_target(value);
    let mut nested_positional_idx = 0usize;
    for sub_pd in sub_params {
        if sub_pd.slurpy {
            if sub_pd.name.starts_with('%') {
                let named = named_values_from_unpack_target(value);
                if !sub_pd.name.is_empty() {
                    interpreter
                        .env
                        .insert(sub_pd.name.clone(), Value::hash(named));
                }
            } else if sub_pd.name.starts_with('@')
                || sub_pd.name.starts_with('$')
                || !sub_pd.name.is_empty()
            {
                // Slurpy *@rest or *$rest: collect remaining positional values
                let remaining: Vec<Value> = positional[nested_positional_idx..].to_vec();
                nested_positional_idx = positional.len();
                if !sub_pd.name.is_empty() {
                    interpreter
                        .env
                        .insert(sub_pd.name.clone(), Value::array(remaining));
                }
            }
            continue;
        }
        let mut candidate = if sub_pd.named {
            extract_named_from_unpack_target(interpreter, value, &sub_pd.name)
        } else if nested_positional_idx < positional.len() {
            let v = positional[nested_positional_idx].clone();
            nested_positional_idx += 1;
            Some(v)
        } else {
            None
        };
        if candidate.is_none()
            && let Some(default_expr) = &sub_pd.default
        {
            candidate = Some(interpreter.eval_block_value(&[Stmt::Expr(default_expr.clone())])?);
        }
        let Some(mut candidate) = candidate else {
            // If the param is required (not optional, no default), error
            if !sub_pd.optional_marker && sub_pd.default.is_none() {
                return Err(RuntimeError::new(
                    "Too few positional arguments in sub-signature binding".to_string(),
                ));
            }
            // Explicitly bind optional params to their type object (Any)
            // so that values from an outer scope don't leak into recursive calls.
            if !sub_pd.name.is_empty() {
                let default_val = if sub_pd.name.starts_with('@') {
                    Value::array(vec![])
                } else if sub_pd.name.starts_with('%') {
                    Value::hash(std::collections::HashMap::new())
                } else {
                    Value::Nil
                };
                interpreter.env.insert(sub_pd.name.clone(), default_val);
            }
            continue;
        };
        if let Value::Pair(key, inner) = &candidate {
            let bind_name = sub_pd
                .name
                .strip_prefix('$')
                .or_else(|| sub_pd.name.strip_prefix('@'))
                .or_else(|| sub_pd.name.strip_prefix('%'))
                .unwrap_or(sub_pd.name.as_str());
            if sub_pd.named || bind_name == key {
                candidate = *inner.clone();
            }
        }
        if let Some(constraint) = &sub_pd.type_constraint {
            if let Some((_target, source)) = parse_coercion_type(constraint) {
                if let Some(src) = source
                    && !interpreter.type_matches_value(src, &candidate)
                {
                    return Err(RuntimeError::new(format!(
                        "X::TypeCheck::Argument: Type check failed for {}: expected {}, got {}",
                        sub_pd.name,
                        constraint,
                        crate::runtime::value_type_name(&candidate)
                    )));
                }
                candidate = interpreter.try_coerce_value_for_constraint(constraint, candidate)?;
            } else if !interpreter.type_matches_value(constraint, &candidate) {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Argument: Type check failed for {}: expected {}, got {}",
                    sub_pd.name,
                    constraint,
                    crate::runtime::value_type_name(&candidate)
                )));
            }
        }
        let bind_alias_name = !(sub_pd.named && sub_pd.sub_signature.is_some());
        if !sub_pd.name.is_empty() && bind_alias_name {
            interpreter
                .env
                .insert(sub_pd.name.clone(), candidate.clone());
        }
        if let Some(nested) = &sub_pd.sub_signature {
            bind_sub_signature_from_value(interpreter, nested, &candidate)?;
        }
    }
    // If there are unconsumed positional elements and no slurpy param, error
    let has_slurpy = sub_params.iter().any(|p| p.slurpy);
    let has_positional_params = sub_params.iter().any(|p| !p.named && !p.slurpy);
    if has_positional_params && !has_slurpy && nested_positional_idx < positional.len() {
        return Err(RuntimeError::new(
            "Too many positional arguments in sub-signature binding".to_string(),
        ));
    }
    Ok(())
}

pub(in crate::runtime) fn sub_signature_target_from_remaining_args(args: &[Value]) -> Value {
    if args.len() == 1 {
        return args[0].clone();
    }
    Value::Capture {
        positional: args.to_vec(),
        named: std::collections::HashMap::new(),
    }
}

pub(in crate::runtime) fn callable_signature_info(
    interpreter: &mut Interpreter,
    value: &Value,
) -> Option<crate::value::signature::SigInfo> {
    use crate::value::signature::param_defs_to_sig_info;

    let callable = match value {
        Value::WeakSub(weak) => weak.upgrade().map(Value::Sub)?,
        other => other.clone(),
    };
    if !interpreter.type_matches_value("Callable", &callable) {
        return None;
    }

    match &callable {
        Value::Sub(data) => {
            let return_type = interpreter
                .callable_return_type(&callable)
                .or_else(|| interpreter.routine_return_spec_by_name(&data.name.resolve()));
            Some(param_defs_to_sig_info(&data.param_defs, return_type))
        }
        Value::Routine { name, .. } => {
            let (params, param_defs) = interpreter.callable_signature(&callable);
            let defs = if !param_defs.is_empty() {
                param_defs
            } else {
                params
                    .into_iter()
                    .map(|name| ParamDef {
                        name,
                        default: None,
                        multi_invocant: true,
                        required: true,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        onearg: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                    })
                    .collect::<Vec<_>>()
            };
            let return_type = interpreter.routine_return_spec_by_name(&name.resolve());
            Some(param_defs_to_sig_info(&defs, return_type))
        }
        _ => {
            let (params, param_defs) = interpreter.callable_signature(&callable);
            let defs = if !param_defs.is_empty() {
                param_defs
            } else {
                params
                    .into_iter()
                    .map(|name| ParamDef {
                        name,
                        default: None,
                        multi_invocant: true,
                        required: true,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        onearg: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        is_invocant: false,
                        shape_constraints: None,
                    })
                    .collect::<Vec<_>>()
            };
            Some(param_defs_to_sig_info(
                &defs,
                interpreter.callable_return_type(&callable),
            ))
        }
    }
}

pub(in crate::runtime) fn code_signature_matches_value(
    interpreter: &mut Interpreter,
    expected_params: &[ParamDef],
    expected_return_type: &Option<String>,
    value: &Value,
) -> bool {
    use crate::value::signature::{param_defs_to_sig_info, signature_smartmatch};

    fn resolve_captured_constraint(interpreter: &Interpreter, constraint: &str) -> String {
        if let Some(captured) = constraint.strip_prefix("::")
            && let Some(Value::Package(name)) = interpreter.env.get(captured)
        {
            return name.resolve();
        }
        if let Some(Value::Package(name)) = interpreter.env.get(constraint) {
            return name.resolve();
        }
        constraint.to_string()
    }

    fn specialize_code_signature_params(
        interpreter: &Interpreter,
        params: &[ParamDef],
    ) -> Vec<ParamDef> {
        params
            .iter()
            .map(|p| {
                let mut next = p.clone();
                if let Some(tc) = &next.type_constraint {
                    next.type_constraint = Some(resolve_captured_constraint(interpreter, tc));
                }
                next
            })
            .collect()
    }

    let Some(actual_info) = callable_signature_info(interpreter, value) else {
        return false;
    };
    let specialized_expected = specialize_code_signature_params(interpreter, expected_params);
    let specialized_return = expected_return_type
        .as_ref()
        .map(|rt| resolve_captured_constraint(interpreter, rt));
    let expected_info = param_defs_to_sig_info(&specialized_expected, specialized_return);
    // Function-type compatibility: candidate callable must be at least as general
    // as the required signature to safely accept all required calls.
    signature_smartmatch(&actual_info, &expected_info)
}
