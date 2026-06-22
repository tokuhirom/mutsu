use super::*;

pub(in crate::runtime) fn positional_values_from_unpack_target(value: &Value) -> Vec<Value> {
    // A variable passed by reference (e.g. `f(@a)` / `f($items)`) arrives as a
    // varref Capture wrapping the real value; unwrap before destructuring.
    if let Some((_, inner)) = varref_from_value(value) {
        return positional_values_from_unpack_target(&inner);
    }
    match value {
        Value::Capture { positional, .. } => (**positional).clone(),
        // For sub-signature destructuring, a list is always taken apart
        // element-wise even when itemized — e.g. `$(3, 4)` or an array variable
        // bound to a single destructuring positional via the single-argument
        // rule. `value_to_list` would otherwise return an itemized list as a
        // single opaque element, which fails the destructuring arity check.
        Value::Array(data, _) => data.items.clone(),
        Value::Seq(items) | Value::Slip(items) => (**items).clone(),
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
    // Type objects cannot be unboxed to native types
    if let Value::Package(pkg_name) = &val {
        return Err(crate::value::RuntimeError::new(format!(
            "Cannot unbox a type object ({}) to {}.",
            pkg_name.resolve(),
            base
        )));
    }
    let big_val = match &val {
        Value::Int(n) => NumBigInt::from(*n),
        Value::BigInt(n) => (**n).clone(),
        _ => return Ok(val),
    };
    if native_types::is_in_native_range(base, &big_val) {
        return Ok(val);
    }
    // Full-width signed native types don't wrap — they should throw on overflow.
    if matches!(base, "int" | "int64") {
        return Err(crate::value::RuntimeError::new(format!(
            "Cannot unbox {} bit wide bigint into native integer",
            big_val.bits()
        )));
    }
    // Full-width unsigned types: positive overflow throws, negative values wrap.
    if matches!(base, "uint" | "uint64") && big_val > NumBigInt::from(0u64) {
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

pub(crate) fn unwrap_varref_value(value: Value) -> Value {
    if let Some((_, inner)) = varref_from_value(&value) {
        inner
    } else {
        value
    }
}

/// Upper bound on how many elements an infinite range contributes to a slurpy.
/// Mirrors `coerce_to_array`'s `MAX_ARRAY_EXPAND`: binding `1..*` to a `*@`/`+@`
/// slurpy must not loop forever (`for i in a..=i64::MAX`). This caps it to a
/// finite prefix instead of hanging. (A truly lazy slurpy binding is the goal of
/// the lazy-array campaign — see `docs/lazy-arrays.md` Slice L4.)
const MAX_SLURPY_RANGE_EXPAND: i64 = 100_000;

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
                let end = (*b).min(a.saturating_add(MAX_SLURPY_RANGE_EXPAND));
                if end >= *a {
                    for i in *a..=end {
                        out.push(Value::Int(i));
                    }
                }
            }
            Value::RangeExcl(a, b) => {
                let end = (*b).min(a.saturating_add(MAX_SLURPY_RANGE_EXPAND));
                if end > *a {
                    for i in *a..end {
                        out.push(Value::Int(i));
                    }
                }
            }
            Value::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(MAX_SLURPY_RANGE_EXPAND));
                if end >= start {
                    for i in start..=end {
                        out.push(Value::Int(i));
                    }
                }
            }
            Value::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(MAX_SLURPY_RANGE_EXPAND));
                if end > start {
                    for i in start..end {
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
                let raw_e = if *excl_end { b } else { b + 1 };
                let e = raw_e.min(s.saturating_add(MAX_SLURPY_RANGE_EXPAND));
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

pub(crate) fn make_varref_value(name: String, value: Value, source_index: Option<usize>) -> Value {
    let mut named = std::collections::HashMap::new();
    named.insert("__mutsu_varref_name".to_string(), Value::str(name));
    named.insert("__mutsu_varref_value".to_string(), value);
    if let Some(i) = source_index {
        named.insert("__mutsu_varref_index".to_string(), Value::Int(i as i64));
    }
    Value::capture(Vec::new(), named)
}

pub(in crate::runtime) fn sigilless_alias_key(name: &str) -> String {
    format!("__mutsu_sigilless_alias::{}", name)
}

pub(in crate::runtime) fn sigilless_readonly_key(name: &str) -> String {
    format!("__mutsu_sigilless_readonly::{}", name)
}

/// Collect the `Pair`/`ValuePair` elements of a list into a named map. Used
/// when a list of pairs is destructured by named sub-signature params.
fn pairs_in_list_to_named(items: &[Value]) -> std::collections::HashMap<String, Value> {
    let mut out = std::collections::HashMap::new();
    for item in items {
        match item {
            Value::Pair(key, val) => {
                out.insert(key.clone(), val.as_ref().clone());
            }
            Value::ValuePair(key, val) => {
                out.insert(key.to_string_value(), val.as_ref().clone());
            }
            _ => {}
        }
    }
    out
}

pub(in crate::runtime) fn named_values_from_unpack_target(
    value: &Value,
) -> std::collections::HashMap<String, Value> {
    // Unwrap a varref Capture (a by-reference variable argument) to the real
    // value before extracting named entries.
    if let Some((_, inner)) = varref_from_value(value) {
        return named_values_from_unpack_target(&inner);
    }
    match value {
        Value::Capture { named, .. } => (**named).clone(),
        Value::Hash(map) => map.map.clone(),
        Value::Pair(key, val) => {
            let mut out = std::collections::HashMap::new();
            out.insert(key.clone(), *val.clone());
            out.insert("key".to_string(), Value::str(key.clone()));
            out.insert("value".to_string(), *val.clone());
            out
        }
        // A list of Pairs (e.g. `(a => 1, b => 2)`) destructured by named
        // params `(:$a, :$b)` binds each pair's key to its value.
        Value::Array(data, _) => pairs_in_list_to_named(&data.items),
        Value::Seq(items) | Value::Slip(items) => pairs_in_list_to_named(items),
        Value::Instance { attributes, .. } => attributes.to_map(),
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

/// Evaluate a subsignature parameter's `where` constraint against a candidate
/// value during multi dispatch.  Mirrors the top-level positional `where`
/// handling in `args_match_param_types`.
fn where_constraint_matches(
    interpreter: &mut Interpreter,
    where_expr: &Expr,
    pd: &ParamDef,
    candidate: &Value,
) -> bool {
    let saved = interpreter.env.clone();
    interpreter.env.insert("_".to_string(), candidate.clone());
    if !pd.name.is_empty() {
        interpreter.env.insert(pd.name.clone(), candidate.clone());
    }
    let ok = match where_expr {
        Expr::AnonSub { body, .. } => interpreter
            .eval_block_value(body)
            .map(|v| v.truthy())
            .unwrap_or(false),
        Expr::MethodCall { target, .. } if matches!(target.as_ref(), Expr::Var(name) if name == "_") => {
            interpreter
                .eval_block_value(&[Stmt::Expr(where_expr.clone())])
                .map(|v| v.truthy())
                .unwrap_or(false)
        }
        expr => interpreter
            .eval_block_value(&[Stmt::Expr(expr.clone())])
            .map(|v| interpreter.smart_match(candidate, &v))
            .unwrap_or(false),
    };
    interpreter.env = saved;
    ok
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
            // Optional params are OK without a value.  Named parameters are
            // optional unless explicitly marked required (`:$x!`), so a missing
            // optional named arg must not fail the match.
            if pd.optional_marker || pd.default.is_some() || (pd.named && !pd.required) {
                continue;
            }
            return false;
        };
        if let Some(constraint) = &pd.type_constraint
            && !interpreter.type_matches_value(constraint, &candidate)
        {
            return false;
        }
        // Sigil-based type constraints: an `@`-param requires a Positional
        // argument and a `%`-param requires an Associative one.  Without this a
        // scalar would wrongly bind to `@x`/`%h` during multi dispatch.
        if !pd.slurpy {
            if pd.name.starts_with('@')
                && !matches!(candidate, Value::Nil)
                && !interpreter.type_matches_value("Positional", &candidate)
            {
                return false;
            }
            if pd.name.starts_with('%')
                && !matches!(candidate, Value::Nil)
                && !interpreter.type_matches_value("Associative", &candidate)
            {
                return false;
            }
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
        if let Some(where_expr) = &pd.where_constraint
            && !where_constraint_matches(interpreter, where_expr, pd, &candidate)
        {
            return false;
        }
    }
    // If there are unconsumed positional elements and no slurpy param, the match fails
    let has_slurpy = sub_params.iter().any(|p| !p.named && p.slurpy);
    if !has_slurpy && positional_idx < positional.len() {
        return false;
    }
    // Reject unexpected named arguments (for the multi-dispatch case where the
    // value is a Capture).  A named slurpy (`*%h`) accepts any named argument.
    if let Value::Capture { named, .. } = value {
        let has_named_slurpy = sub_params
            .iter()
            .any(|p| p.slurpy && (p.named || p.name.starts_with('%')));
        if !has_named_slurpy {
            for key in named.keys() {
                if key.starts_with("__mutsu_") {
                    continue;
                }
                let consumed = sub_params.iter().any(|p| {
                    p.named && p.name.trim_start_matches(|c: char| "$@%&".contains(c)) == key
                });
                if !consumed {
                    return false;
                }
            }
        }
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
            return Err(RuntimeError::typecheck_binding_parameter(
                bind_name,
                tc,
                crate::runtime::value_type_name(value),
                None,
            ));
        }
        // Sigil-based type check: %param requires Associative, @param requires Positional
        if bind_name.starts_with('%') && !matches!(value, Value::Hash(..)) {
            return Err(RuntimeError::typecheck_binding_parameter(
                bind_name,
                "Associative",
                crate::runtime::value_type_name(value),
                None,
            ));
        }
        if bind_name.starts_with('@') && !matches!(value, Value::Array(..) | Value::Nil) {
            return Err(RuntimeError::typecheck_binding_parameter(
                bind_name,
                "Positional",
                crate::runtime::value_type_name(value),
                None,
            ));
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
            if !sub_pd.optional_marker && sub_pd.default.is_none() && !sub_pd.named {
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
            // Only unwrap a positional `key => val` pair whose key matches this
            // positional parameter's name. A NAMED sub-param's candidate already
            // came from `extract_named_from_unpack_target` (the source pair's
            // `.value`), so unwrapping it again would wrongly strip a value that
            // is itself a `Value::Pair` — e.g. `Pair :value((...))` nested
            // destructuring or `:value($v)` where the value is a Pair (the
            // `ValuePair` form already skips this unwrap; this aligns the
            // string-key `Value::Pair` form with it).
            if !sub_pd.named && bind_name == key {
                candidate = *inner.clone();
            }
        }
        if let Some(constraint) = &sub_pd.type_constraint {
            if let Some((target, source)) = parse_coercion_type(constraint) {
                // A `T(S)` parameter accepts a value already of type `T` as well as
                // an `S` (coerced via `.T`); only reject one that matches neither.
                if let Some(src) = source
                    && !interpreter.type_matches_value(src, &candidate)
                    && !interpreter.type_matches_value(target, &candidate)
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
    // A single argument unpacks directly (e.g. `|(Int $x)` matching one value,
    // or `|(Pair $p (:$key, :$value))` matching one Pair whose `.key`/`.value`
    // accessors must remain reachable).  Only when multiple arguments remain do
    // we build a Capture, separating named (Pair) arguments into the named slot
    // so that subsignature named parameters can find them.
    if args.len() == 1 {
        return args[0].clone();
    }
    let mut positional = Vec::new();
    let mut named = std::collections::HashMap::new();
    for arg in args {
        match arg {
            Value::Pair(key, val) => {
                named.insert(key.clone(), val.as_ref().clone());
            }
            Value::ValuePair(key, val) => {
                named.insert(key.to_string_value(), val.as_ref().clone());
            }
            other => positional.push(other.clone()),
        }
    }
    Value::capture(positional, named)
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
