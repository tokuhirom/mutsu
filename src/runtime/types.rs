use super::*;
use crate::symbol::Symbol;

const TEST_CALLSITE_LINE_KEY: &str = "__mutsu_test_callsite_line";

#[inline]
fn is_internal_named_arg(arg: &Value) -> bool {
    match arg {
        Value::Pair(key, _) => key == TEST_CALLSITE_LINE_KEY,
        Value::ValuePair(key, _) => {
            matches!(key.as_ref(), Value::Str(s) if s.as_str() == TEST_CALLSITE_LINE_KEY)
        }
        _ => false,
    }
}

/// Parse a coercion type like "Int()" or "Int(Rat)".
/// Returns Some((target_type, optional_source_type)) if it's a coercion type.
fn parse_coercion_type(constraint: &str) -> Option<(&str, Option<&str>)> {
    if let Some(open) = constraint.find('(')
        && constraint.ends_with(')')
    {
        let target = &constraint[..open];
        let source = &constraint[open + 1..constraint.len() - 1];
        if source.is_empty() {
            Some((target, None))
        } else {
            Some((target, Some(source)))
        }
    } else {
        None
    }
}

fn language_version_is_6e_or_newer(version: &str) -> bool {
    version.starts_with("6.e")
}

fn predicate_requires_defined(predicate: &Expr) -> bool {
    match predicate {
        Expr::MethodCall {
            target, name, args, ..
        } => {
            args.is_empty()
                && name.resolve() == "defined"
                && matches!(target.as_ref(), Expr::Var(v) if v == "_")
        }
        Expr::AnonSub { body, .. } => {
            body.len() == 1
                && matches!(
                    &body[0],
                    Stmt::Expr(expr) if predicate_requires_defined(expr)
                )
        }
        _ => false,
    }
}

#[inline]
fn is_coercion_constraint(constraint: &str) -> bool {
    let bytes = constraint.as_bytes();
    bytes.last() == Some(&b')') && bytes.contains(&b'(') && !bytes.contains(&b'[')
}

pub(crate) fn coerce_impossible_error(target: &str, got: &Value) -> RuntimeError {
    let msg = format!(
        "Impossible coercion from '{}' into '{}': no acceptable coercion method found",
        super::value_type_name(got),
        target
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("from".to_string(), got.clone());
    attrs.insert("to".to_string(), Value::str(target.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Coerce::Impossible"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Coerce a value to the target type.
fn coerce_value(target: &str, value: Value) -> Value {
    let base_target = if target.ends_with(":D") || target.ends_with(":U") || target.ends_with(":_")
    {
        &target[..target.len() - 2]
    } else {
        target
    };
    match base_target {
        "Int" => match &value {
            Value::Int(_) => value,
            Value::Num(n) => Value::Int(*n as i64),
            Value::Rat(n, d) => Value::Int(if *d != 0 { n / d } else { 0 }),
            Value::Str(s) => Value::Int(s.parse::<i64>().unwrap_or(0)),
            Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
            _ => value,
        },
        "Num" => match &value {
            Value::Num(_) => value,
            Value::Int(n) => Value::Num(*n as f64),
            Value::Rat(n, d) => Value::Num(*n as f64 / *d as f64),
            Value::Str(s) => Value::Num(s.parse::<f64>().unwrap_or(0.0)),
            _ => value,
        },
        "Str" => Value::str(crate::runtime::utils::coerce_to_str(&value)),
        "Array" | "List" => crate::runtime::utils::coerce_to_array(value),
        "Hash" => crate::runtime::utils::coerce_to_hash(value),
        "Rat" => {
            match &value {
                Value::Rat(_, _) => value,
                Value::Int(n) => Value::Rat(*n, 1),
                Value::Num(n) => {
                    // Simple float to rat conversion
                    let denom = 1_000_000i64;
                    let numer = (*n * denom as f64) as i64;
                    let g = gcd_i64(numer.abs(), denom);
                    Value::Rat(numer / g, denom / g)
                }
                Value::Str(s) => {
                    if s.contains('.') {
                        let trimmed = s.trim();
                        let negative = trimmed.starts_with('-');
                        let abs_str = if negative { &trimmed[1..] } else { trimmed };
                        if let Some((int_part, frac_part)) = abs_str.split_once('.') {
                            let frac_digits = frac_part.len() as u32;
                            let denom = 10i64.pow(frac_digits);
                            let int_val = int_part.parse::<i64>().unwrap_or(0);
                            let frac_val = frac_part.parse::<i64>().unwrap_or(0);
                            let numer = int_val * denom + frac_val;
                            let numer = if negative { -numer } else { numer };
                            let g = gcd_i64(numer.abs(), denom);
                            Value::Rat(numer / g, denom / g)
                        } else {
                            value
                        }
                    } else {
                        let n = s.trim().parse::<i64>().unwrap_or(0);
                        Value::Rat(n, 1)
                    }
                }
                _ => value,
            }
        }
        "Bool" => Value::Bool(value.truthy()),
        _ => value,
    }
}

fn gcd_i64(a: i64, b: i64) -> i64 {
    if b == 0 { a } else { gcd_i64(b, a % b) }
}

fn positional_values_from_unpack_target(value: &Value) -> Vec<Value> {
    match value {
        Value::Capture { positional, .. } => positional.clone(),
        other => super::value_to_list(other),
    }
}

fn varref_from_value(value: &Value) -> Option<(String, Value)> {
    indexed_varref_from_value(value).map(|(name, inner, _)| (name, inner))
}

fn indexed_varref_from_value(value: &Value) -> Option<(String, Value, Option<usize>)> {
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

fn unwrap_varref_value(value: Value) -> Value {
    if let Some((_, inner)) = varref_from_value(&value) {
        inner
    } else {
        value
    }
}

/// Recursively flatten a list of values for `*@` (flattening slurpy) parameter binding.
/// Non-itemized Array/List elements are flattened recursively; itemized containers
/// (`$(...)`, `$[...]`) are preserved as single elements.
fn flatten_into_slurpy(values: &[Value], out: &mut Vec<Value>) {
    for val in values {
        match val {
            Value::Array(arr, kind) if !kind.is_itemized() => {
                flatten_into_slurpy(arr, out);
            }
            other => {
                out.push(other.clone());
            }
        }
    }
}

fn make_varref_value(name: String, value: Value, source_index: Option<usize>) -> Value {
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

fn sigilless_alias_key(name: &str) -> String {
    format!("__mutsu_sigilless_alias::{}", name)
}

fn sigilless_readonly_key(name: &str) -> String {
    format!("__mutsu_sigilless_readonly::{}", name)
}

fn named_values_from_unpack_target(value: &Value) -> std::collections::HashMap<String, Value> {
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

fn extract_named_from_unpack_target(
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

fn sub_signature_matches_value(
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
fn bind_named_rename_sub_signature(
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
                super::value_type_name(value)
            )));
        }
        // Sigil-based type check: %param requires Associative, @param requires Positional
        if bind_name.starts_with('%') && !matches!(value, Value::Hash(..)) {
            return Err(RuntimeError::new(format!(
                "Type check failed in binding to parameter '{}'; expected Associative, got {}",
                bind_name,
                super::value_type_name(value)
            )));
        }
        if bind_name.starts_with('@') && !matches!(value, Value::Array(..) | Value::Nil) {
            return Err(RuntimeError::new(format!(
                "Type check failed in binding to parameter '{}'; expected Positional, got {}",
                bind_name,
                super::value_type_name(value)
            )));
        }
        interpreter.bind_param_value(bind_name, value.clone());
        interpreter.set_var_type_constraint(bind_name, sub_pd.type_constraint.clone());
    }
    Ok(())
}

fn bind_sub_signature_from_value(
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
                        super::value_type_name(&candidate)
                    )));
                }
                candidate = interpreter.try_coerce_value_for_constraint(constraint, candidate)?;
            } else if !interpreter.type_matches_value(constraint, &candidate) {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Argument: Type check failed for {}: expected {}, got {}",
                    sub_pd.name,
                    constraint,
                    super::value_type_name(&candidate)
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

fn sub_signature_target_from_remaining_args(args: &[Value]) -> Value {
    if args.len() == 1 {
        return args[0].clone();
    }
    Value::Capture {
        positional: args.to_vec(),
        named: std::collections::HashMap::new(),
    }
}

fn callable_signature_info(
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

fn code_signature_matches_value(
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

/// Strip a type smiley suffix (:U, :D, :_) from a constraint string.
/// Returns (base_type, smiley) where smiley is Some(":U"), Some(":D"), Some(":_") or None.
pub(crate) fn strip_type_smiley(constraint: &str) -> (&str, Option<&str>) {
    if let Some(base) = constraint.strip_suffix(":U") {
        (base, Some(":U"))
    } else if let Some(base) = constraint.strip_suffix(":D") {
        (base, Some(":D"))
    } else if let Some(base) = constraint.strip_suffix(":_") {
        (base, Some(":_"))
    } else {
        (constraint, None)
    }
}

/// Check if a value is "defined" in the Raku sense.
/// Type objects (Package) are undefined; concrete values and instances are defined.
pub(crate) fn value_is_defined(value: &Value) -> bool {
    match value {
        Value::Nil | Value::Package(_) => false,
        Value::Slip(items) if items.is_empty() => false,
        Value::Instance { class_name, .. } if class_name == "Failure" => false,
        _ => true,
    }
}

impl Interpreter {
    fn resolve_sigilless_alias_source_name(&self, source_name: &str) -> String {
        let mut resolved = source_name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(resolved.clone()) {
            let key = sigilless_alias_key(&resolved);
            let Some(Value::Str(next)) = self.env.get(&key) else {
                break;
            };
            resolved = next.to_string();
        }
        resolved
    }

    /// Save the current readonly_vars set (call before function body execution).
    pub(crate) fn save_readonly_vars(&self) -> HashSet<String> {
        self.readonly_vars.clone()
    }

    /// Restore the readonly_vars set (call after function body execution).
    pub(crate) fn restore_readonly_vars(&mut self, saved: HashSet<String>) {
        self.readonly_vars = saved;
    }

    /// Get mutable access to readonly_vars set.
    pub(crate) fn readonly_vars_mut(&mut self) -> &mut HashSet<String> {
        &mut self.readonly_vars
    }

    /// Mark a variable as readonly.
    pub(crate) fn mark_readonly(&mut self, name: &str) {
        self.readonly_vars.insert(name.to_string());
    }

    /// Check if a variable is readonly and return an error if so.
    /// Returns Ok(()) if writable, Err with X::Multi::NoMatch for increment/decrement,
    /// or Err with a generic message for assignment.
    pub(crate) fn check_readonly_for_modify(&self, name: &str) -> Result<(), RuntimeError> {
        if self.readonly_vars.contains(name) {
            let msg = format!("Cannot assign to a readonly variable ({}) or a value", name);
            let mut err = RuntimeError::new(msg.clone());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Assignment::RO"),
                attrs,
            )));
            return Err(err);
        }
        Ok(())
    }

    /// Check if a variable is readonly for increment/decrement operations.
    /// Returns Err with X::Multi::NoMatch (like Raku's postfix:<++> dispatch failure).
    pub(crate) fn check_readonly_for_increment(&self, name: &str) -> Result<(), RuntimeError> {
        if self.readonly_vars.contains(name) {
            let msg = format!(
                "Cannot resolve caller postfix:<++>({}); the parameter requires mutable arguments",
                name
            );
            let mut err = RuntimeError::new(msg.clone());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Multi::NoMatch"),
                attrs,
            )));
            return Err(err);
        }
        Ok(())
    }

    pub(crate) fn apply_rw_bindings_to_env(
        &self,
        rw_bindings: &[(String, String)],
        target_env: &mut crate::env::Env,
    ) {
        for (param_name, source_name) in rw_bindings {
            if let Some(updated) = self.env.get(param_name).cloned() {
                target_env.insert(source_name.clone(), updated);
            }
        }
    }

    fn bind_param_value(&mut self, name: &str, value: Value) {
        self.env.insert(name.to_string(), value.clone());
        // Extract attribute name from twigil params: $!x → "x", @!types → "types", %!h → "h"
        let attr_name = if let Some(a) = name.strip_prefix('!') {
            Some(a)
        } else {
            // Handle sigil+twigil: @!types, %!h, @.items, %.pairs
            name.strip_prefix("@!")
                .or_else(|| name.strip_prefix("%!"))
                .or_else(|| name.strip_prefix("@."))
                .or_else(|| name.strip_prefix("%."))
        };
        if let Some(attr_name) = attr_name {
            // Also set the canonical !attr env key so write-back finds it
            self.env.insert(format!("!{}", attr_name), value.clone());
            self.env.insert(format!(".{}", attr_name), value.clone());
            if let Some(Value::Instance {
                class_name,
                attributes,
                id,
            }) = self.env.get("self").cloned()
            {
                let mut updated_attrs = (*attributes).clone();
                updated_attrs.insert(attr_name.to_string(), value.clone());
                self.env.insert(
                    "self".to_string(),
                    Value::make_instance_with_id(class_name, updated_attrs, id),
                );
            }
        }
        if matches!(
            value,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) && !name.starts_with('&')
        {
            self.env.insert(format!("&{}", name), value.clone());
        }
        // Placeholder variables also create a non-twigil alias:
        // $^foo → $foo, &^c → &c
        if let Some(bare) = name.strip_prefix("&^") {
            self.env.insert(format!("&{}", bare), value);
        } else if let Some(bare) = name.strip_prefix('^') {
            self.env.insert(bare.to_string(), value);
        }
    }

    pub(crate) fn captured_type_object(value: &Value) -> Value {
        match value {
            Value::Package(name) => Value::Package(*name),
            Value::Instance { class_name, .. } => Value::Package(*class_name),
            Value::Nil => Value::Package(Symbol::intern("Any")),
            _ => Value::Package(Symbol::intern(super::value_type_name(value))),
        }
    }

    fn type_capture_marker_key(name: &str) -> String {
        format!("__type_capture__{}", name)
    }

    pub(crate) fn bind_type_capture(&mut self, name: &str, value: &Value) {
        self.env
            .insert(name.to_string(), Self::captured_type_object(value));
        self.env
            .insert(Self::type_capture_marker_key(name), Value::Bool(true));
    }

    pub(crate) fn has_type_capture_binding(&self, name: &str) -> bool {
        matches!(
            self.env.get(&Self::type_capture_marker_key(name)),
            Some(Value::Bool(true))
        )
    }

    fn optional_type_object_name(constraint: &str) -> String {
        let mut end = constraint.len();
        for (idx, ch) in constraint.char_indices() {
            if ch == '[' || ch == '(' || ch == ':' {
                end = idx;
                break;
            }
        }
        constraint[..end].to_string()
    }

    fn missing_optional_param_value(pd: &ParamDef) -> Value {
        if pd.name.starts_with('@') {
            return Value::real_array(Vec::new());
        }
        if pd.name.starts_with('%') {
            return Value::hash(std::collections::HashMap::new());
        }
        if let Some(constraint) = &pd.type_constraint {
            return Value::Package(Symbol::intern(&Self::optional_type_object_name(constraint)));
        }
        Value::Nil
    }

    pub(crate) fn nominal_type_object_name_for_constraint(&self, constraint: &str) -> String {
        let (base, _) = strip_type_smiley(constraint);
        if let Some((target, _source)) = parse_coercion_type(base) {
            return self.nominal_type_object_name_for_constraint(target);
        }
        let base_name = Self::optional_type_object_name(base);
        if let Some(captured_name) = base_name.strip_prefix("::")
            && let Some(Value::Package(bound)) = self.env.get(captured_name)
        {
            let resolved = bound.resolve();
            if resolved != captured_name {
                return self.nominal_type_object_name_for_constraint(&resolved);
            }
        }
        if let Some(Value::Package(bound)) = self.env.get(&base_name) {
            let resolved = bound.resolve();
            if resolved != base_name {
                return self.nominal_type_object_name_for_constraint(&resolved);
            }
        }
        if let Some(subset) = self.subsets.get(&base_name)
            && subset.base != base_name
        {
            return self.nominal_type_object_name_for_constraint(&subset.base);
        }
        base_name
    }

    fn resolve_constraint_alias(&self, constraint: &str) -> String {
        if let Some(captured) = constraint.strip_prefix("::")
            && let Some(Value::Package(name)) = self.env.get(captured)
        {
            return name.resolve();
        }
        if let Some(Value::Package(name)) = self.env.get(constraint) {
            return name.resolve();
        }
        constraint.to_string()
    }

    fn checked_default_param_value(
        &mut self,
        pd: &ParamDef,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(constraint) = &pd.type_constraint
            && !self.type_matches_value(constraint, &value)
        {
            return Err(RuntimeError::new(format!(
                "X::Parameter::Default::TypeCheck: Type check failed for default value of parameter '{}'; expected {}, got {}",
                pd.name,
                constraint,
                super::value_type_name(&value)
            )));
        }
        Ok(value)
    }

    fn parse_generic_constraint(constraint: &str) -> Option<(&str, &str)> {
        let open = constraint.find('[')?;
        if open == 0 || !constraint.ends_with(']') {
            return None;
        }
        let base = &constraint[..open];
        let inner = &constraint[open + 1..constraint.len() - 1];
        if base.is_empty() || inner.is_empty() {
            return None;
        }
        Some((base, inner))
    }

    pub(super) fn init_endian_enum(&mut self) {
        let variants = vec![
            ("NativeEndian".to_string(), EnumValue::Int(0)),
            ("LittleEndian".to_string(), EnumValue::Int(1)),
            ("BigEndian".to_string(), EnumValue::Int(2)),
        ];
        self.enum_types
            .insert("Endian".to_string(), variants.clone());
        self.env
            .insert("Endian".to_string(), Value::str_from("Endian"));
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: Symbol::intern("Endian"),
                key: Symbol::intern(key),
                value: val.clone(),
                index,
            };
            // Register as both Endian::NativeEndian and bare NativeEndian
            self.env
                .insert(format!("Endian::{}", key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
    }

    pub(super) fn init_protocol_family_enum(&mut self) {
        let variants = vec![
            ("PF_UNSPEC".to_string(), EnumValue::Int(0)),
            ("PF_INET".to_string(), EnumValue::Int(1)),
            ("PF_INET6".to_string(), EnumValue::Int(2)),
            ("PF_LOCAL".to_string(), EnumValue::Int(3)),
            ("PF_UNIX".to_string(), EnumValue::Int(3)),
            ("PF_MAX".to_string(), EnumValue::Int(4)),
        ];
        self.enum_types
            .insert("ProtocolFamily".to_string(), variants.clone());
        self.env.insert(
            "ProtocolFamily".to_string(),
            Value::Package(Symbol::intern("ProtocolFamily")),
        );
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: Symbol::intern("ProtocolFamily"),
                key: Symbol::intern(key),
                value: val.clone(),
                index,
            };
            self.env
                .insert(format!("ProtocolFamily::{}", key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
    }

    pub(super) fn init_order_enum(&mut self) {
        let variants = vec![
            ("Less".to_string(), EnumValue::Int(-1)),
            ("Same".to_string(), EnumValue::Int(0)),
            ("More".to_string(), EnumValue::Int(1)),
        ];
        self.enum_types
            .insert("Order".to_string(), variants.clone());
        self.env
            .insert("Order".to_string(), Value::str_from("Order"));
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: Symbol::intern("Order"),
                key: Symbol::intern(key),
                value: val.clone(),
                index,
            };
            self.env.insert(format!("Order::{}", key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
    }

    pub(super) fn init_signal_enum(&mut self) {
        // Use libc constants on Unix, standard POSIX numbers on other platforms
        let variants = vec![
            ("SIGHUP".to_string(), EnumValue::Int(Self::sig_num(1))),
            ("SIGINT".to_string(), EnumValue::Int(Self::sig_num(2))),
            ("SIGQUIT".to_string(), EnumValue::Int(Self::sig_num(3))),
            ("SIGILL".to_string(), EnumValue::Int(Self::sig_num(4))),
            ("SIGABRT".to_string(), EnumValue::Int(Self::sig_num(6))),
            ("SIGFPE".to_string(), EnumValue::Int(Self::sig_num(8))),
            ("SIGKILL".to_string(), EnumValue::Int(Self::sig_num(9))),
            ("SIGSEGV".to_string(), EnumValue::Int(Self::sig_num(11))),
            ("SIGPIPE".to_string(), EnumValue::Int(Self::sig_num(13))),
            ("SIGALRM".to_string(), EnumValue::Int(Self::sig_num(14))),
            ("SIGTERM".to_string(), EnumValue::Int(Self::sig_num(15))),
            ("SIGUSR1".to_string(), EnumValue::Int(Self::sig_num(10))),
            ("SIGUSR2".to_string(), EnumValue::Int(Self::sig_num(12))),
            ("SIGCHLD".to_string(), EnumValue::Int(Self::sig_num(17))),
            ("SIGCONT".to_string(), EnumValue::Int(Self::sig_num(18))),
            ("SIGSTOP".to_string(), EnumValue::Int(Self::sig_num(19))),
            ("SIGTSTP".to_string(), EnumValue::Int(Self::sig_num(20))),
            ("SIGTTIN".to_string(), EnumValue::Int(Self::sig_num(21))),
            ("SIGTTOU".to_string(), EnumValue::Int(Self::sig_num(22))),
        ];
        self.enum_types
            .insert("Signal".to_string(), variants.clone());
        self.env
            .insert("Signal".to_string(), Value::str_from("Signal"));
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: Symbol::intern("Signal"),
                key: Symbol::intern(key),
                value: val.clone(),
                index,
            };
            self.env
                .insert(format!("Signal::{}", key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
    }

    /// Get signal number — use the POSIX default value on all platforms.
    fn sig_num(default: i64) -> i64 {
        default
    }

    pub(super) fn version_from_value(arg: Value) -> Value {
        use crate::value::VersionPart;
        match arg {
            Value::Str(s) => {
                if s.is_empty() {
                    return Value::Version {
                        parts: Vec::new(),
                        plus: false,
                        minus: false,
                    };
                }
                let (parts, plus, minus) = Value::parse_version_string(&s);
                Value::Version { parts, plus, minus }
            }
            // Version.new(*) - Whatever argument (bare * evaluates to Num(Inf))
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => Value::Version {
                parts: vec![VersionPart::Whatever],
                plus: false,
                minus: false,
            },
            _ => {
                let s = arg.to_string_value();
                Self::version_from_value(Value::str(s))
            }
        }
    }

    pub(crate) fn version_smart_match(
        left: &Value,
        right_parts: &[crate::value::VersionPart],
        right_plus: bool,
        right_minus: bool,
    ) -> bool {
        use crate::value::VersionPart;
        if let Value::Version {
            parts: left_parts, ..
        } = left
        {
            if right_plus {
                // LHS >= RHS (base version without +)
                super::version_cmp_parts(left_parts, right_parts) != std::cmp::Ordering::Less
            } else if right_minus {
                // LHS <= RHS (base version without -)
                super::version_cmp_parts(left_parts, right_parts) != std::cmp::Ordering::Greater
            } else {
                // Compare up to the length of the RHS; extra LHS parts are ignored
                let rhs_len = right_parts.len();
                for i in 0..rhs_len {
                    let l = left_parts.get(i).unwrap_or(&VersionPart::Num(0));
                    let r = right_parts.get(i).unwrap_or(&VersionPart::Num(0));
                    match (l, r) {
                        (VersionPart::Whatever, _) | (_, VersionPart::Whatever) => continue,
                        (VersionPart::Num(a), VersionPart::Num(b)) => {
                            if a != b {
                                return false;
                            }
                        }
                        (VersionPart::Str(a), VersionPart::Str(b)) => {
                            if a != b {
                                return false;
                            }
                        }
                        // Different types (Num vs Str) are never equal
                        _ => return false,
                    }
                }
                // If RHS is longer than LHS, extra RHS parts must be zero
                if rhs_len > left_parts.len() {
                    for p in &right_parts[left_parts.len()..] {
                        match p {
                            VersionPart::Num(n) if *n != 0 => return false,
                            _ => {}
                        }
                    }
                }
                true
            }
        } else {
            false
        }
    }

    pub(crate) fn value_is_nan(value: &Value) -> bool {
        match value {
            Value::Num(f) => f.is_nan(),
            Value::Complex(r, i) => r.is_nan() || i.is_nan(),
            Value::Str(s) => s.trim().eq_ignore_ascii_case("nan"),
            _ => false,
        }
    }

    pub(super) fn type_matches(constraint: &str, value_type: &str) -> bool {
        if constraint == "Mu" {
            return true;
        }
        if constraint == "Any" {
            // Junction is a direct subtype of Mu, not Any
            return value_type != "Junction";
        }
        if constraint == value_type {
            return true;
        }
        if constraint == "Setty" && matches!(value_type, "Set" | "SetHash") {
            return true;
        }
        if constraint == "Baggy" && matches!(value_type, "Bag" | "BagHash" | "Mix" | "MixHash") {
            return true;
        }
        if constraint == "Mixy" && matches!(value_type, "Mix" | "MixHash") {
            return true;
        }
        // Metamodel:: is an alias for Perl6::Metamodel::
        if constraint.starts_with("Metamodel::") {
            let full = format!("Perl6::{}", constraint);
            if full == value_type {
                return true;
            }
        }
        if value_type.starts_with("Metamodel::") {
            let full = format!("Perl6::{}", value_type);
            if full == constraint {
                return true;
            }
        }
        // Native type aliases: num → Num, int → Int, str → Str
        if constraint == "num" && value_type == "Num" {
            return true;
        }
        if constraint == "int" && value_type == "Int" {
            return true;
        }
        if constraint == "atomicint" && value_type == "Int" {
            return true;
        }
        if constraint == "str" && value_type == "Str" {
            return true;
        }
        // Native integer types match Int values
        if crate::runtime::native_types::is_native_int_type(constraint) && value_type == "Int" {
            return true;
        }
        // Native float types (num32, num64) are subtypes of Num
        if constraint == "Num" && matches!(value_type, "num32" | "num64" | "num") {
            return true;
        }
        if matches!(constraint, "num32" | "num64") && value_type == "Num" {
            return true;
        }
        // Native str type is a subtype of Str
        if constraint == "Str" && value_type == "str" {
            return true;
        }
        // Numeric hierarchy: Int is a Numeric, Num is a Numeric
        if constraint == "Numeric"
            && matches!(
                value_type,
                "Int" | "Num" | "Rat" | "FatRat" | "Complex" | "Bool" | "UInt"
            )
        {
            return true;
        }
        if constraint == "Real"
            && matches!(
                value_type,
                "Int" | "Num" | "Rat" | "FatRat" | "Bool" | "UInt"
            )
        {
            return true;
        }
        if constraint == "Dateish" && matches!(value_type, "Date" | "DateTime") {
            return true;
        }
        // Bool is a subtype of Int in Raku's type hierarchy
        // UInt is a subset of Int
        if constraint == "Int" && matches!(value_type, "Bool" | "UInt") {
            return true;
        }
        if constraint == "Cool"
            && matches!(
                value_type,
                "Int" | "Num" | "Str" | "Bool" | "Rat" | "FatRat" | "Complex"
            )
        {
            return true;
        }
        if constraint == "Stringy" && matches!(value_type, "Str") {
            return true;
        }
        if matches!(constraint, "Callable" | "Code" | "Block")
            && matches!(
                value_type,
                "Sub" | "Routine" | "Method" | "Block" | "WhateverCode"
            )
        {
            return true;
        }
        if constraint == "Routine" && matches!(value_type, "Sub" | "Method" | "Routine") {
            return true;
        }
        if constraint == "Variable" && matches!(value_type, "Scalar" | "Array" | "Hash" | "Sub") {
            return true;
        }
        // Role-like type relationships
        if constraint == "Positional"
            && matches!(
                value_type,
                "Array" | "List" | "Seq" | "Range" | "Buf" | "Blob" | "Capture"
            )
        {
            return true;
        }
        // Array is-a List in Raku type hierarchy
        if constraint == "List" && matches!(value_type, "Array" | "List" | "Seq") {
            return true;
        }
        if constraint == "Associative"
            && matches!(
                value_type,
                "Hash" | "Map" | "Pair" | "Bag" | "Set" | "Mix" | "QuantHash" | "Capture"
            )
        {
            return true;
        }
        // Buf/Blob type hierarchy:
        // Blob is the immutable base; Buf extends Blob (mutable)
        // utf8 is a subtype of Blob
        // buf8/buf16/buf32/buf64 are subtypes of Buf (and transitively Blob)
        // blob8/blob16/blob32/blob64 are subtypes of Blob
        if constraint == "Blob"
            && matches!(
                value_type,
                "Buf"
                    | "utf8"
                    | "utf16"
                    | "buf8"
                    | "buf16"
                    | "buf32"
                    | "buf64"
                    | "blob8"
                    | "blob16"
                    | "blob32"
                    | "blob64"
            )
        {
            return true;
        }
        if constraint == "Buf" && matches!(value_type, "buf8" | "buf16" | "buf32" | "buf64") {
            return true;
        }
        false
    }

    /// Check if a type name is known (either a class, role, or enum).
    pub(crate) fn has_type(&self, name: &str) -> bool {
        self.classes.contains_key(name)
            || self.roles.contains_key(name)
            || self.enum_types.contains_key(name)
            || self.subsets.contains_key(name)
            || Self::parse_parametric_type_name(name).is_some_and(|(base, _)| {
                self.classes.contains_key(&base)
                    || self.roles.contains_key(&base)
                    || self.enum_types.contains_key(&base)
                    || self.subsets.contains_key(&base)
            })
    }

    pub(crate) fn has_enum_type(&self, name: &str) -> bool {
        self.enum_types.contains_key(name)
    }

    pub(crate) fn has_enum_variant(&self, enum_name: &str, variant_name: &str) -> bool {
        self.enum_types
            .get(enum_name)
            .is_some_and(|variants| variants.iter().any(|(k, _)| k == variant_name))
    }

    pub(crate) fn has_role(&self, name: &str) -> bool {
        self.roles.contains_key(name)
    }

    pub(crate) fn role_has_method(&self, role_name: &str, method_name: &str) -> bool {
        self.roles
            .get(role_name)
            .is_some_and(|r| r.methods.contains_key(method_name))
    }

    pub(crate) fn is_definite_constraint(&self, constraint: &str) -> bool {
        let (base_constraint, smiley) = strip_type_smiley(constraint);
        if smiley == Some(":D") {
            return true;
        }
        if let Some((target, _source)) = parse_coercion_type(base_constraint) {
            return self.is_definite_constraint(target);
        }
        if let Some(subset) = self.subsets.get(base_constraint) {
            if self.is_definite_constraint(&subset.base) {
                return true;
            }
            if language_version_is_6e_or_newer(&subset.version)
                && subset
                    .predicate
                    .as_ref()
                    .is_some_and(predicate_requires_defined)
            {
                return true;
            }
        }
        false
    }

    pub(crate) fn set_variables_pragma(&mut self, smiley: &str) {
        // smiley is ":D", ":U", ":_", or empty
        self.variables_pragma = smiley.to_string();
    }

    /// Apply the `use variables` pragma to a type constraint.
    /// If the constraint has no explicit smiley and the pragma is active,
    /// append the pragma smiley (e.g., `Int` → `Int:D` when `use variables :D`).
    pub(crate) fn apply_variables_pragma<'a>(
        &self,
        constraint: &'a str,
    ) -> std::borrow::Cow<'a, str> {
        if self.variables_pragma.is_empty() || self.variables_pragma == ":_" {
            return std::borrow::Cow::Borrowed(constraint);
        }
        let (_base, smiley) = strip_type_smiley(constraint);
        if smiley.is_some() {
            // Already has a smiley — don't override
            return std::borrow::Cow::Borrowed(constraint);
        }
        std::borrow::Cow::Owned(format!("{}{}", constraint, self.variables_pragma))
    }

    pub(crate) fn eval_does_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some((role_name, args)) = self.extract_role_application(&right) {
            let result = self.compose_role_on_value(left.clone(), &role_name, &args)?;
            if let Some(target_name) = Self::var_target_name_from_value(&left) {
                self.set_var_meta_value(&target_name, result.clone());
            }
            return Ok(result);
        }
        let role_name = right.to_string_value();
        Ok(Value::Bool(left.does_check(&role_name)))
    }

    fn var_target_name_from_value(value: &Value) -> Option<String> {
        match value {
            Value::Mixin(inner, _) => Self::var_target_name_from_value(inner),
            Value::Instance { attributes, .. } => match attributes.get("__mutsu_var_target") {
                Some(Value::Str(name)) => Some(name.to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    /// Check if a value represents a role application (used by VM to decide
    /// whether to fall back to the interpreter for `does` operations).
    pub(crate) fn is_role_application(&self, rhs: &Value) -> bool {
        self.extract_role_application(rhs).is_some()
    }

    fn extract_role_application(&self, rhs: &Value) -> Option<(String, Vec<Value>)> {
        match rhs {
            Value::ParametricRole {
                base_name,
                type_args,
            } if self.roles.contains_key(&base_name.resolve()) => {
                Some((base_name.resolve(), type_args.clone()))
            }
            Value::Pair(name, boxed) if self.roles.contains_key(name) => {
                if let Value::Array(args, ..) = boxed.as_ref() {
                    Some((name.clone(), args.as_ref().clone()))
                } else {
                    None
                }
            }
            Value::Package(name) if self.roles.contains_key(&name.resolve()) => {
                Some((name.resolve(), Vec::new()))
            }
            Value::Str(name) if self.roles.contains_key(name.as_str()) => {
                Some((name.to_string(), Vec::new()))
            }
            _ => None,
        }
    }

    fn compose_role_on_value(
        &mut self,
        left: Value,
        role_name: &str,
        role_args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let role = self.roles.get(role_name).cloned();
        if role.is_none() && !matches!(role_name, "Real" | "Numeric" | "Cool" | "Any" | "Mu") {
            return Err(RuntimeError::new(format!("Unknown role: {}", role_name)));
        }

        let (inner, mut mixins) = match left {
            Value::Mixin(inner, existing) => (inner.as_ref().clone(), (*existing).clone()),
            other => (other, HashMap::new()),
        };
        mixins.insert(format!("__mutsu_role__{}", role_name), Value::Bool(true));
        // Store the role's unique ID so that different lexical roles with the
        // same name (e.g. two `my role A { }` in different scopes) produce
        // distinct mixin maps, making `===` return False for values mixed with
        // different role instances.
        let role_id = self.roles.get(role_name).map_or(0, |r| r.role_id);
        if role_id != 0 {
            mixins.insert(
                format!("__mutsu_role_id__{}", role_name),
                Value::Int(role_id as i64),
            );
        }

        if let Some(role) = role {
            // Temporarily merge captured environment from the role definition
            // so that attribute defaults can reference closure variables.
            let saved_env = if let Some(captured) = &role.captured_env {
                let saved = self.env.clone();
                for (k, v) in captured {
                    if !self.env.contains_key(k) {
                        self.env.insert(k.clone(), v.clone());
                    }
                }
                Some(saved)
            } else {
                None
            };
            for (idx, (attr_name, _is_public, default_expr, _, _, _, _)) in
                role.attributes.iter().enumerate()
            {
                let value = if let Some(arg) = role_args.get(idx) {
                    arg.clone()
                } else if let Some(default_expr) = default_expr {
                    self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?
                } else {
                    Value::Nil
                };
                mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
            }
            if let Some(saved) = saved_env {
                self.env = saved;
            }
        }

        Ok(Value::mixin(inner, mixins))
    }

    /// Check if a constraint string refers to a known type (built-in or user-defined).
    /// Used for __type_only__ params to distinguish real type constraints (Str, Int)
    /// from sigilless parameter names (e1, e2) that look like type constraints.
    pub(crate) fn is_resolvable_type(&self, constraint: &str) -> bool {
        // Strip definedness smileys
        let base = constraint
            .strip_suffix(":D")
            .or_else(|| constraint.strip_suffix(":U"))
            .or_else(|| constraint.strip_suffix(":_"))
            .unwrap_or(constraint);
        // Strip parameterization: Array[Int] → Array
        let base = if let Some(idx) = base.find('[') {
            &base[..idx]
        } else {
            base
        };
        // Check built-in types
        if matches!(
            base,
            "Mu" | "Any"
                | "Cool"
                | "Int"
                | "UInt"
                | "Num"
                | "Str"
                | "Bool"
                | "Array"
                | "List"
                | "Hash"
                | "Map"
                | "Rat"
                | "FatRat"
                | "Complex"
                | "Range"
                | "Seq"
                | "Pair"
                | "Set"
                | "SetHash"
                | "Bag"
                | "BagHash"
                | "Mix"
                | "MixHash"
                | "Junction"
                | "Regex"
                | "Match"
                | "Nil"
                | "Failure"
                | "Exception"
                | "Callable"
                | "Sub"
                | "Method"
                | "Block"
                | "Routine"
                | "Code"
                | "WhateverCode"
                | "Whatever"
                | "Numeric"
                | "Real"
                | "Stringy"
                | "Positional"
                | "Associative"
                | "IO"
                | "Supply"
                | "Promise"
                | "Channel"
                | "Buf"
                | "Blob"
                | "utf8"
                | "Version"
                | "Instant"
                | "Duration"
                | "DateTime"
                | "Date"
                | "Capture"
                | "Signature"
                | "Parameter"
                | "Stash"
                | "Grammar"
                | "Proc"
        ) {
            return true;
        }
        // Check native types
        if super::utils::is_known_type_constraint(base) {
            return true;
        }
        // Check user-defined classes
        if self.has_class(base) {
            return true;
        }
        // Check if it starts with uppercase (heuristic for type names)
        // This handles cases like user-defined enum types that may not be registered as classes
        false
    }

    pub(crate) fn type_matches_value(&mut self, constraint: &str, value: &Value) -> bool {
        if let Value::Scalar(inner) = value {
            return self.type_matches_value(constraint, inner.as_ref());
        }
        if constraint == "UInt" {
            return match value {
                Value::Int(i) => *i >= 0,
                Value::BigInt(n) => n.sign() != num_bigint::Sign::Minus,
                Value::Nil => true,
                Value::Package(name) => {
                    let name = name.resolve();
                    name == "UInt" || name == "Int"
                }
                _ => false,
            };
        }
        if let Some((base, _)) = constraint.split_once(':')
            && base == "Variable"
            && varref_from_value(value).is_some()
        {
            return true;
        }
        if constraint == "Variable" && varref_from_value(value).is_some() {
            return true;
        }
        if let Value::Enum { enum_type, .. } = value {
            let enum_name = enum_type.resolve();
            if constraint == enum_name || Self::type_matches(constraint, &enum_name) {
                return true;
            }
        }
        let package_matches_type = |package_name: &str, type_name: &str| -> bool {
            let package_base = package_name.split('[').next().unwrap_or(package_name);
            let (package_base, _) = strip_type_smiley(package_base);
            if Self::type_matches(type_name, package_base) {
                return true;
            }
            if let Some(class_def) = self.classes.get(package_base) {
                return class_def
                    .parents
                    .clone()
                    .iter()
                    .any(|parent| Self::type_matches(type_name, parent));
            }
            false
        };
        if let Value::Package(package_name) = value
            && let Some((lhs_base, lhs_inner)) =
                Self::parse_generic_constraint(&package_name.resolve())
            && let Some((rhs_base, rhs_inner)) = Self::parse_generic_constraint(constraint)
            && Self::type_matches(rhs_base, lhs_base)
        {
            let (lhs_inner_base, lhs_inner_smiley) = strip_type_smiley(lhs_inner);
            let (rhs_inner_base, rhs_inner_smiley) = strip_type_smiley(rhs_inner);
            if !Self::type_matches(rhs_inner_base, lhs_inner_base) {
                return false;
            }
            return match rhs_inner_smiley {
                Some(":D") => lhs_inner_smiley == Some(":D"),
                Some(":U") => lhs_inner_smiley != Some(":D"),
                _ => true,
            };
        }
        if let Value::Package(package_name) = value
            && let Some((pkg_target, pkg_source)) = parse_coercion_type(&package_name.resolve())
            && (Self::type_matches(constraint, pkg_target)
                || pkg_source.is_some_and(|src| Self::type_matches(constraint, src)))
        {
            return true;
        }
        if let Value::Package(package_name) = value
            && let Some((target, source)) = parse_coercion_type(constraint)
        {
            let pkg_resolved = package_name.resolve();
            let target_ok = package_matches_type(&pkg_resolved, target);
            let source_ok = source.is_some_and(|src| self.type_matches_value(src, value));
            return target_ok || source_ok;
        }
        if let Value::Package(package_name) = value
            && package_matches_type(&package_name.resolve(), constraint)
        {
            return true;
        }
        // Handle coercion types: Int() matches anything, Int(Rat) matches Rat
        if let Some((target, source)) = parse_coercion_type(constraint) {
            return if let Some(src) = source {
                self.type_matches_value(src, value) || self.type_matches_value(target, value)
            } else {
                true
            };
        }
        if let Some((base, inner)) = Self::parse_generic_constraint(constraint) {
            match base {
                "Array" | "List" | "Positional" => {
                    if let Value::Array(items, ..) = value {
                        return items.iter().all(|v| self.type_matches_value(inner, v));
                    }
                    return false;
                }
                "buf8" | "blob8" | "buf16" | "buf32" | "buf64" | "blob16" | "blob32" | "blob64" => {
                    if let Value::Instance { class_name, .. } = value {
                        let cn = class_name.resolve();
                        if cn == "Buf"
                            || cn == "Blob"
                            || cn.starts_with("Buf[")
                            || cn.starts_with("Blob[")
                            || cn.starts_with("buf")
                            || cn.starts_with("blob")
                        {
                            return true;
                        }
                    }
                    return false;
                }
                "Hash" | "Associative" => {
                    if let Value::Hash(map) = value {
                        return map.values().all(|v| self.type_matches_value(inner, v));
                    }
                    if let Value::Array(items, ..) = value {
                        return items.iter().all(|item| {
                            if let Value::Pair(_, v) = item {
                                self.type_matches_value(inner, v)
                            } else {
                                false
                            }
                        });
                    }
                    return false;
                }
                "Buf" | "Blob" => {
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = value
                    {
                        let class = class_name.resolve();
                        let class_ok = if base == "Buf" {
                            class == "Buf" || class.starts_with("Buf[") || class.starts_with("buf")
                        } else {
                            class == "Blob"
                                || class == "Buf"
                                || class.starts_with("Blob[")
                                || class.starts_with("blob")
                                || class.starts_with("Buf[")
                                || class.starts_with("buf")
                        };
                        if !class_ok {
                            return false;
                        }
                        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                            return items.iter().all(|v| self.type_matches_value(inner, v));
                        }
                    }
                    return false;
                }
                _ => {}
            }
        }
        if let Some(Value::Package(bound)) = self.env.get(constraint).cloned()
            && bound != *constraint
        {
            return self.type_matches_value(&bound.resolve(), value);
        }
        // Handle type smileys (:U, :D, :_)
        let (base_constraint, smiley) = strip_type_smiley(constraint);
        if let Some(smiley) = smiley {
            let type_ok = self.type_matches_value(base_constraint, value);
            if !type_ok {
                return false;
            }
            return match smiley {
                ":U" => !value_is_defined(value),
                ":D" => value_is_defined(value),
                ":_" => true,
                _ => true,
            };
        }

        if let Some(subset) = self.subsets.get(constraint).cloned() {
            if !self.type_matches_value(&subset.base, value) {
                return false;
            }
            let predicate_value = self.coerce_value_for_constraint(&subset.base, value.clone());
            let ok = if let Some(pred) = &subset.predicate {
                // Check if the predicate is a block/lambda (AnonSub, AnonSubParams, etc.)
                let is_callable_expr = matches!(
                    pred,
                    Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Block(_)
                );
                if is_callable_expr {
                    // Evaluate to get a callable, then call with the value
                    match self.eval_block_value(&[Stmt::Expr(pred.clone())]) {
                        Ok(callable @ Value::Sub(_)) => self
                            .call_sub_value(callable, vec![predicate_value], false)
                            .map(|v| v.truthy())
                            .unwrap_or(false),
                        Ok(v) => v.truthy(),
                        Err(_) => false,
                    }
                } else {
                    // Bare expression: set $_ and evaluate directly
                    let saved = self.env.get("_").cloned();
                    self.env.insert("_".to_string(), predicate_value);
                    let result = self
                        .eval_block_value(&[Stmt::Expr(pred.clone())])
                        .map(|v| self.smart_match(value, &v))
                        .unwrap_or(false);
                    if let Some(old) = saved {
                        self.env.insert("_".to_string(), old);
                    } else {
                        self.env.remove("_");
                    }
                    result
                }
            } else {
                true
            };
            return ok;
        }
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = value
            && (base_name.resolve() == constraint
                || self.role_is_subtype(&base_name.resolve(), constraint)
                || self
                    .role_parent_args_for(&base_name.resolve(), type_args, constraint)
                    .is_some())
        {
            return true;
        }
        // Role constraints should accept composed role instances/mixins.
        if self.roles.contains_key(constraint) && value.does_check(constraint) {
            return true;
        }
        // Type-object checks: Package values should respect declared class/role ancestry.
        if let Value::Package(package_name) = value {
            if Self::type_matches(constraint, &package_name.resolve()) {
                return true;
            }
            if let Some(subset_base) = self
                .subsets
                .get(&package_name.resolve())
                .map(|subset| subset.base.clone())
                && (constraint == package_name.resolve()
                    || self.type_matches_value(
                        constraint,
                        &Value::Package(Symbol::intern(&subset_base)),
                    ))
            {
                return true;
            }
            let mro = self.class_mro(&package_name.resolve());
            if mro.iter().any(|parent| parent == constraint) {
                return true;
            }
            // Check transitive role composition through class_composed_roles and role_parents
            if self.roles.contains_key(constraint) {
                let mut role_stack: Vec<String> = Vec::new();
                let mut seen_roles = HashSet::new();
                // Collect all composed roles from the class and its parents
                for cn in &mro {
                    if let Some(composed) = self.class_composed_roles.get(cn.as_str()) {
                        for cr in composed {
                            let base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                            role_stack.push(base.to_string());
                        }
                    }
                }
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if role_name == constraint {
                        return true;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name) {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
            if let Some(constraint_base) = constraint.split_once('[').map(|(base, _)| base)
                && mro.iter().any(|parent| {
                    parent == constraint_base || parent.starts_with(&format!("{constraint_base}["))
                })
            {
                return true;
            }
            if self.roles.contains_key(&package_name.resolve()) {
                let mut stack: Vec<String> = vec![package_name.resolve()];
                let mut seen = HashSet::new();
                while let Some(role) = stack.pop() {
                    if !seen.insert(role.clone()) {
                        continue;
                    }
                    if let Some(parents) = self.role_parents.get(&role) {
                        for parent in parents {
                            if parent == constraint {
                                return true;
                            }
                            stack.push(parent.clone());
                        }
                    }
                }
            }
        }
        // Check Instance class name against constraint (including parent classes)
        if let Value::Instance { class_name, .. } = value {
            if Self::type_matches(constraint, &class_name.resolve()) {
                return true;
            }
            // Buf/Blob hierarchy: Buf[uint8] isa Buf, buf8 isa Buf, etc.
            let cn = class_name.resolve();
            if (constraint == "Buf" || constraint == "Blob")
                && crate::runtime::utils::is_buf_or_blob_class(&cn)
            {
                // Buf constraint accepts any Buf-like, Blob constraint accepts any Blob-like
                if constraint == "Buf" && crate::runtime::utils::is_buf_like_class(&cn) {
                    return true;
                }
                if constraint == "Blob" {
                    return true; // All Buf/Blob types are Blob (Buf inherits Blob)
                }
            }
            // Check parent classes of the instance
            if let Some(class_def) = self.classes.get(&class_name.resolve()) {
                for parent in class_def.parents.clone() {
                    if Self::type_matches(constraint, &parent) {
                        return true;
                    }
                }
            }
            // Check composed roles for the instance's class (and its MRO)
            if self.roles.contains_key(constraint) {
                let mro = self.class_mro(&class_name.resolve());
                let mut role_stack: Vec<String> = Vec::new();
                let mut seen_roles = HashSet::new();
                for cn in &mro {
                    if let Some(composed) = self.class_composed_roles.get(cn.as_str()) {
                        for cr in composed {
                            let base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                            role_stack.push(base.to_string());
                        }
                    }
                }
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if role_name == constraint {
                        return true;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name) {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
        }
        // Mixin allomorphic types: check both inner type and mixin type keys
        if let Value::Mixin(inner, mixins) = value {
            // Check allomorphic type names first (IntStr, NumStr, RatStr, ComplexStr, Allomorph)
            if value.isa_check(constraint) {
                return true;
            }
            if self.type_matches_value(constraint, inner) {
                return true;
            }
            if mixins.contains_key(constraint) {
                return true;
            }
        }
        // For Package (type object) values, use the package name as the type
        // so that e.g. Junction (which is Mu, not Any) is correctly rejected
        // when the constraint is Any.
        if let Value::Package(package_name) = value {
            let resolved = package_name.resolve();
            return Self::type_matches(constraint, &resolved);
        }
        let value_type = super::value_type_name(value);
        Self::type_matches(constraint, value_type)
    }

    pub(super) fn args_match_param_types(
        &mut self,
        args: &[Value],
        param_defs: &[ParamDef],
    ) -> bool {
        let saved_env = self.env.clone();
        let result = (|| {
            let positional_params: Vec<&ParamDef> =
                param_defs.iter().filter(|p| !p.named).collect();
            let positional_arg_count = args
                .iter()
                .filter(|arg| {
                    !matches!(
                        unwrap_varref_value((*arg).clone()),
                        Value::Pair(..) | Value::ValuePair(..)
                    )
                })
                .count();
            let mut required_positional_count = 0usize;
            let mut positional_max_count = 0usize;
            let mut has_variadic_positional = false;
            for pd in &positional_params {
                let is_capture_param = pd.name == "_capture" || (pd.slurpy && pd.sigilless);
                let is_subsig_capture = pd.name == "__subsig__" && pd.sub_signature.is_some();
                if pd.slurpy || is_capture_param || is_subsig_capture {
                    has_variadic_positional = true;
                    continue;
                }
                positional_max_count += 1;
                if pd.default.is_none() && !pd.optional_marker {
                    required_positional_count += 1;
                }
            }
            if positional_arg_count < required_positional_count {
                return false;
            }
            if !has_variadic_positional && positional_arg_count > positional_max_count {
                return false;
            }
            let mut i = 0usize;
            for pd in positional_params {
                let is_capture_param = pd.name == "_capture" || (pd.slurpy && pd.sigilless);
                let is_subsig_capture = pd.name == "__subsig__" && pd.sub_signature.is_some();
                let arg_for_checks: Option<Value> = if pd.slurpy || is_capture_param {
                    if is_capture_param {
                        // |c capture params preserve both positional and named parts.
                        let mut positional = Vec::new();
                        let mut named = std::collections::HashMap::new();
                        let remaining = args.get(i..).unwrap_or(&[]);
                        for arg in remaining {
                            let arg = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, val) = arg {
                                named.insert(key, *val);
                            } else {
                                positional.push(arg);
                            }
                        }
                        Some(Value::Capture { positional, named })
                    } else {
                        // For single-star slurpy (*@), flatten list arguments but preserve
                        // itemized Arrays ($[...] / .item) as single positional values.
                        let mut items = Vec::new();
                        let remaining = args.get(i..).unwrap_or(&[]);
                        for arg in remaining {
                            let arg = unwrap_varref_value(arg.clone());
                            if !pd.double_slurpy
                                && let Value::Array(arr, kind) = &arg
                                && !kind.is_itemized()
                            {
                                items.extend(arr.iter().cloned());
                                continue;
                            }
                            items.push(arg);
                        }
                        Some(Value::real_array(items))
                    }
                } else if is_subsig_capture {
                    let remaining = args.get(i..).unwrap_or(&[]);
                    Some(sub_signature_target_from_remaining_args(
                        &remaining
                            .iter()
                            .cloned()
                            .map(unwrap_varref_value)
                            .collect::<Vec<_>>(),
                    ))
                } else {
                    args.get(i).cloned().map(unwrap_varref_value)
                };
                // For multi-dispatch: `is rw` params require a writable variable argument
                if pd.traits.iter().any(|t| t == "rw") {
                    let raw_arg = args.get(i);
                    let is_varref = raw_arg
                        .map(|a| varref_from_value(a).is_some())
                        .unwrap_or(false);
                    if !is_varref {
                        return false;
                    }
                }
                if let Some(literal) = &pd.literal_value {
                    if let Some(arg) = arg_for_checks.as_ref() {
                        if arg != literal {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                if arg_for_checks.is_none()
                    && !pd.slurpy
                    && !is_capture_param
                    && !is_subsig_capture
                    && pd.default.is_none()
                    && !pd.optional_marker
                {
                    return false;
                }
                if let Some(constraint) = &pd.type_constraint
                    && let Some(arg) = arg_for_checks.as_ref()
                {
                    if let Some(captured_name) = constraint.strip_prefix("::") {
                        self.bind_type_capture(captured_name, arg);
                    } else if pd.name == "__type_only__" {
                        // Bare identifier param (e.g., enum value) — resolve from env and compare
                        if let Some(expected_val) = self.env.get(constraint).cloned() {
                            if arg != &expected_val {
                                return false;
                            }
                        } else if !self.type_matches_value(constraint, arg) {
                            return false;
                        }
                    } else if pd.name.starts_with('@') {
                        let ok = match arg {
                            Value::Array(items, ..) => {
                                items.iter().all(|v| self.type_matches_value(constraint, v))
                            }
                            Value::Slip(items) => {
                                items.iter().all(|v| self.type_matches_value(constraint, v))
                            }
                            _ => false,
                        };
                        if !ok {
                            return false;
                        }
                    } else if pd.name.starts_with('%') {
                        let ok = match arg {
                            Value::Hash(map) => {
                                map.values().all(|v| self.type_matches_value(constraint, v))
                            }
                            Value::Array(items, ..) => items.iter().all(|item| {
                                if let Value::Pair(_, v) = item {
                                    self.type_matches_value(constraint, v)
                                } else {
                                    false
                                }
                            }),
                            _ => false,
                        };
                        if !ok {
                            return false;
                        }
                    } else if constraint == "Num"
                        && matches!(
                            arg,
                            Value::Int(_)
                                | Value::Num(_)
                                | Value::Rat(_, _)
                                | Value::FatRat(_, _)
                                | Value::BigRat(_, _)
                        )
                    {
                        // Multi-dispatch numeric widening: Int/Rat/FatRat can satisfy Num.
                    } else if !is_coercion_constraint(constraint)
                        && !self.type_matches_value(constraint, arg)
                    {
                        // Coercion source-type validation is deferred until bind time.
                        return false;
                    }
                }
                // Implicit Any constraint: untyped $ parameters reject Junction type objects
                if pd.type_constraint.is_none()
                    && !pd.name.starts_with('@')
                    && !pd.name.starts_with('%')
                    && !pd.name.starts_with('&')
                    && !pd.slurpy
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !self.type_matches_value("Any", arg)
                {
                    return false;
                }
                // Sigil-based dispatch: @ params require Positional args
                if pd.name.starts_with('@')
                    && !pd.slurpy
                    && pd.type_constraint.is_none()
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !matches!(arg, Value::Array(..) | Value::Slip(..) | Value::Nil)
                    && !self.type_matches_value("Positional", arg)
                {
                    return false;
                }
                // Sigil-based dispatch: % params require Associative args
                if pd.name.starts_with('%')
                    && !pd.slurpy
                    && pd.type_constraint.is_none()
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !matches!(arg, Value::Hash(..) | Value::Nil)
                    && !self.type_matches_value("Associative", arg)
                {
                    return false;
                }
                if pd.name.starts_with('&')
                    && let Some(arg) = arg_for_checks.as_ref()
                    && !self.type_matches_value("Callable", arg)
                {
                    return false;
                }
                if let Some(sub_params) = &pd.sub_signature {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    if !sub_signature_matches_value(self, sub_params, arg) {
                        return false;
                    }
                }
                if let Some((sig_params, sig_ret)) = &pd.code_signature {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    if !code_signature_matches_value(self, sig_params, sig_ret, arg) {
                        return false;
                    }
                }
                if let Some(where_expr) = &pd.where_constraint {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    let saved = self.env.clone();
                    self.env.insert("_".to_string(), arg.clone());
                    // Bind the parameter name so that `where {$param ...}` can
                    // reference it during dispatch matching.
                    self.env.insert(pd.name.clone(), arg.clone());
                    let ok = match where_expr.as_ref() {
                        Expr::AnonSub { body, .. } => self
                            .eval_block_value(body)
                            .map(|v| v.truthy())
                            .unwrap_or(false),
                        expr => self
                            .eval_block_value(&[Stmt::Expr(expr.clone())])
                            .map(|v| self.smart_match(arg, &v))
                            .unwrap_or(false),
                    };
                    self.env = saved;
                    if !ok {
                        return false;
                    }
                }
                if is_subsig_capture {
                    i = args.len();
                } else if !pd.slurpy {
                    i += 1;
                }
            }
            let has_named_slurpy = param_defs
                .iter()
                .any(|pd| pd.slurpy && (pd.name.starts_with('%') || pd.sigilless));
            if !has_named_slurpy {
                for arg in args {
                    let arg = unwrap_varref_value(arg.clone());
                    if let Value::Pair(key, _) = arg {
                        let consumed = param_defs.iter().any(|pd| {
                            (pd.named && pd.name == key) || pd.name == format!(":{}", key)
                        });
                        if !consumed {
                            return false;
                        }
                    }
                }
            }
            // Build map of named args for checking required params and where constraints
            let named_args: Vec<(String, Value)> = args
                .iter()
                .filter_map(|a| {
                    let a = unwrap_varref_value(a.clone());
                    if let Value::Pair(key, val) = a {
                        Some((key, *val))
                    } else {
                        None
                    }
                })
                .collect();
            for pd in param_defs.iter().filter(|pd| pd.named) {
                let name = &pd.name;
                let arg_val = named_args
                    .iter()
                    .find(|(k, _)| k == name || *k == format!(":{}", name))
                    .map(|(_, v)| v.clone());

                // Check required named params have corresponding args
                if pd.required && pd.default.is_none() && arg_val.is_none() {
                    return false;
                }

                // Check type constraint on named param
                if let Some(constraint) = &pd.type_constraint
                    && let Some(ref val) = arg_val
                    && !self.type_matches_value(constraint, val)
                {
                    return false;
                }

                // Check where constraint on named param
                if let Some(where_expr) = &pd.where_constraint
                    && let Some(ref val) = arg_val
                {
                    let saved = self.env.clone();
                    self.env.insert("_".to_string(), val.clone());
                    let ok = match where_expr.as_ref() {
                        Expr::AnonSub { body, .. } => self
                            .eval_block_value(body)
                            .map(|v| v.truthy())
                            .unwrap_or(false),
                        expr => self
                            .eval_block_value(&[Stmt::Expr(expr.clone())])
                            .map(|v| self.smart_match(val, &v))
                            .unwrap_or(false),
                    };
                    self.env = saved;
                    if !ok {
                        return false;
                    }
                }
            }
            true
        })();
        self.env = saved_env;
        result
    }

    pub(super) fn method_args_match(&mut self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        let is_invocant_param =
            |p: &ParamDef| p.is_invocant || p.traits.iter().any(|t| t == "invocant");
        let all_invocant_only = param_defs.iter().all(is_invocant_param);
        if all_invocant_only {
            // Hot path for methods like `method m { ... }` where only the implicit invocant
            // is present in signature metadata. Reject any explicit user arguments quickly.
            for arg in args {
                match arg {
                    Value::Pair(key, _) if key == TEST_CALLSITE_LINE_KEY => {}
                    Value::ValuePair(key, _) => {
                        if let Value::Str(name) = key.as_ref() {
                            if name.as_str() != TEST_CALLSITE_LINE_KEY {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                    _ => return false,
                }
            }
            return self.args_match_param_types(args, &[]);
        }

        let filtered_storage;
        let filtered_params: &[ParamDef] = if param_defs.iter().any(is_invocant_param) {
            filtered_storage = param_defs
                .iter()
                .filter(|p| !is_invocant_param(p))
                .cloned()
                .collect::<Vec<_>>();
            filtered_storage.as_slice()
        } else {
            // Common path: method signatures without explicit invocant metadata.
            param_defs
        };
        let positional_params: Vec<&ParamDef> =
            filtered_params.iter().filter(|p| !p.named).collect();
        let positional_arg_count = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(..)))
            .count();
        let mut required = 0usize;
        let mut has_positional_slurpy = false;
        let mut has_hash_slurpy = false;
        for pd in &positional_params {
            if pd.slurpy {
                if pd.sigilless {
                    // Capture parameter (|c) absorbs both positional and named args
                    has_positional_slurpy = true;
                    has_hash_slurpy = true;
                } else if pd.name.starts_with('%') {
                    has_hash_slurpy = true;
                } else {
                    has_positional_slurpy = true;
                }
            } else if pd.default.is_none() && !pd.optional_marker {
                required += 1;
            }
        }
        let max_positional = positional_params.iter().filter(|p| !p.slurpy).count();
        if has_positional_slurpy {
            if positional_arg_count < required {
                return false;
            }
        } else if positional_arg_count < required || positional_arg_count > max_positional {
            return false;
        }
        if !has_hash_slurpy {
            let named_params: std::collections::HashSet<&str> = filtered_params
                .iter()
                .filter(|p| p.named)
                .map(|p| {
                    // Strip sigil for named params with array/hash sigils: :@l -> "l", :%h -> "h"
                    p.name
                        .strip_prefix('@')
                        .or_else(|| p.name.strip_prefix('%'))
                        .unwrap_or(p.name.as_str())
                })
                .collect();
            for arg in args {
                if let Value::Pair(key, _) = arg
                    && key != TEST_CALLSITE_LINE_KEY
                    && !named_params.contains(key.as_str())
                {
                    return false;
                }
                if let Value::ValuePair(key, _) = arg
                    && let Value::Str(name) = key.as_ref()
                    && name.as_str() != TEST_CALLSITE_LINE_KEY
                    && !named_params.contains(name.as_str())
                {
                    return false;
                }
            }
        }
        if !self.args_match_param_types(args, filtered_params) {
            return false;
        }
        true
    }

    /// Create an error for calling a sub with empty signature `()` with arguments.
    pub(crate) fn reject_args_for_empty_sig(args: &[Value]) -> RuntimeError {
        if let Some(k) = args.iter().find_map(|a| match a {
            Value::Pair(key, _) if key != TEST_CALLSITE_LINE_KEY => Some(key.clone()),
            // ValuePair is a positional pair (parenthesized), not a named arg
            _ => None,
        }) {
            return RuntimeError::new(format!("Unexpected named argument '{}' passed", k));
        }
        RuntimeError::new(
            "Too many positionals passed; expected 0 arguments but got more".to_string(),
        )
    }

    pub(crate) fn bind_function_args_values(
        &mut self,
        param_defs: &[ParamDef],
        params: &[String],
        args: &[Value],
    ) -> Result<Vec<(String, String)>, RuntimeError> {
        let filtered_args: Vec<Value> = args
            .iter()
            .filter(|arg| !is_internal_named_arg(&unwrap_varref_value((*arg).clone())))
            .cloned()
            .collect();
        let plain_args: Vec<Value> = filtered_args
            .iter()
            .cloned()
            .map(unwrap_varref_value)
            .collect();
        // Always set @_ for legacy Perl-style argument access
        self.env
            .insert("@_".to_string(), Value::array(plain_args.clone()));
        let args = filtered_args.as_slice();
        let arg_sources = self.take_pending_call_arg_sources();
        let mut rw_bindings = Vec::new();
        let mut raw_nonlvalue_params: Vec<String> = Vec::new();
        let mut raw_slurpy_sources = std::collections::HashSet::new();
        if param_defs.is_empty() {
            if params.is_empty() {
                // No param_defs and no placeholder params — nothing to bind.
                // Argument rejection (for named subs with empty signature) is handled
                // by callers that set `empty_sig` on FunctionDef / CompiledFunction.
                return Ok(rw_bindings);
            }
            // Legacy path: bind positional placeholders ($^a, $^b) by position,
            // and named placeholders ($:name) by matching Pair arg keys.
            let positional_args: Vec<Value> = plain_args
                .iter()
                .filter(|a| match a {
                    Value::Pair(..) => false,
                    Value::ValuePair(key, _) => !matches!(key.as_ref(), Value::Str(..)),
                    _ => true,
                })
                .cloned()
                .collect();
            let named_args: Vec<(String, Value)> = plain_args
                .iter()
                .filter_map(|a| {
                    if let Value::Pair(key, val) = a {
                        Some((key.clone(), *val.clone()))
                    } else if let Value::ValuePair(key, val) = a {
                        if let Value::Str(name) = key.as_ref() {
                            Some((name.to_string(), *val.clone()))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect();
            let mut consumed_named = std::collections::HashSet::new();
            let mut positional_idx = 0usize;
            for param in params.iter() {
                // Named placeholder: $:f, @:f, %:f — match by Pair key
                let named_key = param
                    .strip_prefix(':')
                    .or_else(|| param.strip_prefix("@:"))
                    .or_else(|| param.strip_prefix("%:"));
                if let Some(key) = named_key {
                    if let Some((_, val)) = named_args.iter().find(|(k, _)| k == key) {
                        self.bind_param_value(param, val.clone());
                        // Also bind the bare :key for GetArrayVar/GetHashVar fallback
                        self.env.insert(format!(":{}", key), val.clone());
                        consumed_named.insert(key.to_string());
                    }
                } else if positional_idx < positional_args.len() {
                    let value = positional_args[positional_idx].clone();
                    if param.starts_with("&^") && !self.type_matches_value("Callable", &value) {
                        return Err(RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Callable, got {}",
                            param,
                            super::value_type_name(&value)
                        )));
                    }
                    if param.starts_with("@^") && !self.type_matches_value("Positional", &value) {
                        return Err(RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Positional, got {}",
                            param,
                            super::value_type_name(&value)
                        )));
                    }
                    if param.starts_with("%^") && !self.type_matches_value("Associative", &value) {
                        return Err(RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Associative, got {}",
                            param,
                            super::value_type_name(&value)
                        )));
                    }
                    self.bind_param_value(param, value);
                    positional_idx += 1;
                } else if param.starts_with('^')
                    || param.starts_with("@^")
                    || param.starts_with("%^")
                    || param.starts_with("&^")
                {
                    return Err(RuntimeError::new(format!(
                        "Missing required implicit placeholder parameter ${}",
                        param
                    )));
                }
            }
            self.env.insert(
                "@_".to_string(),
                Value::array(positional_args[positional_idx..].to_vec()),
            );
            let mut leftover_named = std::collections::HashMap::new();
            for (key, val) in named_args {
                if !consumed_named.contains(&key) {
                    leftover_named.insert(key, val);
                }
            }
            self.env
                .insert("%_".to_string(), Value::hash(leftover_named));
            return Ok(rw_bindings);
        }
        // Pre-compute the set of explicit named parameter keys so that
        // slurpy hash (*%rest) can exclude args already bound to named params.
        let explicit_named_keys: std::collections::HashSet<String> = param_defs
            .iter()
            .filter(|pd| (pd.named || pd.name.starts_with(':')) && !pd.slurpy)
            .map(|pd| {
                let name = if pd.name.starts_with(':') {
                    &pd.name[1..]
                } else if let Some(rest) = pd
                    .name
                    .strip_prefix("@:")
                    .or_else(|| pd.name.strip_prefix("%:"))
                {
                    rest
                } else if pd.named {
                    let after_sigil = pd
                        .name
                        .strip_prefix('@')
                        .or_else(|| pd.name.strip_prefix('%'))
                        .unwrap_or(&pd.name);
                    after_sigil
                        .strip_prefix('!')
                        .or_else(|| after_sigil.strip_prefix('.'))
                        .unwrap_or(after_sigil)
                } else {
                    &pd.name
                };
                name.to_string()
            })
            .collect();
        let mut positional_idx = 0usize;
        for pd in param_defs {
            if pd.slurpy {
                let is_hash_slurpy = pd.name.starts_with('%');
                if pd.sigilless {
                    // |c — capture parameter: preserve positional and named parts.
                    let mut positional = Vec::new();
                    let mut named = std::collections::HashMap::new();
                    for arg in args[positional_idx..].iter().cloned() {
                        let arg = unwrap_varref_value(arg);
                        if let Value::Pair(key, val) = arg {
                            named.insert(key, *val);
                        } else {
                            positional.push(arg);
                        }
                    }
                    let capture_value = Value::Capture { positional, named };
                    if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, capture_value.clone());
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                    if let Some(sub_params) = &pd.sub_signature {
                        bind_sub_signature_from_value(self, sub_params, &capture_value)?;
                    }
                } else if is_hash_slurpy {
                    // *%hash — collect Pair arguments into a hash,
                    // excluding args already bound to explicit named parameters.
                    let mut hash_items = std::collections::HashMap::new();
                    for arg in args.iter() {
                        let arg = unwrap_varref_value(arg.clone());
                        if let Value::Pair(k, v) = arg
                            && !explicit_named_keys.contains(&k)
                        {
                            hash_items.insert(k.clone(), *v.clone());
                        }
                    }
                    if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, Value::hash(hash_items));
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                } else if pd.double_slurpy {
                    // **@ (non-flattening slurpy): keep each argument as-is, skip Pairs
                    let mut items = Vec::new();
                    while positional_idx < args.len() {
                        let val = unwrap_varref_value(args[positional_idx].clone());
                        if !matches!(&val, Value::Pair(..)) {
                            items.push(val);
                        }
                        positional_idx += 1;
                    }
                    if !pd.name.is_empty() {
                        let key = if pd.name.starts_with('@') {
                            pd.name.clone()
                        } else {
                            format!("@{}", pd.name)
                        };
                        self.bind_param_value(&key, Value::real_array(items));
                        self.set_var_type_constraint(&key, pd.type_constraint.clone());
                    }
                } else {
                    let mut items = Vec::new();
                    let is_raw_slurpy = pd.traits.iter().any(|t| t == "raw");
                    while positional_idx < args.len() {
                        let raw_arg = args[positional_idx].clone();
                        if is_raw_slurpy
                            && let Some((source_name, source_value, source_index)) =
                                indexed_varref_from_value(&raw_arg)
                        {
                            if raw_slurpy_sources.insert(source_name.clone()) {
                                rw_bindings.push((source_name.clone(), source_name.clone()));
                            }
                            self.env.insert(source_name.clone(), source_value.clone());
                            if !pd.double_slurpy
                                && let Value::Array(arr, kind) = &source_value
                                && !kind.is_itemized()
                            {
                                for (idx, item) in arr.iter().cloned().enumerate() {
                                    items.push(make_varref_value(
                                        source_name.clone(),
                                        item,
                                        Some(idx),
                                    ));
                                }
                            } else {
                                items.push(make_varref_value(
                                    source_name,
                                    source_value,
                                    source_index,
                                ));
                            }
                            positional_idx += 1;
                            continue;
                        }
                        // *@ (flattening slurpy): recursively flatten list args
                        // but preserve itemized Arrays ($[...] / .item) as single values.
                        // Skip Pair values — they are named args for *%_ or will be rejected
                        match unwrap_varref_value(raw_arg) {
                            Value::Pair(..) => {
                                // Named arg — leave for *%_ slurpy or post-loop check
                            }
                            Value::Array(arr, kind) => {
                                if kind.is_itemized() {
                                    items.push(Value::Array(arr.clone(), kind));
                                } else {
                                    flatten_into_slurpy(&arr, &mut items);
                                }
                            }
                            other => {
                                items.push(other);
                            }
                        }
                        positional_idx += 1;
                    }
                    let slurpy_value = Value::real_array(items);
                    if !pd.name.is_empty() {
                        let key = if pd.name.starts_with('@') {
                            pd.name.clone()
                        } else {
                            format!("@{}", pd.name)
                        };
                        self.bind_param_value(&key, slurpy_value.clone());
                        self.set_var_type_constraint(&key, pd.type_constraint.clone());
                    }
                    // Unpack sub-signature from the slurpy array (e.g., *[$a, $b, $c])
                    if let Some(sub_params) = &pd.sub_signature {
                        bind_sub_signature_from_value(self, sub_params, &slurpy_value)?;
                    }
                }
            } else if pd.named || pd.name.starts_with(':') {
                // Look for a matching named argument (Pair) in args
                let match_key = if pd.name.starts_with(':') {
                    &pd.name[1..]
                } else if let Some(rest) = pd
                    .name
                    .strip_prefix("@:")
                    .or_else(|| pd.name.strip_prefix("%:"))
                {
                    rest
                } else if pd.named {
                    // Named params like :@l or :%h have name "@l" or "%h";
                    // strip the sigil to match the Pair key "l" or "h".
                    // Also strip twigil prefixes: :$!x has name "!x", :$.x has name ".x",
                    // :@!types has name "@!types" — match against Pair key "types".
                    // First strip sigil (@, %), then strip twigil (!, .).
                    let after_sigil = pd
                        .name
                        .strip_prefix('@')
                        .or_else(|| pd.name.strip_prefix('%'))
                        .unwrap_or(&pd.name);
                    after_sigil
                        .strip_prefix('!')
                        .or_else(|| after_sigil.strip_prefix('.'))
                        .unwrap_or(after_sigil)
                } else {
                    &pd.name
                };
                let mut found = false;
                for arg in args {
                    let arg = unwrap_varref_value(arg.clone());
                    if let Value::Pair(key, val) = arg
                        && key == match_key
                    {
                        if let Some((sig_params, sig_ret)) = &pd.code_signature
                            && !code_signature_matches_value(self, sig_params, sig_ret, &val)
                        {
                            let mut err = RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Callable with matching signature, got {}",
                                pd.name,
                                super::value_type_name(&val)
                            ));
                            let mut ex_attrs = std::collections::HashMap::new();
                            ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                            let exception = Value::make_instance(
                                Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                ex_attrs,
                            );
                            err.exception = Some(Box::new(exception));
                            return Err(err);
                        }
                        self.bind_param_value(&pd.name, *val.clone());
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                        if let Some(sub_params) = &pd.sub_signature {
                            bind_named_rename_sub_signature(self, sub_params, &val)?;
                        }
                        found = true;
                        break;
                    }
                }
                // Alias matching: for :a(:$b), also accept b => val
                if !found && let Some(sub_params) = &pd.sub_signature {
                    for sub_pd in sub_params {
                        if found {
                            break;
                        }
                        if !sub_pd.named {
                            continue;
                        }
                        let inner_key = sub_pd.name.strip_prefix(':').unwrap_or(&sub_pd.name);
                        for arg in args.iter() {
                            let arg = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, inner_val) = arg
                                && key == inner_key
                            {
                                self.bind_param_value(&pd.name, *inner_val.clone());
                                self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                                bind_named_rename_sub_signature(self, sub_params, &inner_val)?;
                                found = true;
                                break;
                            }
                        }
                    }
                }
                if !found && let Some(default_expr) = &pd.default {
                    let value = self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?;
                    let value = self.checked_default_param_value(pd, value)?;
                    if let Some((sig_params, sig_ret)) = &pd.code_signature
                        && !code_signature_matches_value(self, sig_params, sig_ret, &value)
                    {
                        let mut err = RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Callable with matching signature, got {}",
                            pd.name,
                            super::value_type_name(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, value);
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                } else if !found && pd.required {
                    return Err(RuntimeError::new(format!(
                        "Required named parameter '{}' not passed",
                        pd.name
                    )));
                } else if !found && !pd.name.is_empty() {
                    // Only bind a default if the env doesn't already have a value
                    // (e.g. BUILD/TWEAK attribute bindings pre-populate the env).
                    if !self.env.contains_key(&pd.name) {
                        let value = Self::missing_optional_param_value(pd);
                        self.bind_param_value(&pd.name, value);
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                }
            } else {
                // Positional param — skip over Value::Pair entries (named args)
                while positional_idx < args.len()
                    && matches!(
                        unwrap_varref_value(args[positional_idx].clone()),
                        Value::Pair(..)
                    )
                {
                    positional_idx += 1;
                }
                if positional_idx < args.len() {
                    if pd.name == "__subsig__"
                        && let Some(sub_params) = &pd.sub_signature
                    {
                        let capture =
                            sub_signature_target_from_remaining_args(&args[positional_idx..]);
                        bind_sub_signature_from_value(self, sub_params, &capture)?;
                        positional_idx = args.len();
                        continue;
                    }
                    let is_rw = pd.traits.iter().any(|t| t == "rw");
                    let is_raw = pd.traits.iter().any(|t| t == "raw");
                    if is_rw || is_raw {
                        let source_name = arg_sources
                            .as_ref()
                            .and_then(|names| names.get(positional_idx))
                            .and_then(|name| name.as_ref())
                            .cloned();
                        if let Some(source_name) = source_name {
                            rw_bindings.push((pd.name.clone(), source_name));
                        } else if is_rw {
                            return Err(RuntimeError::new(format!(
                                "X::Parameter::RW: '{}' expects a writable variable argument",
                                pd.name
                            )));
                        } else {
                            // is raw with non-lvalue: param stays readonly
                            // (not added to rw_bindings, will be marked readonly below)
                            raw_nonlvalue_params.push(pd.name.clone());
                        }
                    }
                    let raw_arg = args[positional_idx].clone();
                    let source_type_constraint = varref_from_value(&raw_arg)
                        .and_then(|(source_name, _)| self.var_type_constraint(&source_name));
                    let bound_type_constraint =
                        source_type_constraint.or_else(|| pd.type_constraint.clone());
                    let mut value = unwrap_varref_value(raw_arg.clone());
                    if pd.sigilless {
                        let alias_key = sigilless_alias_key(&pd.name);
                        let readonly_key = sigilless_readonly_key(&pd.name);
                        if let Some((source_name, inner)) = varref_from_value(&raw_arg) {
                            let resolved_source =
                                self.resolve_sigilless_alias_source_name(&source_name);
                            value = inner;
                            self.env.insert(alias_key, Value::str(resolved_source));
                            self.env.insert(readonly_key, Value::Bool(false));
                        } else if let Some(source_name) = arg_sources
                            .as_ref()
                            .and_then(|names| names.get(positional_idx))
                            .and_then(|name| name.as_ref())
                            .cloned()
                        {
                            let resolved_source =
                                self.resolve_sigilless_alias_source_name(&source_name);
                            if let Some(source_val) = self.env.get(&resolved_source).cloned() {
                                value = source_val;
                                self.env.insert(alias_key, Value::str(resolved_source));
                                self.env.insert(readonly_key, Value::Bool(false));
                            } else {
                                self.env.remove(&alias_key);
                                self.env.insert(readonly_key, Value::Bool(true));
                            }
                        } else {
                            self.env.remove(&alias_key);
                            self.env.insert(readonly_key, Value::Bool(true));
                        }
                    }
                    if let Some(constraint) = &pd.type_constraint
                        && (pd.name != "__type_only__" || self.is_resolvable_type(constraint))
                    {
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        if let Some(captured_name) = constraint.strip_prefix("::") {
                            self.bind_type_capture(captured_name, &value);
                        } else if let Some((target, source)) = parse_coercion_type(constraint) {
                            // Coercion type: check source type if specified, then coerce
                            if let Some(src) = source
                                && !self.type_matches_value(src, &value)
                            {
                                let mut err = RuntimeError::new(format!(
                                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                                    pd.name,
                                    constraint,
                                    super::value_type_name(&value)
                                ));
                                let mut ex_attrs = std::collections::HashMap::new();
                                ex_attrs
                                    .insert("message".to_string(), Value::str(err.message.clone()));
                                let exception = Value::make_instance(
                                    Symbol::intern("X::TypeCheck::Binding::Parameter"),
                                    ex_attrs,
                                );
                                err.exception = Some(Box::new(exception));
                                return Err(err);
                            }
                            let original = value.clone();
                            value = self.try_coerce_value_for_constraint(constraint, value)?;
                            if !self.type_matches_value(target, &value) {
                                return Err(coerce_impossible_error(constraint, &original));
                            }
                        } else if pd.name.starts_with('@') {
                            let ok = match &value {
                                Value::Array(items, ..) => {
                                    items.iter().all(|v| self.type_matches_value(constraint, v))
                                }
                                Value::Slip(items) => {
                                    items.iter().all(|v| self.type_matches_value(constraint, v))
                                }
                                _ => false,
                            };
                            if !ok {
                                return Err(RuntimeError::new(format!(
                                    "{}: Type check failed for {}: expected {}, got {}",
                                    type_error_kind,
                                    pd.name,
                                    constraint,
                                    super::value_type_name(&value)
                                )));
                            }
                        } else if pd.name.starts_with('%') {
                            let ok = match &value {
                                Value::Hash(map) => {
                                    map.values().all(|v| self.type_matches_value(constraint, v))
                                }
                                Value::Array(items, ..) => items.iter().all(|item| {
                                    if let Value::Pair(_, v) = item {
                                        self.type_matches_value(constraint, v)
                                    } else {
                                        false
                                    }
                                }),
                                _ => false,
                            };
                            if !ok {
                                return Err(RuntimeError::new(format!(
                                    "{}: Type check failed for {}: expected {}, got {}",
                                    type_error_kind,
                                    pd.name,
                                    constraint,
                                    super::value_type_name(&value)
                                )));
                            }
                        } else if constraint == "Num"
                            && matches!(
                                value,
                                Value::Int(_)
                                    | Value::Num(_)
                                    | Value::Rat(_, _)
                                    | Value::FatRat(_, _)
                                    | Value::BigRat(_, _)
                            )
                        {
                            // Binding accepts numeric widening into Num parameters.
                        } else if !self.type_matches_value(constraint, &value) {
                            let display_name = if pd.name == "__type_only__" {
                                format!("parameter '{}'", constraint)
                            } else {
                                pd.name.clone()
                            };
                            return Err(RuntimeError::new(format!(
                                "{}: Type check failed for {}: expected {}, got {}",
                                type_error_kind,
                                display_name,
                                constraint,
                                super::value_type_name(&value)
                            )));
                        } else {
                            value = self.try_coerce_value_for_constraint(constraint, value)?;
                        }
                        if (constraint.starts_with("Associative[")
                            || constraint.starts_with("Hash["))
                            && let Value::Array(items, ..) = &value
                        {
                            let mut map = std::collections::HashMap::new();
                            for item in items.iter() {
                                if let Value::Pair(k, v) = item {
                                    map.insert(k.clone(), *v.clone());
                                }
                            }
                            value = Value::hash(map);
                        }
                    }
                    // Implicit Any constraint: untyped $ parameters default to Any,
                    // which rejects Junction (a direct subtype of Mu, not Any).
                    if pd.type_constraint.is_none()
                        && !pd.name.starts_with('@')
                        && !pd.name.starts_with('%')
                        && !pd.name.starts_with('&')
                        && !pd.slurpy
                        && !self.type_matches_value("Any", &value)
                    {
                        let mut err = RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Any but got {} ({})",
                            if pd.name.is_empty() {
                                "<anon>"
                            } else {
                                &pd.name
                            },
                            super::value_type_name(&value),
                            super::utils::gist_value(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception =
                            Value::make_instance(Symbol::intern("X::TypeCheck::Binding"), ex_attrs);
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    // Implicit Positional constraint: untyped @-sigiled parameters
                    // require the argument to be Positional (Array, List, etc.).
                    if pd.type_constraint.is_none()
                        && pd.name.starts_with('@')
                        && !pd.slurpy
                        && !self.type_matches_value("Positional", &value)
                    {
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        let mut err = RuntimeError::new(format!(
                            "{}: Type check failed in binding to parameter '{}'; expected Positional but got {} ({})",
                            type_error_kind,
                            pd.name,
                            super::value_type_name(&value),
                            super::utils::gist_value(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    // Implicit Associative constraint: untyped %-sigiled parameters
                    // require the argument to be Associative (Hash, Map, etc.).
                    if pd.type_constraint.is_none()
                        && pd.name.starts_with('%')
                        && !pd.slurpy
                        && !self.type_matches_value("Associative", &value)
                    {
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        let mut err = RuntimeError::new(format!(
                            "{}: Type check failed in binding to parameter '{}'; expected Associative but got {} ({})",
                            type_error_kind,
                            pd.name,
                            super::value_type_name(&value),
                            super::utils::gist_value(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    if let Some((sig_params, sig_ret)) = &pd.code_signature
                        && !code_signature_matches_value(self, sig_params, sig_ret, &value)
                    {
                        let mut err = RuntimeError::new(format!(
                            "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected Callable with matching signature, got {}",
                            pd.name,
                            super::value_type_name(&value)
                        ));
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(err.message.clone()));
                        let exception = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Binding::Parameter"),
                            ex_attrs,
                        );
                        err.exception = Some(Box::new(exception));
                        return Err(err);
                    }
                    if let Some(where_expr) = &pd.where_constraint {
                        let saved_param = if pd.name.is_empty() {
                            None
                        } else {
                            self.env.get(&pd.name).cloned()
                        };
                        if !pd.name.is_empty() {
                            self.bind_param_value(&pd.name, value.clone());
                        }
                        let saved_topic = self.env.get("_").cloned();
                        self.env.insert("_".to_string(), value.clone());
                        let ok = match where_expr.as_ref() {
                            Expr::AnonSub { body, .. } => self
                                .eval_block_value(body)
                                .map(|v| v.truthy())
                                .unwrap_or(false),
                            expr => self
                                .eval_block_value(&[Stmt::Expr(expr.clone())])
                                .map(|v| self.smart_match(&value, &v))
                                .unwrap_or(false),
                        };
                        if let Some(previous) = saved_topic {
                            self.env.insert("_".to_string(), previous);
                        } else {
                            self.env.remove("_");
                        }
                        if !pd.name.is_empty() {
                            if let Some(previous) = saved_param {
                                self.env.insert(pd.name.clone(), previous);
                            } else {
                                self.env.remove(&pd.name);
                            }
                        }
                        if !ok {
                            return Err(RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: where constraint failed for parameter '{}'",
                                pd.name
                            )));
                        }
                    }
                    if !pd.name.is_empty()
                        && pd.name != "__type_only__"
                        && !pd.name.starts_with("__type_capture__")
                    {
                        if pd.name.starts_with('%')
                            && let Value::Array(items, ..) = &value
                        {
                            let mut map = std::collections::HashMap::new();
                            for item in items.iter() {
                                if let Value::Pair(k, v) = item {
                                    map.insert(k.clone(), *v.clone());
                                }
                            }
                            self.bind_param_value(&pd.name, Value::hash(map));
                            self.set_var_type_constraint(&pd.name, bound_type_constraint.clone());
                            if let Some(sub_params) = &pd.sub_signature {
                                let target = self.env.get(&pd.name).cloned().unwrap_or(Value::Nil);
                                bind_sub_signature_from_value(self, sub_params, &target)?;
                            }
                            positional_idx += 1;
                            continue;
                        }
                        // Shape constraint check for array parameters
                        if let Some(shape_exprs) = &pd.shape_constraints {
                            self.check_shape_constraint(&pd.name, &value, shape_exprs, args)?;
                        }
                        self.bind_param_value(&pd.name, value);
                        self.set_var_type_constraint(&pd.name, bound_type_constraint.clone());
                    }
                    if let Some(sub_params) = &pd.sub_signature {
                        let target = self
                            .env
                            .get(&pd.name)
                            .cloned()
                            .unwrap_or_else(|| args[positional_idx].clone());
                        bind_sub_signature_from_value(self, sub_params, &target)?;
                    }
                    positional_idx += 1;
                } else if let Some(default_expr) = &pd.default {
                    let value = self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?;
                    let value = self.checked_default_param_value(pd, value)?;
                    if !pd.name.is_empty() {
                        self.bind_param_value(&pd.name, value);
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                    }
                    if let Some(sub_params) = &pd.sub_signature {
                        let target = self.env.get(&pd.name).cloned().unwrap_or(Value::Nil);
                        bind_sub_signature_from_value(self, sub_params, &target)?;
                    }
                } else if !pd.required && !pd.name.is_empty() {
                    // Optional parameters use typed empties/type objects when omitted.
                    self.bind_param_value(&pd.name, Self::missing_optional_param_value(pd));
                    self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                }
            }
        }
        // Check for unexpected named arguments when no hash/capture slurpy is present
        let has_hash_slurpy = param_defs
            .iter()
            .any(|pd| pd.slurpy && (pd.name.starts_with('%') || pd.sigilless));
        // Only skip for positional sub-signatures (unpacking dispatch), not named renaming
        let has_positional_sub_sig = param_defs
            .iter()
            .any(|pd| !pd.named && pd.sub_signature.is_some());
        if !has_hash_slurpy && !has_positional_sub_sig {
            for arg in plain_args.iter() {
                let arg = unwrap_varref_value(arg.clone());
                if let Value::Pair(key, _) = arg {
                    // Check if this named arg was consumed by a named param or colon placeholder
                    let consumed = param_defs.iter().any(|pd| {
                        if (pd.named && pd.name == key)
                            || pd.name == format!(":{}", key)
                            || pd.name == format!("@:{}", key)
                            || pd.name == format!("%:{}", key)
                            // Named params with sigils: :@l has name "@l", match key "l"
                            || (pd.named && (pd.name == format!("@{}", key) || pd.name == format!("%{}", key)))
                        {
                            return true;
                        }
                        // Also check inner named aliases from sub-signatures
                        if let Some(sub_params) = &pd.sub_signature {
                            for sp in sub_params {
                                if sp.named && sp.name == key {
                                    return true;
                                }
                            }
                        }
                        false
                    });
                    if !consumed {
                        return Err(RuntimeError::new(format!(
                            "Unexpected named argument '{}' passed",
                            key
                        )));
                    }
                }
                // Note: ValuePair is a positional pair (e.g. from parenthesized (:a(3))),
                // so it's NOT treated as a named argument.
            }
        }
        // Check for extra positional arguments when no array/capture slurpy is present
        let has_array_slurpy = param_defs
            .iter()
            .any(|pd| pd.slurpy && (!pd.name.starts_with('%') || pd.sigilless));
        if !has_array_slurpy && !has_positional_sub_sig {
            let positional_param_count = param_defs
                .iter()
                .filter(|pd| !pd.named && !pd.slurpy)
                .count();
            let positional_arg_count = plain_args
                .iter()
                .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                .count();
            if positional_arg_count > positional_param_count {
                return Err(RuntimeError::new(format!(
                    "Too many positionals passed; expected {} arguments but got {}",
                    positional_param_count, positional_arg_count
                )));
            }
        }
        // Mark parameters as readonly unless they have `is rw`, `is copy`, or `is raw` traits.
        // Sigilless params (\x) are always writable (they are raw aliases).
        // For `is raw` with non-lvalue args, also mark readonly.
        for pd in param_defs {
            if pd.name.is_empty() || pd.name == "__type_only__" || pd.name == "__subsig__" {
                continue;
            }
            if pd.sigilless {
                continue; // Sigilless params are raw aliases, always writable
            }
            if pd.name.starts_with('!') || pd.name.starts_with('.') {
                continue; // Attribute-binding params ($!attr, $.attr) are always writable
            }
            let has_mutable_trait = pd
                .traits
                .iter()
                .any(|t| t == "rw" || t == "copy" || t == "raw");
            if !has_mutable_trait || raw_nonlvalue_params.contains(&pd.name) {
                self.readonly_vars.insert(pd.name.clone());
            }
        }
        Ok(rw_bindings)
    }

    /// Coerce a value to the target type, trying built-in coercions first,
    /// then falling back to target class COERCE when available.
    fn try_coerce_value_with_method(
        &mut self,
        target: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let base_target =
            if target.ends_with(":D") || target.ends_with(":U") || target.ends_with(":_") {
                &target[..target.len() - 2]
            } else {
                target
            };
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && self.class_has_method(&class_name.resolve(), base_target)
        {
            let (coerced, _) = self.run_instance_method(
                &class_name.resolve(),
                (**attributes).clone(),
                base_target,
                vec![],
                Some(value.clone()),
            )?;
            if self.type_matches_value(base_target, &coerced) {
                return Ok(coerced);
            }
            return Err(coerce_impossible_error(target, &value));
        }
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|c| c == "Str")
            && let Some(inner) = attributes.get("value").cloned()
            && !matches!(inner, Value::Nil)
            && let Ok(coerced) = self.try_coerce_value_with_method(target, inner)
            && self.type_matches_value(base_target, &coerced)
        {
            return Ok(coerced);
        }
        if let Some(variants) = self.enum_types.get(base_target).cloned()
            && let Some(enum_value) =
                self.coerce_to_enum_variant(base_target, &variants, value.clone())
        {
            return Ok(enum_value);
        }
        let result = coerce_value(target, value.clone());
        if self.type_matches_value(base_target, &result) {
            return Ok(result);
        }
        if let Ok(coerced) = self.call_method_with_values(value.clone(), base_target, vec![]) {
            if self.type_matches_value(base_target, &coerced) {
                return Ok(coerced);
            }
            return Err(coerce_impossible_error(target, &value));
        }
        if self.classes.contains_key(base_target) {
            // Wrap Pair values in a Scalar container so they are passed as
            // positional arguments to COERCE/new rather than being flattened
            // into named arguments by the method dispatch logic.
            let coerce_arg = match &value {
                Value::Pair(..) | Value::ValuePair(..) => Value::Scalar(Box::new(value.clone())),
                _ => value.clone(),
            };
            // Try COERCE method first
            if let Ok(coerced) = self.call_method_with_values(
                Value::Package(Symbol::intern(base_target)),
                "COERCE",
                vec![coerce_arg.clone()],
            ) {
                if self.type_matches_value(base_target, &coerced) {
                    return Ok(coerced);
                }
                return Err(coerce_impossible_error(target, &value));
            }
            // Fallback: try calling `new` on the target class with the value,
            // but only if there's an explicit `new` variant that accepts a
            // positional parameter matching the value type.  The default
            // constructor (named-only params) must NOT be used for coercion.
            if self.class_has_new_accepting_positional(base_target, &value)
                && let Ok(coerced) = self.call_method_with_values(
                    Value::Package(Symbol::intern(base_target)),
                    "new",
                    vec![coerce_arg],
                )
                && self.type_matches_value(base_target, &coerced)
            {
                return Ok(coerced);
            }
        }
        Err(coerce_impossible_error(target, &value))
    }

    pub(crate) fn coerce_value_for_constraint(&mut self, constraint: &str, value: Value) -> Value {
        self.try_coerce_value_for_constraint(constraint, value.clone())
            .unwrap_or(value)
    }

    pub(crate) fn try_coerce_value_for_constraint(
        &mut self,
        constraint: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let (constraint, _) = strip_type_smiley(constraint);
        if let Some((target, source)) = parse_coercion_type(constraint) {
            let intermediate = if let Some(src) = source {
                self.try_coerce_value_for_constraint(src, value)?
            } else {
                value
            };
            let resolved_target = self.resolve_constraint_alias(target);
            return self.try_coerce_value_with_method(&resolved_target, intermediate);
        }
        let resolved_constraint = self.resolve_constraint_alias(constraint);
        if resolved_constraint != constraint {
            return self.try_coerce_value_for_constraint(&resolved_constraint, value);
        }
        if let Some(subset) = self.subsets.get(resolved_constraint.as_str()).cloned()
            && subset.base != resolved_constraint
        {
            return self.try_coerce_value_for_constraint(&subset.base, value);
        }
        Ok(value)
    }

    /// Check shape constraint for array parameters in signatures.
    fn check_shape_constraint(
        &mut self,
        param_name: &str,
        value: &Value,
        shape_exprs: &[Expr],
        all_args: &[Value],
    ) -> Result<(), RuntimeError> {
        let _ = all_args; // reserved for future use
        let actual_shape = crate::runtime::utils::shaped_array_shape(value);

        // Evaluate expected dimensions from shape expressions
        let mut expected_dims: Vec<Option<usize>> = Vec::new();
        for expr in shape_exprs {
            match expr {
                Expr::Whatever | Expr::HyperWhatever => {
                    // * means any size for this dimension
                    expected_dims.push(None);
                }
                _ => {
                    let dim_val = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                    match &dim_val {
                        Value::Whatever | Value::HyperWhatever => {
                            expected_dims.push(None);
                        }
                        Value::Int(n) => {
                            expected_dims.push(Some(*n as usize));
                        }
                        Value::BigInt(n) => {
                            use num_traits::ToPrimitive;
                            expected_dims.push(Some(n.to_usize().unwrap_or(0)));
                        }
                        Value::Num(n) if n.is_infinite() || n.is_nan() => {
                            // Inf/NaN means wildcard (e.g. * coerced to Inf)
                            expected_dims.push(None);
                        }
                        Value::Num(n) => {
                            expected_dims.push(Some(*n as usize));
                        }
                        _ => {
                            let coerced = crate::runtime::utils::coerce_to_numeric(dim_val);
                            match &coerced {
                                Value::Int(n) => expected_dims.push(Some(*n as usize)),
                                Value::Num(n) if n.is_infinite() || n.is_nan() => {
                                    expected_dims.push(None);
                                }
                                _ => expected_dims.push(None),
                            }
                        }
                    }
                }
            }
        }

        // Get actual shape
        let actual = match &actual_shape {
            Some(shape) => shape.clone(),
            None => {
                // Unshaped array - reject unless shape constraint is just [*]
                // (single wildcard dimension accepts unshaped arrays)
                if expected_dims.len() == 1 && expected_dims[0].is_none() {
                    return Ok(());
                }
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected shaped array with {} dimension(s)",
                    param_name,
                    expected_dims.len()
                )));
            }
        };

        // Check number of dimensions matches
        if actual.len() != expected_dims.len() {
            return Err(RuntimeError::new(format!(
                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {} dimension(s), got {}",
                param_name,
                expected_dims.len(),
                actual.len()
            )));
        }

        // Check each dimension
        for (i, (expected, &actual_dim)) in expected_dims.iter().zip(actual.iter()).enumerate() {
            if let Some(expected_dim) = expected
                && *expected_dim != actual_dim
            {
                return Err(RuntimeError::new(format!(
                    "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; dimension {} expected {}, got {}",
                    param_name, i, expected_dim, actual_dim
                )));
            }
        }

        Ok(())
    }
}
