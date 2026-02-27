use super::*;

const TEST_CALLSITE_LINE_KEY: &str = "__mutsu_test_callsite_line";

#[inline]
fn is_internal_named_arg(arg: &Value) -> bool {
    match arg {
        Value::Pair(key, _) => key == TEST_CALLSITE_LINE_KEY,
        Value::ValuePair(key, _) => {
            matches!(key.as_ref(), Value::Str(s) if s == TEST_CALLSITE_LINE_KEY)
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

#[inline]
fn is_coercion_constraint(constraint: &str) -> bool {
    let bytes = constraint.as_bytes();
    bytes.last() == Some(&b')') && bytes.contains(&b'(')
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
            Value::Rat(n, d) => Value::Int(n / d),
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
        "Str" => Value::Str(crate::runtime::utils::coerce_to_str(&value)),
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
    if let Value::Capture { positional, named } = value
        && positional.is_empty()
        && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
        && let Some(inner) = named.get("__mutsu_varref_value")
    {
        return Some((name.clone(), inner.clone()));
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
            out.insert("key".to_string(), Value::Str(key.clone()));
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
    if let Some(v) = named.get(name) {
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
            continue;
        };
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
    /// Save the current readonly_vars set (call before function body execution).
    pub(crate) fn save_readonly_vars(&self) -> HashSet<String> {
        self.readonly_vars.clone()
    }

    /// Restore the readonly_vars set (call after function body execution).
    pub(crate) fn restore_readonly_vars(&mut self, saved: HashSet<String>) {
        self.readonly_vars = saved;
    }

    /// Check if a variable is readonly and return an error if so.
    /// Returns Ok(()) if writable, Err with X::Multi::NoMatch for increment/decrement,
    /// or Err with a generic message for assignment.
    pub(crate) fn check_readonly_for_modify(&self, name: &str) -> Result<(), RuntimeError> {
        if self.readonly_vars.contains(name) {
            let msg = format!("Cannot assign to a readonly variable ({}) or a value", name);
            let mut err = RuntimeError::new(msg.clone());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::Str(msg));
            err.exception = Some(Box::new(Value::make_instance(
                "X::Assignment::RO".to_string(),
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
            attrs.insert("message".to_string(), Value::Str(msg));
            err.exception = Some(Box::new(Value::make_instance(
                "X::Multi::NoMatch".to_string(),
                attrs,
            )));
            return Err(err);
        }
        Ok(())
    }

    pub(crate) fn apply_rw_bindings_to_env(
        &self,
        rw_bindings: &[(String, String)],
        target_env: &mut std::collections::HashMap<String, Value>,
    ) {
        for (param_name, source_name) in rw_bindings {
            if let Some(updated) = self.env.get(param_name).cloned() {
                target_env.insert(source_name.clone(), updated);
            }
        }
    }

    fn bind_param_value(&mut self, name: &str, value: Value) {
        self.env.insert(name.to_string(), value.clone());
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

    fn captured_type_object(value: &Value) -> Value {
        match value {
            Value::Package(name) => Value::Package(name.clone()),
            Value::Instance { class_name, .. } => Value::Package(class_name.clone()),
            Value::Nil => Value::Package("Any".to_string()),
            _ => Value::Package(super::value_type_name(value).to_string()),
        }
    }

    fn optional_type_object_name(constraint: &str) -> String {
        if let Some((target, _source)) = parse_coercion_type(constraint) {
            return target.to_string();
        }
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
            return Value::array(Vec::new());
        }
        if pd.name.starts_with('%') {
            return Value::hash(std::collections::HashMap::new());
        }
        if let Some(constraint) = &pd.type_constraint {
            return Value::Package(Self::optional_type_object_name(constraint));
        }
        Value::Nil
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
            ("NativeEndian".to_string(), 0i64),
            ("LittleEndian".to_string(), 1i64),
            ("BigEndian".to_string(), 2i64),
        ];
        self.enum_types
            .insert("Endian".to_string(), variants.clone());
        self.env
            .insert("Endian".to_string(), Value::Str("Endian".to_string()));
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: "Endian".to_string(),
                key: key.clone(),
                value: *val,
                index,
            };
            // Register as both Endian::NativeEndian and bare NativeEndian
            self.env
                .insert(format!("Endian::{}", key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
    }

    pub(super) fn init_order_enum(&mut self) {
        let variants = vec![
            ("Less".to_string(), -1i64),
            ("Same".to_string(), 0i64),
            ("More".to_string(), 1i64),
        ];
        self.enum_types
            .insert("Order".to_string(), variants.clone());
        self.env
            .insert("Order".to_string(), Value::Str("Order".to_string()));
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: "Order".to_string(),
                key: key.clone(),
                value: *val,
                index,
            };
            self.env.insert(format!("Order::{}", key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
    }

    pub(super) fn init_signal_enum(&mut self) {
        // Use libc constants on Unix, standard POSIX numbers on other platforms
        let variants = vec![
            ("SIGHUP".to_string(), Self::sig_num(1)),
            ("SIGINT".to_string(), Self::sig_num(2)),
            ("SIGQUIT".to_string(), Self::sig_num(3)),
            ("SIGILL".to_string(), Self::sig_num(4)),
            ("SIGABRT".to_string(), Self::sig_num(6)),
            ("SIGFPE".to_string(), Self::sig_num(8)),
            ("SIGKILL".to_string(), Self::sig_num(9)),
            ("SIGSEGV".to_string(), Self::sig_num(11)),
            ("SIGPIPE".to_string(), Self::sig_num(13)),
            ("SIGALRM".to_string(), Self::sig_num(14)),
            ("SIGTERM".to_string(), Self::sig_num(15)),
            ("SIGUSR1".to_string(), Self::sig_num(10)),
            ("SIGUSR2".to_string(), Self::sig_num(12)),
            ("SIGCHLD".to_string(), Self::sig_num(17)),
            ("SIGCONT".to_string(), Self::sig_num(18)),
            ("SIGSTOP".to_string(), Self::sig_num(19)),
            ("SIGTSTP".to_string(), Self::sig_num(20)),
            ("SIGTTIN".to_string(), Self::sig_num(21)),
            ("SIGTTOU".to_string(), Self::sig_num(22)),
        ];
        self.enum_types
            .insert("Signal".to_string(), variants.clone());
        self.env
            .insert("Signal".to_string(), Value::Str("Signal".to_string()));
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: "Signal".to_string(),
                key: key.clone(),
                value: *val,
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
                Self::version_from_value(Value::Str(s))
            }
        }
    }

    pub(super) fn version_smart_match(
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

    pub(super) fn value_is_nan(value: &Value) -> bool {
        match value {
            Value::Num(f) => f.is_nan(),
            Value::Complex(r, i) => r.is_nan() || i.is_nan(),
            Value::Str(s) => s.trim().eq_ignore_ascii_case("nan"),
            _ => false,
        }
    }

    pub(super) fn type_matches(constraint: &str, value_type: &str) -> bool {
        if constraint == "Any" || constraint == "Mu" {
            return true;
        }
        if constraint == value_type {
            return true;
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
        // Numeric hierarchy: Int is a Numeric, Num is a Numeric
        if constraint == "Numeric"
            && matches!(value_type, "Int" | "Num" | "Rat" | "FatRat" | "Complex")
        {
            return true;
        }
        if constraint == "Real" && matches!(value_type, "Int" | "Num" | "Rat" | "FatRat") {
            return true;
        }
        if constraint == "Dateish" && matches!(value_type, "Date" | "DateTime") {
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
            && matches!(value_type, "Sub" | "Routine" | "Method" | "Block")
        {
            return true;
        }
        if constraint == "Routine" && matches!(value_type, "Sub" | "Method" | "Routine") {
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
        false
    }

    /// Check if a type name is known (either a class, role, or enum).
    pub(crate) fn has_type(&self, name: &str) -> bool {
        self.classes.contains_key(name)
            || self.roles.contains_key(name)
            || self.enum_types.contains_key(name)
            || self.subsets.contains_key(name)
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
            return self.is_definite_constraint(&subset.base);
        }
        false
    }

    pub(crate) fn eval_does_values(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some((role_name, args)) = self.extract_role_application(&right) {
            return self.compose_role_on_value(left, &role_name, &args);
        }
        let role_name = right.to_string_value();
        Ok(Value::Bool(left.does_check(&role_name)))
    }

    fn extract_role_application(&self, rhs: &Value) -> Option<(String, Vec<Value>)> {
        match rhs {
            Value::Pair(name, boxed) if self.roles.contains_key(name) => {
                if let Value::Array(args, ..) = boxed.as_ref() {
                    Some((name.clone(), args.as_ref().clone()))
                } else {
                    None
                }
            }
            Value::Package(name) | Value::Str(name) if self.roles.contains_key(name) => {
                Some((name.clone(), Vec::new()))
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
        let role = self
            .roles
            .get(role_name)
            .cloned()
            .ok_or_else(|| RuntimeError::new(format!("Unknown role: {}", role_name)))?;

        let (inner, mut mixins) = match left {
            Value::Mixin(inner, mixins) => (inner, mixins),
            other => (Box::new(other), HashMap::new()),
        };
        mixins.insert(format!("__mutsu_role__{}", role_name), Value::Bool(true));

        for (idx, (attr_name, _is_public, default_expr, _is_rw)) in
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

        Ok(Value::Mixin(inner, mixins))
    }

    pub(crate) fn type_matches_value(&mut self, constraint: &str, value: &Value) -> bool {
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
            && let Some((lhs_base, lhs_inner)) = Self::parse_generic_constraint(package_name)
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
            && let Some((target, source)) = parse_coercion_type(constraint)
        {
            let target_ok = package_matches_type(package_name, target);
            let source_ok = source.is_some_and(|src| self.type_matches_value(src, value));
            return target_ok || source_ok;
        }
        if let Value::Package(package_name) = value
            && package_matches_type(package_name, constraint)
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
                _ => {}
            }
        }
        if let Some(Value::Package(bound)) = self.env.get(constraint).cloned()
            && bound != constraint
        {
            return self.type_matches_value(&bound, value);
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
            let saved = self.env.get("_").cloned();
            self.env.insert("_".to_string(), predicate_value);
            let ok = if let Some(pred) = &subset.predicate {
                self.eval_block_value(&[Stmt::Expr(pred.clone())])
                    .map(|v| v.truthy())
                    .unwrap_or(false)
            } else {
                true
            };
            if let Some(old) = saved {
                self.env.insert("_".to_string(), old);
            } else {
                self.env.remove("_");
            }
            return ok;
        }
        // Role constraints should accept composed role instances/mixins.
        if self.roles.contains_key(constraint) && value.does_check(constraint) {
            return true;
        }
        // Check Instance class name against constraint (including parent classes)
        if let Value::Instance { class_name, .. } = value {
            if Self::type_matches(constraint, class_name) {
                return true;
            }
            // Check parent classes of the instance
            if let Some(class_def) = self.classes.get(class_name.as_str()) {
                for parent in class_def.parents.clone() {
                    if Self::type_matches(constraint, &parent) {
                        return true;
                    }
                }
            }
        }
        // Mixin allomorphic types: check both inner type and mixin type keys
        if let Value::Mixin(inner, mixins) = value {
            if self.type_matches_value(constraint, inner) {
                return true;
            }
            if mixins.contains_key(constraint) {
                return true;
            }
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
            let mut i = 0usize;
            for pd in positional_params {
                let is_capture_param = pd.name == "_capture" || (pd.slurpy && pd.sigilless);
                let is_subsig_capture = pd.name == "__subsig__" && pd.sub_signature.is_some();
                let arg_for_checks: Option<Value> = if pd.slurpy || is_capture_param {
                    if is_capture_param {
                        // |c capture params preserve both positional and named parts.
                        let mut positional = Vec::new();
                        let mut named = std::collections::HashMap::new();
                        for arg in &args[i..] {
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
                        for arg in &args[i..] {
                            let arg = unwrap_varref_value(arg.clone());
                            if !pd.double_slurpy
                                && let Value::Array(arr, is_array) = &arg
                                && !*is_array
                            {
                                items.extend(arr.iter().cloned());
                                continue;
                            }
                            items.push(arg);
                        }
                        Some(Value::array(items))
                    }
                } else if is_subsig_capture {
                    Some(sub_signature_target_from_remaining_args(
                        &args[i..]
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
                if let Some(constraint) = &pd.type_constraint
                    && let Some(arg) = arg_for_checks.as_ref()
                {
                    if let Some(captured_name) = constraint.strip_prefix("::") {
                        self.env
                            .insert(captured_name.to_string(), Self::captured_type_object(arg));
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
                if let Some(sub_params) = &pd.sub_signature {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    if !sub_signature_matches_value(self, sub_params, arg) {
                        return false;
                    }
                }
                if let Some(where_expr) = &pd.where_constraint {
                    let Some(arg) = arg_for_checks.as_ref() else {
                        return false;
                    };
                    let saved = self.env.clone();
                    self.env.insert("_".to_string(), arg.clone());
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
            true
        })();
        self.env = saved_env;
        result
    }

    pub(super) fn method_args_match(&mut self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        let filtered_params: Vec<ParamDef> = param_defs
            .iter()
            .filter(|p| !p.traits.iter().any(|t| t == "invocant"))
            .cloned()
            .collect();
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
            } else {
                required += 1;
            }
        }
        if has_positional_slurpy {
            if positional_arg_count < required {
                return false;
            }
        } else if positional_arg_count != required {
            return false;
        }
        if !has_hash_slurpy {
            let named_params: std::collections::HashSet<&str> = filtered_params
                .iter()
                .filter(|p| p.named)
                .map(|p| p.name.as_str())
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
                    && name != TEST_CALLSITE_LINE_KEY
                    && !named_params.contains(name.as_str())
                {
                    return false;
                }
            }
        }
        if !self.args_match_param_types(args, &filtered_params) {
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
                .filter(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
                .cloned()
                .collect();
            let named_args: Vec<(String, Value)> = plain_args
                .iter()
                .filter_map(|a| {
                    if let Value::Pair(key, val) = a {
                        Some((key.clone(), *val.clone()))
                    } else if let Value::ValuePair(key, val) = a {
                        if let Value::Str(name) = key.as_ref() {
                            Some((name.clone(), *val.clone()))
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
                if let Some(key) = param.strip_prefix(':') {
                    // Named placeholder: match by Pair key
                    if let Some((_, val)) = named_args.iter().find(|(k, _)| k == key) {
                        self.bind_param_value(param, val.clone());
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
                    // *%hash — collect Pair arguments into a hash
                    let mut hash_items = std::collections::HashMap::new();
                    for arg in args.iter() {
                        let arg = unwrap_varref_value(arg.clone());
                        if let Value::Pair(k, v) = arg {
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
                        self.bind_param_value(&key, Value::array(items));
                        self.set_var_type_constraint(&key, pd.type_constraint.clone());
                    }
                } else {
                    let mut items = Vec::new();
                    while positional_idx < args.len() {
                        // *@ (flattening slurpy): flatten list args but preserve
                        // itemized Arrays ($[...] / .item) as single values.
                        // Skip Pair values — they are named args for *%_ or will be rejected
                        match unwrap_varref_value(args[positional_idx].clone()) {
                            Value::Pair(..) => {
                                // Named arg — leave for *%_ slurpy or post-loop check
                            }
                            Value::Array(arr, is_array) => {
                                if is_array {
                                    items.push(Value::Array(arr.clone(), true));
                                } else {
                                    items.extend(arr.iter().cloned());
                                }
                            }
                            other => {
                                items.push(other);
                            }
                        }
                        positional_idx += 1;
                    }
                    let slurpy_value = Value::array(items);
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
                } else {
                    &pd.name
                };
                let mut found = false;
                for arg in args {
                    let arg = unwrap_varref_value(arg.clone());
                    if let Value::Pair(key, val) = arg
                        && key == match_key
                    {
                        self.bind_param_value(&pd.name, *val.clone());
                        self.set_var_type_constraint(&pd.name, pd.type_constraint.clone());
                        if let Some(sub_params) = &pd.sub_signature {
                            bind_sub_signature_from_value(self, sub_params, &val)?;
                        }
                        found = true;
                        break;
                    }
                }
                if !found && let Some(default_expr) = &pd.default {
                    let value = self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?;
                    let value = self.checked_default_param_value(pd, value)?;
                    if !pd.name.is_empty() {
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
                            value = inner;
                            self.env.insert(alias_key, Value::Str(source_name));
                            self.env.insert(readonly_key, Value::Bool(false));
                        } else {
                            self.env.remove(&alias_key);
                            self.env.insert(readonly_key, Value::Bool(true));
                        }
                    }
                    if pd.name != "__type_only__"
                        && let Some(constraint) = &pd.type_constraint
                    {
                        let type_error_kind = "X::TypeCheck::Binding::Parameter";
                        if let Some(captured_name) = constraint.strip_prefix("::") {
                            self.env.insert(
                                captured_name.to_string(),
                                Self::captured_type_object(&value),
                            );
                        } else if let Some((_target, source)) = parse_coercion_type(constraint) {
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
                                    .insert("message".to_string(), Value::Str(err.message.clone()));
                                let exception = Value::make_instance(
                                    "X::TypeCheck::Binding::Parameter".to_string(),
                                    ex_attrs,
                                );
                                err.exception = Some(Box::new(exception));
                                return Err(err);
                            }
                            value = self.try_coerce_value_for_constraint(constraint, value)?;
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
                            return Err(RuntimeError::new(format!(
                                "{}: Type check failed for {}: expected {}, got {}",
                                type_error_kind,
                                pd.name,
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
        // Skip this check when any param has a sub-signature (unpacking dispatch)
        let has_hash_slurpy = param_defs
            .iter()
            .any(|pd| pd.slurpy && (pd.name.starts_with('%') || pd.sigilless));
        let has_sub_sig = param_defs.iter().any(|pd| pd.sub_signature.is_some());
        if !has_hash_slurpy && !has_sub_sig {
            for arg in plain_args.iter() {
                let arg = unwrap_varref_value(arg.clone());
                if let Value::Pair(key, _) = arg {
                    // Check if this named arg was consumed by a named param or colon placeholder
                    let consumed = param_defs
                        .iter()
                        .any(|pd| (pd.named && pd.name == key) || pd.name == format!(":{}", key));
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
        if !has_array_slurpy && !has_sub_sig {
            let positional_param_count = param_defs
                .iter()
                .filter(|pd| !pd.named && !pd.slurpy)
                .count();
            let positional_arg_count = plain_args
                .iter()
                .filter(|a| {
                    !matches!(
                        unwrap_varref_value((*a).clone()),
                        Value::Pair(..) | Value::ValuePair(..)
                    )
                })
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
            && self.class_has_method(class_name, base_target)
        {
            let (coerced, _) = self.run_instance_method(
                class_name,
                (**attributes).clone(),
                base_target,
                vec![],
                Some(value.clone()),
            )?;
            return Ok(coerced);
        }
        let result = coerce_value(target, value.clone());
        if std::mem::discriminant(&result) == std::mem::discriminant(&value) {
            if let Ok(coerced) = self.call_method_with_values(value.clone(), base_target, vec![]) {
                return Ok(coerced);
            }
            if self.classes.contains_key(base_target)
                && let Ok(coerced) = self.call_method_with_values(
                    Value::Package(base_target.to_string()),
                    "COERCE",
                    vec![value],
                )
            {
                return Ok(coerced);
            }
        }
        Ok(result)
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
            return self.try_coerce_value_with_method(target, intermediate);
        }
        if let Some(subset) = self.subsets.get(constraint).cloned()
            && subset.base != constraint
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
