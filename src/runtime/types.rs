use super::*;

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

/// Coerce a value to the target type.
fn coerce_value(target: &str, value: Value) -> Value {
    match target {
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
        "Str" => Value::Str(value.to_string_value()),
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
        _ => true,
    }
}

impl Interpreter {
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
        }
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
            && matches!(value_type, "Sub" | "Routine")
        {
            return true;
        }
        if constraint == "Routine" && matches!(value_type, "Sub" | "Routine") {
            return true;
        }
        // Role-like type relationships
        if constraint == "Positional" && matches!(value_type, "Array" | "List" | "Seq") {
            return true;
        }
        if constraint == "Associative"
            && matches!(value_type, "Hash" | "Map" | "Bag" | "Set" | "Mix")
        {
            return true;
        }
        false
    }

    pub(crate) fn type_matches_value(&mut self, constraint: &str, value: &Value) -> bool {
        // Handle coercion types: Int() matches anything, Int(Rat) matches Rat
        if let Some((_, source)) = parse_coercion_type(constraint) {
            return if let Some(src) = source {
                self.type_matches_value(src, value)
            } else {
                true
            };
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
            let saved = self.env.get("_").cloned();
            self.env.insert("_".to_string(), value.clone());
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
        let value_type = super::value_type_name(value);
        Self::type_matches(constraint, value_type)
    }

    pub(super) fn args_match_param_types(
        &mut self,
        args: &[Value],
        param_defs: &[ParamDef],
    ) -> bool {
        let positional_params: Vec<&ParamDef> = param_defs
            .iter()
            .filter(|p| !p.slurpy && !p.named)
            .collect();
        for (i, pd) in positional_params.iter().enumerate() {
            if let Some(literal) = &pd.literal_value {
                if let Some(arg) = args.get(i) {
                    if arg != literal {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            if let Some(constraint) = &pd.type_constraint
                && let Some(arg) = args.get(i)
            {
                if pd.name == "__type_only__" {
                    // Bare identifier param (e.g., enum value) — resolve from env and compare
                    if let Some(expected_val) = self.env.get(constraint).cloned() {
                        if *arg != expected_val {
                            return false;
                        }
                    } else if !self.type_matches_value(constraint, arg) {
                        return false;
                    }
                } else if !self.type_matches_value(constraint, arg) {
                    return false;
                }
            }
        }
        true
    }

    pub(super) fn method_args_match(&mut self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        let positional_params: Vec<&ParamDef> = param_defs.iter().filter(|p| !p.named).collect();
        let mut required = 0usize;
        let mut has_slurpy = false;
        for pd in &positional_params {
            if pd.slurpy {
                has_slurpy = true;
            } else {
                required += 1;
            }
        }
        if has_slurpy {
            if args.len() < required {
                return false;
            }
        } else if args.len() != required {
            return false;
        }
        self.args_match_param_types(args, param_defs)
    }

    pub(crate) fn bind_function_args_values(
        &mut self,
        param_defs: &[ParamDef],
        params: &[String],
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        // Always set @_ for legacy Perl-style argument access
        self.env
            .insert("@_".to_string(), Value::array(args.to_vec()));
        if param_defs.is_empty() {
            // Legacy path: just bind by position
            for (i, param) in params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    self.env.insert(param.clone(), value.clone());
                }
            }
            return Ok(());
        }
        let mut positional_idx = 0usize;
        for pd in param_defs {
            if pd.slurpy {
                let is_hash_slurpy = pd.name.starts_with('%');
                if pd.sigilless {
                    // |c — capture parameter: collect ALL remaining args
                    // (positional + named Pairs) into an array, stored as
                    // sigilless variable (no sigil prefix)
                    let mut items = Vec::new();
                    while positional_idx < args.len() {
                        items.push(args[positional_idx].clone());
                        positional_idx += 1;
                    }
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), Value::array(items));
                    }
                } else if is_hash_slurpy {
                    // *%hash — collect Pair arguments into a hash
                    let mut hash_items = std::collections::HashMap::new();
                    for arg in args.iter() {
                        if let Value::Pair(k, v) = arg {
                            hash_items.insert(k.clone(), *v.clone());
                        }
                    }
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), Value::hash(hash_items));
                    }
                } else {
                    let mut items = Vec::new();
                    while positional_idx < args.len() {
                        // *@ (flattening slurpy): flatten array/list args
                        match &args[positional_idx] {
                            Value::Array(arr) => {
                                items.extend(arr.iter().cloned());
                            }
                            other => {
                                items.push(other.clone());
                            }
                        }
                        positional_idx += 1;
                    }
                    if !pd.name.is_empty() {
                        let key = if pd.name.starts_with('@') {
                            pd.name.clone()
                        } else {
                            format!("@{}", pd.name)
                        };
                        self.env.insert(key, Value::array(items));
                    }
                }
            } else if pd.named {
                // Look for a matching named argument (Pair) in args
                let mut found = false;
                for arg in args {
                    if let Value::Pair(key, val) = arg
                        && key == &pd.name
                    {
                        self.env.insert(pd.name.clone(), *val.clone());
                        found = true;
                        break;
                    }
                }
                if !found && let Some(default_expr) = &pd.default {
                    let value = self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?;
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                }
            } else {
                // Positional param
                if positional_idx < args.len() {
                    let mut value = args[positional_idx].clone();
                    if pd.name != "__type_only__"
                        && let Some(constraint) = &pd.type_constraint
                    {
                        if let Some((target, source)) = parse_coercion_type(constraint) {
                            // Coercion type: check source type if specified, then coerce
                            if let Some(src) = source
                                && !self.type_matches_value(src, &value)
                            {
                                return Err(RuntimeError::new(format!(
                                    "X::TypeCheck: Type check failed for {}: expected {}, got {}",
                                    pd.name,
                                    constraint,
                                    super::value_type_name(&value)
                                )));
                            }
                            value = coerce_value(target, value);
                        } else if !self.type_matches_value(constraint, &value) {
                            return Err(RuntimeError::new(format!(
                                "X::TypeCheck: Type check failed for {}: expected {}, got {}",
                                pd.name,
                                constraint,
                                super::value_type_name(&value)
                            )));
                        }
                    }
                    if !pd.name.is_empty() && pd.name != "__type_only__" {
                        self.env.insert(pd.name.clone(), value);
                    }
                    positional_idx += 1;
                } else if let Some(default_expr) = &pd.default {
                    let value = self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?;
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                }
            }
        }
        Ok(())
    }
}
