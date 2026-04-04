use super::*;

impl Interpreter {
    pub(in crate::runtime) fn init_endian_enum(&mut self) {
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

    pub(in crate::runtime) fn init_protocol_family_enum(&mut self) {
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

    pub(in crate::runtime) fn init_order_enum(&mut self) {
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

    pub(in crate::runtime) fn init_signal_enum(&mut self) {
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

    pub(in crate::runtime) fn version_from_value(arg: Value) -> Value {
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
                crate::runtime::version_cmp_parts(left_parts, right_parts)
                    != std::cmp::Ordering::Less
            } else if right_minus {
                // LHS <= RHS (base version without -)
                crate::runtime::version_cmp_parts(left_parts, right_parts)
                    != std::cmp::Ordering::Greater
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

    pub(crate) fn set_attributes_pragma(&mut self, smiley: &str) {
        // smiley is ":D", ":U", ":_", or empty
        self.attributes_pragma = smiley.to_string();
    }

    /// Apply the `use variables` pragma to a type constraint.
    /// If the constraint has no explicit smiley and the pragma is active,
    /// append the pragma smiley (e.g., `Int` -> `Int:D` when `use variables :D`).
    pub(crate) fn apply_variables_pragma<'a>(
        &self,
        constraint: &'a str,
    ) -> std::borrow::Cow<'a, str> {
        if self.variables_pragma.is_empty() || self.variables_pragma == ":_" {
            return std::borrow::Cow::Borrowed(constraint);
        }
        let (_base, smiley) = strip_type_smiley(constraint);
        if smiley.is_some() {
            // Already has a smiley -- don't override
            return std::borrow::Cow::Borrowed(constraint);
        }
        std::borrow::Cow::Owned(format!("{}{}", constraint, self.variables_pragma))
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
        // Strip parameterization: Array[Int] -> Array
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
        if crate::runtime::utils::is_known_type_constraint(base) {
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

    /// Resolve the ultimate base type of a constraint, following subset chains.
    /// Returns the constraint itself if it is not a subset type.
    pub(crate) fn resolve_subset_base_type<'a>(&'a self, constraint: &'a str) -> &'a str {
        let mut current = constraint;
        // Follow subset chain up to a reasonable depth to avoid cycles
        for _ in 0..20 {
            if let Some(subset) = self.subsets.get(current) {
                current = &subset.base;
            } else {
                break;
            }
        }
        current
    }
}
