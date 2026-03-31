use super::*;
use crate::symbol::Symbol;

impl VM {
    /// Create a Thread instance with the current OS thread's ID.
    pub(super) fn make_thread_instance() -> Value {
        let thread_id = std::thread::current().id();
        // Extract a numeric ID from ThreadId's Debug representation
        let id_str = format!("{:?}", thread_id);
        let numeric_id: i64 = id_str
            .chars()
            .filter(|c| c.is_ascii_digit())
            .collect::<String>()
            .parse()
            .unwrap_or(0);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("id".to_string(), Value::Int(numeric_id));
        Value::make_instance(Symbol::intern("Thread"), attrs)
    }

    pub(super) fn const_str(code: &CompiledCode, idx: u32) -> &str {
        match &code.constants[idx as usize] {
            Value::Str(s) => s.as_str(),
            _ => unreachable!("expected string constant"),
        }
    }

    fn superscript_digit_value(c: char) -> Option<u32> {
        match c {
            '\u{2070}' => Some(0), // ⁰
            '\u{00B9}' => Some(1), // ¹
            '\u{00B2}' => Some(2), // ²
            '\u{00B3}' => Some(3), // ³
            '\u{2074}' => Some(4), // ⁴
            '\u{2075}' => Some(5), // ⁵
            '\u{2076}' => Some(6), // ⁶
            '\u{2077}' => Some(7), // ⁷
            '\u{2078}' => Some(8), // ⁸
            '\u{2079}' => Some(9), // ⁹
            _ => None,
        }
    }

    fn superscript_digit_char(d: u32) -> char {
        match d {
            0 => '\u{2070}', // ⁰
            1 => '\u{00B9}', // ¹
            2 => '\u{00B2}', // ²
            3 => '\u{00B3}', // ³
            4 => '\u{2074}', // ⁴
            5 => '\u{2075}', // ⁵
            6 => '\u{2076}', // ⁶
            7 => '\u{2077}', // ⁷
            8 => '\u{2078}', // ⁸
            9 => '\u{2079}', // ⁹
            _ => unreachable!("superscript digit out of range"),
        }
    }

    fn superscript_succ(s: &str) -> Option<String> {
        let mut digits = Vec::new();
        for ch in s.chars() {
            digits.push(Self::superscript_digit_value(ch)?);
        }
        if digits.is_empty() {
            return None;
        }
        let mut carry = 1u32;
        for d in digits.iter_mut().rev() {
            if carry == 0 {
                break;
            }
            let sum = *d + carry;
            *d = sum % 10;
            carry = sum / 10;
        }
        if carry > 0 {
            digits.insert(0, carry);
        }
        Some(
            digits
                .into_iter()
                .map(Self::superscript_digit_char)
                .collect(),
        )
    }

    fn superscript_pred(s: &str) -> Option<String> {
        let mut digits = Vec::new();
        for ch in s.chars() {
            digits.push(Self::superscript_digit_value(ch)?);
        }
        if digits.is_empty() {
            return None;
        }
        let mut borrow = 1u32;
        for d in digits.iter_mut().rev() {
            if borrow == 0 {
                break;
            }
            if *d >= borrow {
                *d -= borrow;
                borrow = 0;
            } else {
                *d = 10 + *d - borrow;
                borrow = 1;
            }
        }
        if borrow > 0 {
            return None;
        }
        Some(
            digits
                .into_iter()
                .map(Self::superscript_digit_char)
                .collect(),
        )
    }

    fn string_succ(s: &str) -> String {
        crate::builtins::str_increment::string_succ(s)
    }

    fn string_pred_checked(s: &str) -> Option<String> {
        crate::builtins::str_increment::string_pred_checked(s)
    }

    pub(super) fn increment_value(value: &Value) -> Value {
        match value {
            Value::Int(i) => i
                .checked_add(1)
                .map(Value::Int)
                .unwrap_or_else(|| Value::bigint(num_bigint::BigInt::from(*i) + 1)),
            Value::BigInt(n) => Value::from_bigint(n.as_ref() + 1),
            Value::Bool(_) => Value::Bool(true),
            Value::Rat(n, d) => make_rat(n + d, *d),
            Value::FatRat(n, d) => match make_rat(n + d, *d) {
                Value::Rat(nn, dd) => Value::FatRat(nn, dd),
                other => other,
            },
            Value::Num(f) => Value::Num(f + 1.0),
            Value::Complex(r, i) => Value::Complex(r + 1.0, *i),
            Value::Str(s) => {
                if let Some(next) = Self::superscript_succ(s) {
                    Value::str(next)
                } else {
                    Value::str(Self::string_succ(s))
                }
            }
            // Mixin (allomorphic types like IntStr): increment the inner value
            Value::Mixin(inner, _) => Self::increment_value(inner),
            _ => Value::Int(1),
        }
    }

    pub(super) fn normalize_incdec_source(value: Value) -> Value {
        match value {
            Value::Nil => Value::Int(0),
            Value::Package(name) => match name.resolve().as_str() {
                "Num" | "num" => Value::Num(0.0),
                "Rat" => crate::value::make_rat(0, 1),
                "Complex" => Value::Complex(0.0, 0.0),
                "Bool" => Value::Bool(false),
                _ => Value::Int(0),
            },
            other => other,
        }
    }

    /// Like normalize_incdec_source, but also checks the variable's type
    /// constraint when the value is Nil. This ensures that e.g. `my Num $v; ++$v`
    /// starts from Num(0.0) rather than Int(0).
    pub(super) fn normalize_incdec_source_with_type(&self, var_name: &str, value: Value) -> Value {
        match &value {
            Value::Nil => {
                if let Some(tc) = self.interpreter.var_type_constraint(var_name) {
                    match tc.as_str() {
                        "Num" | "num" => Value::Num(0.0),
                        "Rat" => crate::value::make_rat(0, 1),
                        "Complex" => Value::Complex(0.0, 0.0),
                        "Bool" => Value::Bool(false),
                        _ => Value::Int(0),
                    }
                } else {
                    Value::Int(0)
                }
            }
            _ => Self::normalize_incdec_source(value),
        }
    }

    pub(super) fn decrement_value(value: &Value) -> Value {
        match value {
            Value::Int(i) => i
                .checked_sub(1)
                .map(Value::Int)
                .unwrap_or_else(|| Value::bigint(num_bigint::BigInt::from(*i) - 1)),
            Value::BigInt(n) => Value::from_bigint(n.as_ref() - 1),
            Value::Bool(_) => Value::Bool(false),
            Value::Rat(n, d) => make_rat(n - d, *d),
            Value::FatRat(n, d) => match make_rat(n - d, *d) {
                Value::Rat(nn, dd) => Value::FatRat(nn, dd),
                other => other,
            },
            Value::Num(f) => Value::Num(f - 1.0),
            Value::Complex(r, i) => Value::Complex(r - 1.0, *i),
            Value::Str(s) => {
                if let Some(prev) = Self::superscript_pred(s) {
                    Value::str(prev)
                } else if let Some(pred) = Self::string_pred_checked(s) {
                    Value::str(pred)
                } else {
                    // Decrement underflow: return a Failure value
                    Self::make_decrement_failure()
                }
            }
            // Mixin (allomorphic types like IntStr): decrement the inner value
            Value::Mixin(inner, _) => Self::decrement_value(inner),
            _ => Value::Int(-1),
        }
    }

    /// Create a Failure value for "Decrement out of range".
    fn make_decrement_failure() -> Value {
        let mut ex_attrs = std::collections::HashMap::new();
        ex_attrs.insert(
            "message".to_string(),
            Value::str("Decrement out of range".to_string()),
        );
        let exception = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);
        let mut failure_attrs = std::collections::HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(super) fn strict_undeclared_error(&self, name: &str) -> RuntimeError {
        let suggestion = if name.is_empty() {
            String::new()
        } else {
            let mut chars = name.chars();
            let first = chars.next().unwrap().to_uppercase().collect::<String>();
            format!("{}{}", first, chars.as_str())
        };
        let var_name = if name.starts_with('$')
            || name.starts_with('@')
            || name.starts_with('%')
            || name.starts_with('&')
        {
            name.to_string()
        } else {
            format!("${name}")
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("variable".to_string(), Value::str(var_name.clone()));
        attrs.insert("suggestions".to_string(), Value::str(suggestion.clone()));
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Variable '{}' is not declared. Did you mean '{}'?",
                var_name, suggestion
            )),
        );
        let ex = Value::make_instance(Symbol::intern("X::Undeclared"), attrs);
        let mut err = RuntimeError::new(format!(
            "X::Undeclared: Variable '{}' is not declared. Did you mean '{}'?",
            var_name, suggestion
        ));
        err.exception = Some(Box::new(ex));
        err
    }

    pub(crate) fn is_builtin_type(name: &str) -> bool {
        matches!(
            name,
            "Hash"
                | "Array"
                | "Int"
                | "Num"
                | "Rat"
                | "FatRat"
                | "Complex"
                | "Str"
                | "Bool"
                | "Pair"
                | "Map"
                | "QuantHash"
                | "Set"
                | "Bag"
                | "Mix"
                | "List"
                | "Seq"
                | "Range"
                | "Any"
                | "Mu"
                | "Cool"
                | "Real"
                | "Numeric"
                | "Stringy"
                | "Positional"
                | "Associative"
                | "Failure"
                | "Exception"
                | "Order"
                | "Uni"
                | "NFC"
                | "NFD"
                | "NFKC"
                | "NFKD"
                | "Version"
                | "Nil"
                | "Regex"
                | "Block"
                | "Routine"
                | "Sub"
                | "Callable"
                | "Method"
                | "IO"
                | "Proc"
                | "Slip"
                | "Duration"
                | "Date"
                | "DateTime"
                | "Dateish"
                | "ObjAt"
                | "ValueObjAt"
                | "Code"
                | "Capture"
                | "Junction"
                | "Match"
                | "Signature"
                | "Parameter"
                | "WhateverCode"
                | "HyperWhatever"
                | "Stash"
                | "Scalar"
                | "SetHash"
                | "BagHash"
                | "MixHash"
                | "Setty"
                | "Baggy"
                | "Mixy"
                | "Grammar"
                | "Submethod"
                | "Label"
                | "Lock"
                | "Semaphore"
                | "Whatever"
                | "Instant"
                | "Buf"
                | "Blob"
                | "blob8"
                | "buf8"
                | "blob16"
                | "buf16"
                | "blob32"
                | "buf32"
                | "blob64"
                | "buf64"
                | "Endian"
                | "Kernel"
                | "CX::Warn"
                | "CX::Return"
                | "X::AdHoc"
                | "CompUnit::DependencySpecification"
                | "CompUnit::Repository"
                | "CompUnit::Repository::FileSystem"
                | "CompUnit::RepositoryRegistry"
                | "Proxy"
                | "CallFrame"
                | "Backtrace"
                | "array"
                | "UInt"
                | "NativeInt"
                | "int8"
                | "int16"
                | "int32"
                | "int64"
                | "uint8"
                | "uint16"
                | "uint32"
                | "uint64"
                | "byte"
                | "int"
                | "uint"
                | "num"
                | "num32"
                | "num64"
                | "str"
                | "IntStr"
                | "NumStr"
                | "RatStr"
                | "ComplexStr"
                | "Allomorph"
                | "Attribute"
                | "Collation"
                | "Cursor"
                | "Deprecation"
                | "X"
        ) || {
            // Handle parameterized types like Buf[uint8], Array[Int], etc.
            if let Some(open) = name.find('[')
                && name.ends_with(']')
                && open > 0
            {
                Self::is_builtin_type(&name[..open])
            } else {
                false
            }
        }
    }

    /// Resolve type aliases (e.g., Cursor -> Match).
    /// Returns the canonical name if the input is an alias, or the input unchanged.
    pub(super) fn resolve_type_alias(name: &str) -> &str {
        match name {
            "Cursor" => "Match",
            _ => name,
        }
    }

    /// Check if a name is a type with a smiley suffix (:U, :D, :_).
    pub(super) fn is_type_with_smiley(name: &str, interp: &crate::runtime::Interpreter) -> bool {
        let (base, smiley) = crate::runtime::types::strip_type_smiley(name);
        if smiley.is_none() {
            return false;
        }
        interp.has_class(base) || Self::is_builtin_type(base)
    }

    /// Get the current $*COLLATION settings, falling back to defaults.
    pub(super) fn get_collation_settings(&self) -> crate::builtins::collation::CollationSettings {
        // Check local env first (variable stored as "*COLLATION")
        if let Some(val) = self.get_env_with_main_alias("*COLLATION") {
            return crate::builtins::collation::CollationSettings::from_value(&val);
        }
        // Try dynamic lookup through caller stack
        if let Ok(val) = self.interpreter.get_dynamic_var("*COLLATION") {
            return crate::builtins::collation::CollationSettings::from_value(&val);
        }
        // Also check with $* prefix
        if let Some(val) = self.interpreter.env().get("$*COLLATION") {
            return crate::builtins::collation::CollationSettings::from_value(val);
        }
        crate::builtins::collation::CollationSettings::default()
    }

    /// If the variable has a native int type constraint, wrap the value.
    /// Used by increment/decrement to implement overflow/underflow wrapping.
    pub(super) fn maybe_wrap_native_int(
        interp: &crate::runtime::Interpreter,
        var_name: &str,
        value: Value,
    ) -> Value {
        use crate::runtime::native_types;
        use num_bigint::BigInt as NumBigInt;
        use num_traits::ToPrimitive;

        let constraint = match interp.var_type_constraint(var_name) {
            Some(c) => c,
            None => return value,
        };
        if !native_types::is_native_int_type(&constraint) {
            return value;
        }

        let big_val = match &value {
            Value::Int(n) => NumBigInt::from(*n),
            Value::BigInt(n) => (**n).clone(),
            _ => return value,
        };

        let wrapped = native_types::wrap_native_int(&constraint, &big_val);
        wrapped
            .to_i64()
            .map(Value::Int)
            .unwrap_or_else(|| Value::bigint(wrapped))
    }

    /// Recursively flatten a value like a `*@` slurpy would: non-itemized
    /// Array/List elements are expanded, itemized containers are preserved.
    /// Ranges and Seqs are also expanded into their elements.
    pub(super) fn flatten_value_for_slurpy(val: &Value, out: &mut Vec<Value>) {
        match val {
            Value::Array(items, kind) if !kind.is_itemized() => {
                for item in items.iter() {
                    Self::flatten_value_for_slurpy(item, out);
                }
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
                // Try to convert to integer range for flattening
                let a = crate::runtime::to_int(start);
                let b = crate::runtime::to_int(end);
                let s = if *excl_start { a + 1 } else { a };
                let e = if *excl_end { b } else { b + 1 };
                for i in s..e {
                    out.push(Value::Int(i));
                }
            }
            Value::Seq(items) | Value::Slip(items) => {
                for item in items.iter() {
                    Self::flatten_value_for_slurpy(item, out);
                }
            }
            other => {
                out.push(other.clone());
            }
        }
    }
}
