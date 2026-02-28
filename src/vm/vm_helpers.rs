use super::*;

impl VM {
    fn twigil_dynamic_alias(name: &str) -> Option<String> {
        if let Some(rest) = name.strip_prefix("$*") {
            return Some(format!("*{}", rest));
        }
        if let Some(rest) = name.strip_prefix('*') {
            return Some(format!("$*{}", rest));
        }
        None
    }

    fn main_unqualified_name(name: &str) -> Option<String> {
        for sigil in ["$", "@", "%", "&"] {
            let prefix = format!("{sigil}Main::");
            if let Some(rest) = name.strip_prefix(&prefix) {
                return Some(format!("{sigil}{rest}"));
            }
        }
        None
    }

    fn main_qualified_name(name: &str) -> Option<String> {
        for sigil in ["$", "@", "%", "&"] {
            if let Some(rest) = name.strip_prefix(sigil)
                && !rest.contains("::")
            {
                return Some(format!("{sigil}Main::{rest}"));
            }
        }
        None
    }

    /// Look up a sigiled variable name (e.g. "@z") in the locals array by its
    /// bare (sigil-stripped) name.  This handles function parameters that are
    /// stored without sigils in the compiled-function locals.
    pub(super) fn get_local_by_bare_name(&self, code: &CompiledCode, name: &str) -> Option<Value> {
        // Strip the leading sigil (@, %)
        let bare = name.strip_prefix('@').or_else(|| name.strip_prefix('%'))?;
        // Respect lexical shadowing by resolving the most recently-declared local.
        let idx = code.locals.iter().rposition(|n| n == bare)?;
        Some(self.locals.get(idx)?.clone())
    }

    pub(super) fn get_env_with_main_alias(&self, name: &str) -> Option<Value> {
        // Follow binding aliases ($CALLER::target := $source)
        if let Some(resolved) = self.interpreter.resolve_binding(name)
            && let Some(val) = self.interpreter.env().get(resolved)
        {
            return Some(val.clone());
        }
        if let Some(val) = self.interpreter.env().get(name) {
            return Some(val.clone());
        }
        if let Some(alias) = Self::twigil_dynamic_alias(name) {
            return self.interpreter.env().get(&alias).cloned();
        }
        if let Some(alias) = Self::main_unqualified_name(name) {
            return self.interpreter.env().get(&alias).cloned();
        }
        if let Some(qualified) = Self::main_qualified_name(name) {
            return self.interpreter.env().get(&qualified).cloned();
        }
        None
    }

    pub(super) fn set_env_with_main_alias(&mut self, name: &str, value: Value) {
        self.interpreter
            .env_mut()
            .insert(name.to_string(), value.clone());
        if let Some(alias) = Self::twigil_dynamic_alias(name) {
            self.interpreter.env_mut().insert(alias, value.clone());
        }
        if let Some(inner) = name
            .strip_prefix("&infix:<")
            .or_else(|| name.strip_prefix("&prefix:<"))
            .or_else(|| name.strip_prefix("&postfix:<"))
            && let Some(op_name) = inner.strip_suffix('>')
        {
            self.interpreter
                .env_mut()
                .insert(format!("&{}", op_name), value.clone());
        }
        if let Some(alias) = Self::main_unqualified_name(name) {
            self.interpreter.env_mut().insert(alias, value);
            return;
        }
        if let Some(qualified) = Self::main_qualified_name(name)
            && self.interpreter.env().contains_key(&qualified)
        {
            self.interpreter.env_mut().insert(qualified, value);
        }
    }

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
        Value::make_instance("Thread".to_string(), attrs)
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
        if s.is_empty() {
            return String::new();
        }
        let mut chars: Vec<char> = s.chars().collect();
        let mut carry = true;
        for ch in chars.iter_mut().rev() {
            if !carry {
                break;
            }
            if ch.is_ascii_lowercase() {
                if *ch == 'z' {
                    *ch = 'a';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_uppercase() {
                if *ch == 'Z' {
                    *ch = 'A';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_digit() {
                if *ch == '9' {
                    *ch = '0';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else {
                *ch = char::from_u32(*ch as u32 + 1).unwrap_or(*ch);
                carry = false;
            }
        }
        if carry {
            let first = chars[0];
            let prefix = if first.is_ascii_lowercase() {
                'a'
            } else if first.is_ascii_uppercase() {
                'A'
            } else if first.is_ascii_digit() {
                '1'
            } else {
                first
            };
            chars.insert(0, prefix);
        }
        chars.into_iter().collect()
    }

    fn string_pred(s: &str) -> String {
        if s.is_empty() {
            return String::new();
        }
        let mut chars: Vec<char> = s.chars().collect();
        if chars.len() == 1 {
            let ch = chars[0];
            if let Some(prev) = char::from_u32(ch as u32 - 1) {
                return prev.to_string();
            }
            return s.to_string();
        }
        let mut borrow = true;
        for ch in chars.iter_mut().rev() {
            if !borrow {
                break;
            }
            if ch.is_ascii_lowercase() {
                if *ch == 'a' {
                    *ch = 'z';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else if ch.is_ascii_uppercase() {
                if *ch == 'A' {
                    *ch = 'Z';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else if ch.is_ascii_digit() {
                if *ch == '0' {
                    *ch = '9';
                } else {
                    *ch = (*ch as u8 - 1) as char;
                    borrow = false;
                }
            } else {
                if let Some(prev) = char::from_u32(*ch as u32 - 1) {
                    *ch = prev;
                }
                borrow = false;
            }
        }
        if borrow && chars.len() > 1 {
            chars.remove(0);
        }
        chars.into_iter().collect()
    }

    pub(super) fn increment_value(value: &Value) -> Value {
        match value {
            Value::Int(i) => i
                .checked_add(1)
                .map(Value::Int)
                .unwrap_or_else(|| Value::BigInt(num_bigint::BigInt::from(*i) + 1)),
            Value::BigInt(n) => Value::from_bigint(n + 1),
            Value::Bool(_) => Value::Bool(true),
            Value::Rat(n, d) => make_rat(n + d, *d),
            Value::Str(s) => {
                if let Some(next) = Self::superscript_succ(s) {
                    Value::Str(next)
                } else {
                    Value::Str(Self::string_succ(s))
                }
            }
            _ => Value::Int(1),
        }
    }

    pub(super) fn normalize_incdec_source(value: Value) -> Value {
        match value {
            Value::Nil | Value::Package(_) => Value::Int(0),
            other => other,
        }
    }

    pub(super) fn decrement_value(value: &Value) -> Value {
        match value {
            Value::Int(i) => i
                .checked_sub(1)
                .map(Value::Int)
                .unwrap_or_else(|| Value::BigInt(num_bigint::BigInt::from(*i) - 1)),
            Value::BigInt(n) => Value::from_bigint(n - 1),
            Value::Bool(_) => Value::Bool(false),
            Value::Rat(n, d) => make_rat(n - d, *d),
            Value::Str(s) => {
                if let Some(prev) = Self::superscript_pred(s) {
                    Value::Str(prev)
                } else {
                    Value::Str(Self::string_pred(s))
                }
            }
            _ => Value::Int(-1),
        }
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
        attrs.insert("variable".to_string(), Value::Str(var_name.clone()));
        attrs.insert("suggestions".to_string(), Value::Str(suggestion.clone()));
        attrs.insert(
            "message".to_string(),
            Value::Str(format!(
                "Variable '{}' is not declared. Did you mean '{}'?",
                var_name, suggestion
            )),
        );
        let ex = Value::make_instance("X::Undeclared".to_string(), attrs);
        let mut err = RuntimeError::new(format!(
            "X::Undeclared: Variable '{}' is not declared. Did you mean '{}'?",
            var_name, suggestion
        ));
        err.exception = Some(Box::new(ex));
        err
    }

    pub(super) fn is_builtin_type(name: &str) -> bool {
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
                | "X::AdHoc"
                | "CompUnit::DependencySpecification"
                | "Proxy"
                | "CallFrame"
                | "Backtrace"
                | "array"
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
        )
    }

    /// Check if a name is a type with a smiley suffix (:U, :D, :_).
    pub(super) fn is_type_with_smiley(name: &str, interp: &crate::runtime::Interpreter) -> bool {
        let (base, smiley) = crate::runtime::types::strip_type_smiley(name);
        if smiley.is_none() {
            return false;
        }
        interp.has_class(base) || Self::is_builtin_type(base)
    }

    pub(super) fn label_matches(error_label: &Option<String>, loop_label: &Option<String>) -> bool {
        error_label.as_deref() == loop_label.as_deref() || error_label.is_none()
    }

    /// Force a LazyList into a Seq by evaluating the gather body.
    fn force_lazy_if_needed(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyList(ll) = &val {
            let items = self.interpreter.force_lazy_list_bridge(ll)?;
            Ok(Value::Seq(std::sync::Arc::new(items)))
        } else {
            Ok(val)
        }
    }

    pub(super) fn eval_binary_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        f: fn(&mut VM, Value, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        // Force LazyList values before arithmetic/comparison operations
        let left = self.force_lazy_if_needed(left)?;
        let right = self.force_lazy_if_needed(right)?;
        f(self, left, right)
    }

    /// Smartmatch with junction threading but WITHOUT forcing lazy values.
    pub(super) fn eval_smartmatch_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
    ) -> Result<Value, RuntimeError> {
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_smartmatch_with_junctions(v, right.clone(), negate))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_smartmatch_with_junctions(left.clone(), v, negate))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if negate {
            self.not_smart_match_op(left, right)
        } else {
            self.smart_match_op(left, right)
        }
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let is_regex = matches!(
            &right,
            Value::Regex(_)
                | Value::RegexWithAdverbs { .. }
                | Value::Routine { is_regex: true, .. }
        );
        let matched = self.interpreter.smart_match_values(&left, &right);
        // Check for pending regex security error (set by regex parse/match)
        if let Some(err) = crate::runtime::Interpreter::take_pending_regex_error() {
            return Err(err);
        }
        if is_regex {
            // For regex smartmatch, return the Match object (from $/) or Nil
            if matched {
                Ok(self
                    .interpreter
                    .env()
                    .get("/")
                    .cloned()
                    .unwrap_or(Value::Nil))
            } else {
                Ok(Value::Nil)
            }
        } else {
            Ok(Value::Bool(matched))
        }
    }

    pub(super) fn not_smart_match_op(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::Bool(
            !self.interpreter.smart_match_values(&left, &right),
        ))
    }

    pub(super) fn eval_reduction_operator_values(
        &mut self,
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(inner_op) = op.strip_prefix('R')
            && !inner_op.is_empty()
        {
            return self.eval_reduction_operator_values(inner_op, right, left);
        }
        let normalized_op = if op == "∘" { "o" } else { op };
        match Interpreter::apply_reduction_op(normalized_op, left, right) {
            Ok(v) => Ok(v),
            Err(err) if err.message.starts_with("Unsupported reduction operator:") => {
                let args = vec![left.clone(), right.clone()];
                if let Some(name) = normalized_op.strip_prefix('&') {
                    let callable = self.interpreter.resolve_code_var(name);
                    if matches!(
                        callable,
                        Value::Sub(_)
                            | Value::WeakSub(_)
                            | Value::Routine { .. }
                            | Value::Instance { .. }
                    ) {
                        return self.interpreter.eval_call_on_value(callable, args);
                    }
                } else {
                    let infix_name = format!("infix:<{}>", normalized_op);
                    if let Some(v) = self.try_user_infix(&infix_name, left, right)? {
                        return Ok(v);
                    }
                    if let Some(callable) = self
                        .interpreter
                        .env()
                        .get(&format!("&{}", infix_name))
                        .cloned()
                    {
                        return self.interpreter.eval_call_on_value(callable, args.clone());
                    }
                    if let Some(callable) = self
                        .interpreter
                        .env()
                        .get(&format!("&{}", normalized_op))
                        .cloned()
                    {
                        return self.interpreter.eval_call_on_value(callable, args.clone());
                    }
                }
                Err(err)
            }
            Err(err) => Err(err),
        }
    }

    pub(super) fn coerce_numeric_bridge_value(
        &mut self,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if !matches!(value, Value::Instance { .. }) {
            return Ok(value);
        }
        if !(self.interpreter.type_matches_value("Real", &value)
            || self.interpreter.type_matches_value("Numeric", &value))
        {
            return Ok(value);
        }
        self.interpreter
            .call_method_with_values(value.clone(), "Numeric", vec![])
            .or_else(|_| {
                self.interpreter
                    .call_method_with_values(value.clone(), "Bridge", vec![])
            })
            .or(Ok(value))
    }

    pub(super) fn coerce_numeric_bridge_pair(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<(Value, Value), RuntimeError> {
        Ok((
            self.coerce_numeric_bridge_value(left)?,
            self.coerce_numeric_bridge_value(right)?,
        ))
    }

    pub(super) fn sync_locals_from_env(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
    }

    pub(super) fn sync_env_from_locals(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            self.set_env_with_main_alias(name, self.locals[i].clone());
        }
    }

    pub(super) fn find_local_slot(&self, code: &CompiledCode, name: &str) -> Option<usize> {
        code.locals.iter().position(|n| n == name)
    }

    pub(super) fn update_local_if_exists(&mut self, code: &CompiledCode, name: &str, val: &Value) {
        if let Some(slot) = self.find_local_slot(code, name) {
            self.locals[slot] = val.clone();
        }
    }

    pub(super) fn try_native_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        fn collection_contains_instance(value: &Value) -> bool {
            match value {
                Value::Instance { .. } => true,
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                    items.iter().any(collection_contains_instance)
                }
                Value::Hash(map) => map.values().any(collection_contains_instance),
                _ => false,
            }
        }
        let bypass_supply_extrema_fastpath = matches!(method, "max" | "min" | "lines")
            && args.len() <= 1
            && (matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supply"
            ) || matches!(target, Value::Package(name) if name == "Supply"));
        let bypass_supplier_supply_fastpath = method == "Supply"
            && args.is_empty()
            && matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supplier"
            );
        let bypass_gist_fastpath =
            method == "gist" && args.is_empty() && collection_contains_instance(target);
        let bypass_pickroll_type_fastpath = matches!(method, "pick" | "roll")
            && args.len() <= 1
            && matches!(target, Value::Package(_) | Value::Str(_));
        let bypass_squish_fastpath = method == "squish";
        let bypass_numeric_bridge_instance_fastpath = matches!(target, Value::Instance { .. })
            && (self.interpreter.type_matches_value("Real", target)
                || self.interpreter.type_matches_value("Numeric", target)
                || matches!(target, Value::Instance { class_name, .. }
                    if self.interpreter.has_user_method(class_name, "Bridge")));
        if bypass_supply_extrema_fastpath
            || bypass_supplier_supply_fastpath
            || bypass_gist_fastpath
            || bypass_pickroll_type_fastpath
            || bypass_squish_fastpath
            || bypass_numeric_bridge_instance_fastpath
        {
            return None;
        }
        if args.len() == 2 {
            return crate::builtins::native_method_2arg(target, method, &args[0], &args[1]);
        }
        if args.len() == 1 {
            return crate::builtins::native_method_1arg(target, method, &args[0]);
        }
        if !args.is_empty() {
            return None;
        }
        crate::builtins::native_method_0arg(target, method)
    }

    pub(super) fn try_native_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.iter().any(|arg| matches!(arg, Value::Instance { .. })) {
            return None;
        }
        crate::builtins::native_function(name, args)
    }

    pub(super) fn find_compiled_function<'a>(
        &mut self,
        compiled_fns: &'a HashMap<String, CompiledFunction>,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        let expected_fingerprint = self
            .interpreter
            .resolve_function_with_types(name, args)
            .map(|def| {
                crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
            });
        // If runtime resolution fails, avoid reusing stale compiled cache entries.
        // This can happen across repeated EVAL calls that redefine the same routine name.
        let expected_fingerprint = expected_fingerprint?;
        let matches_resolved = |cf: &CompiledFunction| cf.fingerprint == expected_fingerprint;
        let pkg = self.interpreter.current_package();
        let arity = args.len();
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| runtime::value_type_name(v).to_string())
            .collect();
        if name.contains("::") {
            let key_typed = format!("{name}/{arity}:{}", type_sig.join(","));
            if let Some(cf) = compiled_fns.get(&key_typed)
                && matches_resolved(cf)
            {
                return Some(cf);
            }
            let key_arity = format!("{name}/{arity}");
            if let Some(cf) = compiled_fns.get(&key_arity)
                && matches_resolved(cf)
            {
                return Some(cf);
            }
            if let Some(cf) = compiled_fns.get(name)
                && matches_resolved(cf)
            {
                return Some(cf);
            }
        }
        let key_typed = format!("{}::{}/{}:{}", pkg, name, arity, type_sig.join(","));
        if let Some(cf) = compiled_fns.get(&key_typed)
            && matches_resolved(cf)
        {
            return Some(cf);
        }
        let key_arity = format!("{}::{}/{}", pkg, name, arity);
        if let Some(cf) = compiled_fns.get(&key_arity)
            && matches_resolved(cf)
        {
            return Some(cf);
        }
        let key_simple = format!("{}::{}", pkg, name);
        if let Some(cf) = compiled_fns.get(&key_simple)
            && matches_resolved(cf)
        {
            return Some(cf);
        }
        if pkg != "GLOBAL" {
            let key_global = format!("GLOBAL::{}", name);
            if let Some(cf) = compiled_fns.get(&key_global)
                && matches_resolved(cf)
            {
                return Some(cf);
            }
        }
        None
    }

    pub(super) fn call_compiled_function_named(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        if callsite_line.is_some() {
            self.interpreter.set_pending_callsite_line(callsite_line);
        }
        let saved_env = self.interpreter.env().clone();
        let saved_readonly = self.interpreter.save_readonly_vars();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack_depth = self.stack.len();
        let return_spec = cf.return_type.clone();

        self.interpreter.inject_pending_callsite_line();
        self.interpreter.push_caller_env();

        // Push Sub value to block_stack for callframe().code
        let sub_val = Value::make_sub(
            fn_package.to_string(),
            fn_name.to_string(),
            cf.params.clone(),
            cf.param_defs.clone(),
            vec![],
            false,
            self.interpreter.env().clone(),
        );
        self.interpreter.push_block(sub_val);

        let mut callable_id: Option<u64> = None;
        if !fn_name.is_empty() {
            self.interpreter
                .push_routine(fn_package.to_string(), fn_name.to_string());
            let callable_key = format!("__mutsu_callable_id::{fn_package}::{fn_name}");
            let resolved_callable_id = self
                .interpreter
                .env()
                .get(&callable_key)
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i),
                    _ => None,
                })
                .unwrap_or(0);
            callable_id = (resolved_callable_id != 0).then_some(resolved_callable_id as u64);
            self.interpreter.env_mut().insert(
                "__mutsu_callable_id".to_string(),
                Value::Int(resolved_callable_id),
            );
        }
        let is_test_assertion = if fn_name.is_empty() {
            false
        } else {
            self.interpreter
                .routine_is_test_assertion_by_name(fn_name, &args)
        };
        let pushed_assertion = self
            .interpreter
            .push_test_assertion_context(is_test_assertion);

        if cf.empty_sig && !args.is_empty() {
            if !fn_name.is_empty() {
                self.interpreter.pop_routine();
            }
            self.interpreter
                .pop_test_assertion_context(pushed_assertion);
            self.interpreter.pop_caller_env();
            self.stack.truncate(saved_stack_depth);
            self.locals = saved_locals;
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        let rw_bindings =
            match self
                .interpreter
                .bind_function_args_values(&cf.param_defs, &cf.params, &args)
            {
                Ok(bindings) => bindings,
                Err(e) => {
                    if !fn_name.is_empty() {
                        self.interpreter.pop_routine();
                    }
                    self.interpreter
                        .pop_test_assertion_context(pushed_assertion);
                    self.interpreter.pop_caller_env();
                    self.stack.truncate(saved_stack_depth);
                    self.locals = saved_locals;
                    *self.interpreter.env_mut() = saved_env;
                    self.interpreter.restore_readonly_vars(saved_readonly);
                    return Err(e);
                }
            };
        self.interpreter
            .prepare_definite_return_slot(return_spec.as_deref());

        self.locals = vec![Value::Nil; cf.code.locals.len()];
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cf.code.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{fn_package}::{fn_name}");
                    let matches_frame = if let Some(target_id) = e.leave_callable_id {
                        Some(target_id) == callable_id
                    } else if let Some(target_routine) = e.leave_routine.as_ref() {
                        target_routine == &routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.is_last = false;
                        let ret_val = e.return_value.unwrap_or(Value::Nil);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val);
                        self.interpreter.discard_let_saves(let_mark);
                        result = Ok(());
                        break;
                    }
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    // fail() — restore let saves and return a Failure value
                    fail_bypass = true;
                    let failure = self.interpreter.fail_error_to_failure_value(&e);
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back to persistent storage.
        // Read from env first (methods like push update env directly),
        // falling back to locals.
        for (slot, key) in &cf.code.state_locals {
            let local_name = &cf.code.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }

        if !fn_name.is_empty() {
            self.interpreter.pop_routine();
        }
        self.interpreter
            .pop_test_assertion_context(pushed_assertion);
        self.interpreter.pop_block();

        let mut restored_env = saved_env;
        self.interpreter
            .pop_caller_env_with_writeback(&mut restored_env);
        self.interpreter
            .apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        let rw_sources: std::collections::HashSet<String> = rw_bindings
            .iter()
            .map(|(_, source)| source.clone())
            .collect();
        let local_names: std::collections::HashSet<&String> = cf.code.locals.iter().collect();
        for (k, v) in self.interpreter.env().iter() {
            if restored_env.contains_key(k) && !local_names.contains(k) && !rw_sources.contains(k) {
                restored_env.insert(k.clone(), v.clone());
            }
        }
        self.locals = saved_locals;
        *self.interpreter.env_mut() = restored_env;
        self.interpreter.restore_readonly_vars(saved_readonly);

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                let base_result = if let Some(v) = explicit_return {
                    let mut e = RuntimeError::new("return");
                    e.return_value = Some(v);
                    Err(e)
                } else {
                    Ok(ret_val)
                };
                self.interpreter
                    .finalize_return_with_spec(base_result, return_spec.as_deref())
            }
            Err(e) => Err(e),
        }
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
            Value::BigInt(n) => n.clone(),
            _ => return value,
        };

        let wrapped = native_types::wrap_native_int(&constraint, &big_val);
        wrapped
            .to_i64()
            .map(Value::Int)
            .unwrap_or_else(|| Value::BigInt(wrapped))
    }
}
