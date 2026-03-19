use super::*;
use crate::symbol::Symbol;

const ATTR_ALIAS_META_PREFIX: &str = "__mutsu_attr_alias::";

impl VM {
    fn thread_right_first(
        left: &crate::value::JunctionKind,
        right: &crate::value::JunctionKind,
    ) -> bool {
        use crate::value::JunctionKind::{All, Any, None, One};
        matches!(left, Any | One) && matches!(right, All | None)
    }

    /// Sync locals from env if the dirty flag is set, then clear the flag.
    /// If locals are also dirty, flush them to env first so we don't lose
    /// values that were only written to the locals array (fast-path SetLocal).
    pub(super) fn ensure_locals_synced(&mut self, code: &CompiledCode) {
        if self.env_dirty {
            // Flush locals→env first so env has the latest simple-local values
            // before we pull env→locals (which may overwrite them with stale data).
            self.ensure_env_synced(code);
            self.sync_locals_from_env(code);
            self.env_dirty = false;
        }
    }

    /// Save the current env, locals, stack depth, readonly vars, and env_dirty flag
    /// into a new call frame. Resets env_dirty to false for the new frame.
    pub(super) fn push_call_frame(&mut self) {
        let frame = VmCallFrame {
            saved_env: self.interpreter.clone_env(),
            saved_readonly: self.interpreter.save_readonly_vars(),
            saved_locals: std::mem::take(&mut self.locals),
            saved_stack_depth: self.stack.len(),
            saved_env_dirty: self.env_dirty,
            saved_locals_dirty: self.locals_dirty,
        };
        self.env_dirty = false;
        self.locals_dirty = false;
        self.call_frames.push(frame);
    }

    /// Pop the most recent call frame and restore locals, readonly vars, and env_dirty flag.
    /// Returns the frame so callers can access `saved_env` for site-specific merge logic.
    pub(super) fn pop_call_frame(&mut self) -> VmCallFrame {
        let mut frame = self
            .call_frames
            .pop()
            .expect("pop_call_frame: no frame to pop");
        self.locals = std::mem::take(&mut frame.saved_locals);
        self.interpreter
            .restore_readonly_vars(std::mem::take(&mut frame.saved_readonly));
        self.env_dirty = frame.saved_env_dirty;
        self.locals_dirty = frame.saved_locals_dirty;
        frame
    }

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
        // Check local env first so that function parameters and lexical
        // variables take precedence over shared_vars.  Without this,
        // recursive `start` blocks read stale parameter values from
        // shared_vars instead of the locally-bound ones.
        if let Some(val) = self.interpreter.env().get(name) {
            return Some(val.clone());
        }
        // Anonymous scalar placeholders (from bare `$`) are invocation-local.
        if name.starts_with("__ANON_STATE_") {
            return None;
        }
        // Fall back to shared_vars for cross-thread visibility of variables
        // that were explicitly updated by other threads.
        if let Some(v) = self.interpreter.get_shared_var(name) {
            return Some(v);
        }
        // Follow binding aliases ($CALLER::target := $source)
        if let Some(resolved) = self.interpreter.resolve_binding(name)
            && let Some(val) = self.interpreter.env().get(resolved)
        {
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
        // Placeholder block parameters are stored as "^name". Allow lexical
        // access by the de-careted name inside the same block.
        if !name.starts_with('^') {
            let placeholder = format!("^{name}");
            if let Some(val) = self.interpreter.env().get(&placeholder) {
                return Some(val.clone());
            }
            if let Some(val) = self.interpreter.get_shared_var(&placeholder) {
                return Some(val);
            }
        }
        None
    }

    pub(super) fn set_env_with_main_alias(&mut self, name: &str, value: Value) {
        if name.starts_with("__ANON_STATE_") {
            self.interpreter.env_mut().insert(name.to_string(), value);
            return;
        }
        if !name.starts_with('^') {
            let placeholder = format!("^{name}");
            if self.interpreter.env().contains_key(&placeholder) {
                self.interpreter.set_shared_var(&placeholder, value.clone());
                self.interpreter.env_mut().insert(placeholder, value);
                return;
            }
        }
        self.interpreter.set_shared_var(name, value.clone());
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
                } else {
                    Value::str(Self::string_pred(s))
                }
            }
            // Mixin (allomorphic types like IntStr): decrement the inner value
            Value::Mixin(inner, _) => Self::decrement_value(inner),
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
                | "num32"
                | "num64"
                | "str"
                | "IntStr"
                | "NumStr"
                | "RatStr"
                | "ComplexStr"
                | "Allomorph"
                | "Attribute"
                | "Cursor"
                | "X"
                | "utf16"
                | "utf32"
                | "Macro"
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

    pub(super) fn label_matches(error_label: &Option<String>, loop_label: &Option<String>) -> bool {
        error_label.as_deref() == loop_label.as_deref() || error_label.is_none()
    }

    /// Check if a method on LazyList requires forcing the list first.
    pub(super) fn lazy_list_needs_forcing(method: &str) -> bool {
        matches!(
            method,
            "list"
                | "Array"
                | "Numeric"
                | "Int"
                | "elems"
                | "hyper"
                | "race"
                | "first"
                | "grep"
                | "map"
                | "sort"
                | "reverse"
                | "join"
                | "head"
                | "tail"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "flat"
                | "unique"
                | "squish"
                | "classify"
                | "categorize"
                | "produce"
                | "rotor"
                | "batch"
                | "reduce"
                | "combinations"
                | "permutations"
                | "values"
                | "List"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "Seq"
                | "item"
                | "cache"
                | "pick"
                | "roll"
                | "keys"
                | "kv"
                | "pairs"
                | "antipairs"
        )
    }

    /// Force a LazyList by running its compiled bytecode in the VM.
    /// Falls back to interpreter if no compiled code is available.
    pub(super) fn force_lazy_list_vm(
        &mut self,
        list: &LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        // Check cache first
        if let Some(cached) = list.cache.lock().unwrap().clone() {
            return Ok(cached);
        }

        // If no compiled code, fall back to interpreter
        let (cc, fns) = match (&list.compiled_code, &list.compiled_fns) {
            (Some(cc), Some(fns)) => (cc.clone(), fns.clone()),
            _ => return self.interpreter.force_lazy_list_bridge(list),
        };

        // Save current VM state.
        // Flush any pending locals→env writes so env is consistent.
        // We do a manual sync rather than ensure_env_synced since we don't
        // have the outer code here -- we'll restore locals directly.
        let saved_env = self.interpreter.clone_env();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_env_dirty = self.env_dirty;
        let saved_locals_dirty = self.locals_dirty;

        // Set up lazy list's environment
        *self.interpreter.env_mut() = list.env.clone();

        // Push gather items collector
        let saved_gather_len = self.interpreter.gather_items_len();
        self.interpreter.push_gather_items(Vec::new());
        self.interpreter.push_gather_take_limit(None);

        // Initialize locals for the compiled code
        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        self.env_dirty = false;
        self.locals_dirty = false;
        self.stack = Vec::new();

        // Run the compiled code using the lazy list's own compiled_fns.
        // Outer scope subs are available via the env as Value::Sub.
        let run_fns = fns.as_ref();

        let mut ip = 0;
        let mut run_result = Ok(());
        while ip < cc.ops.len() {
            match self.exec_one(&cc, &mut ip, run_fns) {
                Ok(()) => {}
                Err(e) if e.is_warn => {
                    if !self.interpreter.warning_suppressed() {
                        self.interpreter.write_warn_to_stderr(&e.message);
                    }
                    ip += 1;
                    continue;
                }
                Err(e) => {
                    run_result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        // Collect gather items
        let items = self.interpreter.pop_gather_items().unwrap_or_default();
        self.interpreter.pop_gather_take_limit();

        // Clean up extra gather items if needed
        while self.interpreter.gather_items_len() > saved_gather_len {
            self.interpreter.pop_gather_items();
            self.interpreter.pop_gather_take_limit();
        }

        // Merge env changes back (like the interpreter version does)
        let mut merged_env = saved_env;
        for (k, v) in self.interpreter.env().iter() {
            merged_env.insert(k.clone(), v.clone());
        }
        *self.interpreter.env_mut() = merged_env;

        // Restore VM state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = saved_env_dirty;
        self.locals_dirty = saved_locals_dirty;

        // Check for errors
        run_result?;

        // Cache the result
        *list.cache.lock().unwrap() = Some(items.clone());
        Ok(items)
    }

    /// Force a LazyList into a Seq by evaluating the gather body.
    fn force_lazy_if_needed(&mut self, val: Value) -> Result<Value, RuntimeError> {
        if let Value::LazyList(ll) = &val {
            let items = self.force_lazy_list_vm(ll)?;
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
        // Auto-FETCH Proxy containers in binary operations
        let left = self.interpreter.auto_fetch_proxy(&left)?;
        let right = self.interpreter.auto_fetch_proxy(&right)?;
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(right_kind.clone(), results?));
        }
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
    /// For `!~~` (negate=true), we compute `~~` first and then negate the
    /// collapsed result.  Raku defines `$x !~~ $y` as `not ($x ~~ $y)`,
    /// where `not` collapses junctions before negating.
    pub(super) fn eval_smartmatch_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        negate: bool,
    ) -> Result<Value, RuntimeError> {
        // For !~~, compute ~~ first, then negate the collapsed boolean.
        if negate {
            let match_result = self.eval_smartmatch_with_junctions(left, right, false)?;
            let bool_val = match_result.truthy();
            return Ok(Value::Bool(!bool_val));
        }
        if let (
            Value::Junction {
                kind: left_kind,
                values: _,
            },
            Value::Junction {
                kind: right_kind,
                values: right_values,
            },
        ) = (&left, &right)
            && Self::thread_right_first(left_kind, right_kind)
        {
            let results: Result<Vec<Value>, RuntimeError> = right_values
                .iter()
                .cloned()
                .map(|v| self.eval_smartmatch_with_junctions(left.clone(), v, false))
                .collect();
            return Ok(Value::junction(right_kind.clone(), results?));
        }
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_smartmatch_with_junctions(v, right.clone(), false))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_smartmatch_with_junctions(left.clone(), v, false))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        self.smart_match_op(left, right)
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        // When RHS is Whatever, autoprime: return a WhateverCode that takes
        // one argument and smartmatches LHS against it.
        // In Raku, `$x ~~ *` produces `-> $a { $x ~~ $a }`.
        if matches!(&right, Value::Whatever) {
            use crate::ast::{Expr, Stmt};
            use crate::env::Env;
            let mut env = Env::new();
            env.insert(
                "__mutsu_callable_type".to_string(),
                Value::str_from("WhateverCode"),
            );
            // Capture the LHS value in the closure environment
            env.insert("__wc_sm_lhs".to_string(), left);
            let param = "__wc_0".to_string();
            let body = vec![Stmt::Expr(Expr::Binary {
                left: Box::new(Expr::Var("__wc_sm_lhs".to_string())),
                op: crate::token_kind::TokenKind::SmartMatch,
                right: Box::new(Expr::Var(param.clone())),
            })];
            return Ok(Value::make_sub(
                Symbol::intern("GLOBAL"),
                Symbol::intern("<whatevercode-smartmatch>"),
                vec![param],
                Vec::new(),
                body,
                false,
                env,
            ));
        }
        let is_regex = matches!(
            &right,
            Value::Regex(_)
                | Value::RegexWithAdverbs { .. }
                | Value::Routine { is_regex: true, .. }
        );
        let matched = self.vm_smart_match(&left, &right);
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
                // Clear capture variables ($0, $1, ...) and $/ on failed match
                self.interpreter
                    .env_mut()
                    .insert("/".to_string(), Value::Nil);
                for i in 0..10 {
                    self.interpreter.env_mut().remove(&i.to_string());
                }
                self.env_dirty = true;
                Ok(Value::Nil)
            }
        } else {
            Ok(Value::Bool(matched))
        }
    }

    /// Strip hyper operator delimiters (>>...<<, >>...>>, <<...<<, <<...>>)
    /// and their Unicode variants, returning the inner operator if found.
    fn strip_hyper_delimiters_str(s: &str) -> Option<&str> {
        let after_left = s
            .strip_prefix(">>")
            .or_else(|| s.strip_prefix("<<"))
            .or_else(|| s.strip_prefix('\u{00BB}'))
            .or_else(|| s.strip_prefix('\u{00AB}'))?;
        let inner = after_left
            .strip_suffix(">>")
            .or_else(|| after_left.strip_suffix("<<"))
            .or_else(|| after_left.strip_suffix('\u{00BB}'))
            .or_else(|| after_left.strip_suffix('\u{00AB}'))?;
        if inner.is_empty() {
            return None;
        }
        Some(inner)
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
        // Bare Z: zip two lists into tuples (used by [Z] reduction).
        // When left elements are already lists (from a prior Z fold), flatten them
        // so that [Z] (a,b,c),(d,e,f),(g,h,i) produces (a d g), (b e h), (c f i).
        if op == "Z" {
            let left_list = runtime::value_to_list(left);
            let right_list = runtime::value_to_list(right);
            let len = left_list.len().min(right_list.len());
            let mut results = Vec::new();
            for i in 0..len {
                let mut tuple = match &left_list[i] {
                    Value::Array(items, kind) if !kind.is_itemized() => items.to_vec(),
                    other => vec![other.clone()],
                };
                tuple.push(right_list[i].clone());
                results.push(Value::array(tuple));
            }
            return Ok(Value::array(results));
        }
        // Z-prefixed meta-operator: zip two lists element-wise with the inner op.
        if let Some(inner_op) = op.strip_prefix('Z')
            && !inner_op.is_empty()
        {
            let left_list = runtime::value_to_list(left);
            let right_list = runtime::value_to_list(right);
            let len = left_list.len().min(right_list.len());
            let mut results = Vec::new();
            for i in 0..len {
                results.push(self.eval_reduction_operator_values(
                    inner_op,
                    &left_list[i],
                    &right_list[i],
                )?);
            }
            return Ok(Value::array(results));
        }
        // Hyper operator forms: >>op<<, >>op>>, <<op<<, <<op>>
        // Apply inner op element-wise to two lists.
        if let Some(inner_op) = Self::strip_hyper_delimiters_str(op) {
            let left_list = runtime::value_to_list(left);
            let right_list = runtime::value_to_list(right);
            let dwim_left = op.starts_with("<<") || op.starts_with('\u{00AB}');
            let dwim_right = op.ends_with(">>") || op.ends_with('\u{00BB}');
            let len = if dwim_left && dwim_right {
                left_list.len().max(right_list.len())
            } else if dwim_left {
                right_list.len()
            } else if dwim_right {
                left_list.len()
            } else {
                left_list.len().max(right_list.len())
            };
            let mut results = Vec::with_capacity(len);
            for i in 0..len {
                let l = if left_list.is_empty() {
                    &Value::Int(0.into())
                } else {
                    &left_list[i % left_list.len()]
                };
                let r = if right_list.is_empty() {
                    &Value::Int(0.into())
                } else {
                    &right_list[i % right_list.len()]
                };
                results.push(self.eval_reduction_operator_values(inner_op, l, r)?);
            }
            return Ok(Value::array(results));
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
                        return self.vm_call_on_value(callable, args, None);
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
                        return self.vm_call_on_value(callable, args.clone(), None);
                    }
                    if let Some(callable) = self
                        .interpreter
                        .env()
                        .get(&format!("&{}", normalized_op))
                        .cloned()
                    {
                        return self.vm_call_on_value(callable, args.clone(), None);
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
        // Match coerces to Numeric via its matched string
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && class_name == "Match"
            && let Some(str_val) = attributes.get("str")
        {
            let s = str_val.to_string_value();
            let s = s.trim();
            if let Ok(i) = s.parse::<i64>() {
                return Ok(Value::Int(i));
            }
            if let Ok(f) = s.parse::<f64>() {
                return Ok(Value::Num(f));
            }
            return Ok(Value::Int(0));
        }
        // Check if type is known to be Real/Numeric, OR if the class has a
        // user-defined Numeric method (for classes like `class Blue { method Numeric { 3 } }`)
        let known_numeric = self.interpreter.type_matches_value("Real", &value)
            || self.interpreter.type_matches_value("Numeric", &value);
        let has_numeric_method = if let Value::Instance { ref class_name, .. } = value {
            let cn = class_name.to_string();
            self.interpreter.has_user_method(&cn, "Numeric")
        } else {
            false
        };
        if !known_numeric && !has_numeric_method {
            return Ok(value);
        }
        self.try_compiled_method_or_interpret(value.clone(), "Numeric", vec![])
            .or_else(|_| self.try_compiled_method_or_interpret(value.clone(), "Bridge", vec![]))
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
                continue;
            }
            if let Some(bare) = name
                .strip_prefix('$')
                .or_else(|| name.strip_prefix('@'))
                .or_else(|| name.strip_prefix('%'))
                .or_else(|| name.strip_prefix('&'))
                && let Some(val) = self.interpreter.env().get(bare)
            {
                self.locals[i] = val.clone();
            }
        }
    }

    pub(super) fn sync_env_from_locals(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            self.set_env_with_main_alias(name, self.locals[i].clone());
        }
    }

    pub(super) fn sync_regex_interpolation_env_from_locals(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            if name == "_"
                || name == "/"
                || name == "!"
                || name == "¢"
                || name.chars().all(|ch| ch.is_ascii_digit())
            {
                continue;
            }
            self.set_env_with_main_alias(name, self.locals[i].clone());
        }
    }

    /// Sync only simple locals to env (the ones whose SetLocal fast path
    /// skips env writes). Only runs when locals_dirty is set.
    pub(super) fn ensure_env_synced(&mut self, code: &CompiledCode) {
        if self.locals_dirty {
            for (i, name) in code.locals.iter().enumerate() {
                if code.simple_locals[i] {
                    self.set_env_with_main_alias(name, self.locals[i].clone());
                }
            }
            self.locals_dirty = false;
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

    pub(super) fn locals_get_by_name(&self, code: &CompiledCode, name: &str) -> Option<Value> {
        self.find_local_slot(code, name)
            .map(|slot| self.locals[slot].clone())
    }

    pub(super) fn locals_set_by_name(&mut self, code: &CompiledCode, name: &str, val: Value) {
        if let Some(slot) = self.find_local_slot(code, name) {
            self.locals[slot] = val;
        }
    }

    pub(super) fn try_native_method(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        fn collection_contains_instance(value: &Value) -> bool {
            match value {
                Value::Instance { .. } => true,
                v if v.as_list_items().is_some() => v
                    .as_list_items()
                    .unwrap()
                    .iter()
                    .any(collection_contains_instance),
                Value::Hash(map) => map.values().any(collection_contains_instance),
                _ => false,
            }
        }
        let bypass_supply_extrema_fastpath = (method_sym == "max"
            || method_sym == "min"
            || method_sym == "lines"
            || method_sym == "elems"
            || method_sym == "head"
            || method_sym == "flat"
            || method_sym == "sort"
            || method_sym == "comb"
            || method_sym == "words"
            || method_sym == "batch"
            || method_sym == "rotor"
            || method_sym == "rotate"
            || method_sym == "produce"
            || method_sym == "snip"
            || method_sym == "minmax"
            || method_sym == "start"
            || method_sym == "wait"
            || method_sym == "zip"
            || method_sym == "zip-latest")
            && (matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supply"
            ) || matches!(target, Value::Package(name) if name == "Supply"));
        let bypass_supplier_supply_fastpath = method_sym == "Supply"
            && args.is_empty()
            && matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supplier"
            );
        let bypass_gist_fastpath =
            method_sym == "gist" && args.is_empty() && collection_contains_instance(target);
        let bypass_pickroll_type_fastpath = (method_sym == "pick" || method_sym == "roll")
            && args.len() <= 1
            && matches!(target, Value::Package(_) | Value::Str(_));
        let bypass_squish_fastpath = method_sym == "squish";
        let bypass_tail_fastpath = method_sym == "tail";
        let bypass_numeric_bridge_instance_fastpath = matches!(target, Value::Instance { .. })
            && (self.interpreter.type_matches_value("Real", target)
                || self.interpreter.type_matches_value("Numeric", target)
                || matches!(target, Value::Instance { class_name, .. }
                    if self.interpreter.has_user_method(&class_name.resolve(), "Bridge")));
        let method_name = method_sym.resolve();
        let bypass_runtime_native_instance_fastpath = matches!(target, Value::Instance { class_name, .. }
                if self
                    .interpreter
                    .is_native_method(&class_name.resolve(), &method_name));
        // Proxy containers must auto-FETCH before dispatching methods (except meta-methods)
        let bypass_proxy = matches!(target, Value::Proxy { .. })
            && !matches!(
                method_sym.resolve().as_ref(),
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            );
        if bypass_supply_extrema_fastpath
            || bypass_supplier_supply_fastpath
            || bypass_gist_fastpath
            || bypass_pickroll_type_fastpath
            || bypass_squish_fastpath
            || bypass_tail_fastpath
            || bypass_numeric_bridge_instance_fastpath
            || bypass_runtime_native_instance_fastpath
            || bypass_proxy
        {
            return None;
        }
        let result = if args.len() == 2 {
            crate::builtins::native_method_2arg(target, method_sym, &args[0], &args[1])
        } else if args.len() == 1 {
            crate::builtins::native_method_1arg(target, method_sym, &args[0])
        } else if args.is_empty() {
            crate::builtins::native_method_0arg(target, method_sym)
        } else {
            return None;
        };

        if method_name == "decode" {
            return result.map(|res| {
                res.map(|value| match value {
                    Value::Str(decoded) => {
                        Value::str(self.interpreter.translate_newlines_for_decode(&decoded))
                    }
                    other => other,
                })
            });
        }

        result
    }

    pub(super) fn try_native_function(
        &mut self,
        name_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.iter().any(|arg| matches!(arg, Value::Instance { .. })) {
            return None;
        }
        crate::builtins::native_function(name_sym, args)
    }

    /// Try compiled function dispatch first, then native, then interpreter fallback.
    /// Returns the result of whichever path succeeds.
    pub(super) fn call_function_compiled_first(
        &mut self,
        name: &str,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        if let Some(cf) = self.find_compiled_function(compiled_fns, name, &args) {
            let pkg = self.interpreter.current_package().to_string();
            return self.call_compiled_function_named(cf, args, compiled_fns, &pkg, name);
        }
        if let Some(native_result) =
            self.try_native_function(crate::symbol::Symbol::intern(name), &args)
        {
            return native_result;
        }
        self.interpreter.call_function(name, args)
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
            let key_fp = format!("{name}/{}#{:x}", arity, expected_fingerprint);
            if let Some(cf) = compiled_fns.get(&key_fp)
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
        // Try fingerprint-keyed lookup (for same-named subs in different scopes)
        let key_fp = format!("{}::{}/{}#{:x}", pkg, name, arity, expected_fingerprint);
        if let Some(cf) = compiled_fns.get(&key_fp)
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
            let key_fp_global = format!("GLOBAL::{}/{}#{:x}", name, arity, expected_fingerprint);
            if let Some(cf) = compiled_fns.get(&key_fp_global)
                && matches_resolved(cf)
            {
                return Some(cf);
            }
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
        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        let return_spec = cf.return_type.clone();

        self.interpreter.inject_pending_callsite_line();
        self.interpreter.push_caller_env();

        // Push Sub value to block_stack for callframe().code
        let sub_val = Value::make_sub(
            Symbol::intern(fn_package),
            Symbol::intern(fn_name),
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
            let frame = self.pop_call_frame();
            drop(frame);
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        // Set current_package to the function's defining package so that default
        // value expressions can resolve package-scoped functions (e.g. &double).
        let saved_package = self.interpreter.current_package().to_string();
        if !fn_package.is_empty() && fn_package != "GLOBAL" {
            self.interpreter.set_current_package(fn_package.to_string());
        }
        let rw_bindings =
            match self
                .interpreter
                .bind_function_args_values(&cf.param_defs, &cf.params, &args)
            {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.interpreter.set_current_package(saved_package);
                    if !fn_name.is_empty() {
                        self.interpreter.pop_routine();
                    }
                    self.interpreter
                        .pop_test_assertion_context(pushed_assertion);
                    self.interpreter.pop_caller_env();
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.interpreter.env_mut() = frame.saved_env;
                    return Err(Interpreter::enhance_binding_error(
                        e,
                        fn_name,
                        &cf.param_defs,
                        &args,
                    ));
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

        self.interpreter.set_current_package(saved_package);
        if !fn_name.is_empty() {
            self.interpreter.pop_routine();
        }
        self.interpreter
            .pop_test_assertion_context(pushed_assertion);
        self.interpreter.pop_block();

        let frame = self.pop_call_frame();
        let mut restored_env = frame.saved_env;
        self.interpreter
            .pop_caller_env_with_writeback(&mut restored_env);
        self.interpreter
            .apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        let rw_sources: std::collections::HashSet<String> = rw_bindings
            .iter()
            .flat_map(|(_, source)| {
                let mut names = vec![source.clone()];
                // For indexed rw bindings ("%h\0a\0b"), also add the base variable name
                if let Some(base) = source.split('\0').next()
                    && base != source
                {
                    names.push(base.to_string());
                }
                names
            })
            .collect();
        let local_names: std::collections::HashSet<&String> = cf.code.locals.iter().collect();
        for (k, v) in self.interpreter.env().iter() {
            if restored_env.contains_key(k) && !local_names.contains(k) && !rw_sources.contains(k) {
                restored_env.insert(k.clone(), v.clone());
            }
        }
        *self.interpreter.env_mut() = restored_env;

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

    /// Call a compiled closure (Value::Sub with compiled_code).
    pub(super) fn call_compiled_closure(
        &mut self,
        data: &crate::value::SubData,
        cc: &CompiledCode,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let (mut args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        if callsite_line.is_some() {
            self.interpreter.set_pending_callsite_line(callsite_line);
        }

        // Apply assumed args from .assuming() (same logic as tree-walker call_sub_value)
        if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
            let mut positional = Vec::new();
            let mut named = data.assumed_named.clone();
            let mut incoming_positional = Vec::new();
            for arg in &args {
                if let Value::Pair(key, boxed) = arg {
                    named.insert(key.clone(), *boxed.clone());
                } else {
                    incoming_positional.push(arg.clone());
                }
            }
            let mut incoming_idx = 0usize;
            for assumed in &data.assumed_positional {
                let is_placeholder = matches!(assumed, Value::Whatever)
                    || matches!(assumed, Value::Num(f) if f.is_infinite())
                    || matches!(assumed, Value::Rat(_, 0));
                if is_placeholder {
                    if incoming_idx < incoming_positional.len() {
                        positional.push(incoming_positional[incoming_idx].clone());
                        incoming_idx += 1;
                    }
                } else {
                    positional.push(assumed.clone());
                }
            }
            positional.extend(incoming_positional.into_iter().skip(incoming_idx));
            for (key, value) in named {
                positional.push(Value::Pair(key, Box::new(value)));
            }
            args = positional;
        }

        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;

        self.interpreter.inject_pending_callsite_line();

        // Merge captured environment into current env (or_insert = don't overwrite existing)
        for (k, v) in &data.env {
            self.interpreter
                .env_mut()
                .entry(k.clone())
                .or_insert_with(|| v.clone());
        }

        self.interpreter.push_caller_env();

        // Push Sub value to block_stack for callframe().code
        // Also set &?BLOCK as a weak self-reference (mirrors resolution.rs)
        let block_arc = std::sync::Arc::new(crate::value::SubData {
            package: data.package,
            name: data.name,
            params: data.params.clone(),
            param_defs: data.param_defs.clone(),
            body: vec![],
            is_rw: data.is_rw,
            is_raw: data.is_raw,
            env: data.env.clone(),
            assumed_positional: data.assumed_positional.clone(),
            assumed_named: data.assumed_named.clone(),
            id: data.id,
            empty_sig: data.empty_sig,
            compiled_code: data.compiled_code.clone(),
        });
        self.interpreter.env_mut().insert(
            "&?BLOCK".to_string(),
            Value::WeakSub(std::sync::Arc::downgrade(&block_arc)),
        );
        self.interpreter.push_block(Value::Sub(block_arc));

        // Push routine info and callable_id for leave/return targeting
        self.interpreter
            .push_routine(data.package.resolve(), data.name.resolve());
        self.interpreter.env_mut().insert(
            "__mutsu_callable_id".to_string(),
            Value::Int(data.id as i64),
        );

        if data.empty_sig && !args.is_empty() {
            self.interpreter.pop_routine();
            self.interpreter.pop_block();
            self.interpreter.pop_caller_env();
            self.stack.truncate(saved_stack_depth);
            let frame = self.pop_call_frame();
            *self.interpreter.env_mut() = frame.saved_env;
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        // Bind parameters
        let rw_bindings =
            match self
                .interpreter
                .bind_function_args_values(&data.param_defs, &data.params, &args)
            {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.interpreter.pop_routine();
                    self.interpreter.pop_block();
                    self.interpreter.pop_caller_env();
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.interpreter.env_mut() = frame.saved_env;
                    return Err(Interpreter::enhance_binding_error(
                        e,
                        &data.name.resolve(),
                        &data.param_defs,
                        &args,
                    ));
                }
            };

        // Handle implicit $_ for bare blocks (no explicit params, single arg)
        let uses_positional = data.params.iter().any(|p| p != "_" && !p.starts_with(':'));
        if !uses_positional
            && !data.params.is_empty()
            && data.params.iter().all(|p| p.starts_with('$'))
        {
            // Named params with placeholders: handled by bind_function_args_values
        } else if !uses_positional && !args.is_empty() {
            if let Some(first) = args.iter().find(|v| !matches!(v, Value::Pair(_, _))) {
                self.interpreter
                    .env_mut()
                    .insert("_".to_string(), first.clone());
            }
        } else if data.params.is_empty() && args.is_empty() && data.name == "" {
            let caller_topic = self.call_frames.last().unwrap().saved_env.get("_").cloned();
            if let Some(topic) = caller_topic {
                self.interpreter.env_mut().insert("_".to_string(), topic);
            }
        }

        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, local_name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cc.ops.len() {
            match self.exec_one(cc, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{}::{}", data.package, data.name);
                    let matches_frame = if let Some(target_id) = e.leave_callable_id {
                        target_id == data.id
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

        // Sync state variables back
        for (slot, key) in &cc.state_locals {
            let local_name = &cc.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }

        self.interpreter.pop_routine();
        self.interpreter.pop_block();

        if self.env_dirty {
            self.sync_locals_from_env(cc);
        }

        // Sync locals back to env so captured variable changes are visible
        for (i, local_name) in cc.locals.iter().enumerate() {
            if !local_name.is_empty() {
                self.interpreter
                    .env_mut()
                    .insert(local_name.clone(), self.locals[i].clone());
            }
        }

        // Environment writeback: merge changes back to caller
        let frame = self.pop_call_frame();
        let mut restored_env = frame.saved_env;
        self.interpreter
            .pop_caller_env_with_writeback(&mut restored_env);
        self.interpreter
            .apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        let rw_sources: std::collections::HashSet<String> = rw_bindings
            .iter()
            .flat_map(|(_, source)| {
                let mut names = vec![source.clone()];
                if let Some(base) = source.split('\0').next()
                    && base != source
                {
                    names.push(base.to_string());
                }
                names
            })
            .collect();
        let captured_names: std::collections::HashSet<&str> =
            data.env.keys().map(|s| s.as_str()).collect();
        // Write back captured-variable changes, but NOT the closure's own
        // parameters/locals (which live in cc.locals).  Without this filter,
        // recursive &?BLOCK calls clobber the outer frame's $n, etc.
        let local_names: std::collections::HashSet<&str> =
            cc.locals.iter().map(|s| s.as_str()).collect();
        for (k, v) in self.interpreter.env().iter() {
            if k != "_"
                && k != "@_"
                && !rw_sources.contains(k)
                && (restored_env.contains_key(k)
                    || captured_names.contains(k.as_str())
                    || k.starts_with("__mutsu_predictive_seq_iter::")
                    || k.starts_with("__mutsu_sigilless_alias::!"))
                && (!local_names.contains(k.as_str()) || captured_names.contains(k.as_str()))
            {
                restored_env.insert(k.clone(), v.clone());
            }
        }
        self.interpreter
            .merge_sigilless_alias_writes(&mut restored_env, self.interpreter.env());
        *self.interpreter.env_mut() = restored_env;

        let return_spec = data.env.get("__mutsu_return_type").and_then(|v| match v {
            Value::Str(s) => Some(s.to_string()),
            _ => None,
        });

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                // For closures, absorb `return` — don't re-propagate as error.
                let base_val = explicit_return.unwrap_or(ret_val);
                self.interpreter
                    .finalize_return_with_spec(Ok(base_val), return_spec.as_deref())
            }
            Err(e) => Err(e),
        }
    }

    /// Call a compiled method body (MethodDef with compiled_code).
    /// Mirrors `Interpreter::run_instance_method_resolved` but executes bytecode.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn call_compiled_method(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_name: &str,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        mut attributes: HashMap<String, Value>,
        args: Vec<Value>,
        invocant: Option<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        // Build the base (self) value
        let mut base = if let Some(inv) = invocant {
            inv
        } else if attributes.is_empty() {
            Value::Package(crate::symbol::Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(
                crate::symbol::Symbol::intern(receiver_class_name),
                attributes.clone(),
            )
        };

        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;

        // Clear var_bindings so attribute aliases from outer interpreter-level
        // method calls don't leak into compiled method locals (e.g. `x → !x`
        // from run_instance_method_resolved shadowing a local parameter `x`).
        let saved_var_bindings = self.interpreter.take_var_bindings();

        self.interpreter.push_method_class(owner_class.to_string());

        // Detect role context: use the pre-computed role_origin stored on the
        // MethodDef (set during role composition) instead of expensive fingerprint
        // matching on every call.
        let role_context = if self.interpreter.is_role(owner_class) {
            Some(owner_class.to_string())
        } else {
            method_def
                .original_role
                .as_ref()
                .or(method_def.role_origin.as_ref())
                .cloned()
        };

        // Set ::?CLASS / ::?ROLE
        self.interpreter.env_mut().insert(
            "?CLASS".to_string(),
            Value::Package(crate::symbol::Symbol::intern(owner_class)),
        );
        if let Some(role_name) = role_context {
            self.interpreter.env_mut().insert(
                "?ROLE".to_string(),
                Value::Package(crate::symbol::Symbol::intern(&role_name)),
            );
        } else {
            self.interpreter.env_mut().remove("?ROLE");
        }

        // Set self and __ANON_STATE__ (used by `$.foo` desugaring inside methods)
        self.interpreter
            .env_mut()
            .insert("self".to_string(), base.clone());
        self.interpreter
            .env_mut()
            .insert("__ANON_STATE__".to_string(), base.clone());

        // In Raku, methods do NOT set $_ to the invocant by default.
        // $_ in a method body is Any unless the invocant is explicitly named $_
        // (e.g. `method foo ($_: ) { ... }`). The invocant binding loop below
        // will set $_ back to self if the invocant param is named "_".
        self.interpreter.env_mut().insert(
            "_".to_string(),
            Value::Package(crate::symbol::Symbol::intern("Any")),
        );

        // Role param bindings
        if let Some(role_bindings) = self
            .interpreter
            .class_role_param_bindings(owner_class)
            .cloned()
        {
            for (name, value) in &role_bindings {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), value.clone());
            }
        } else if let Some(role_bindings) = self
            .interpreter
            .class_role_param_bindings(receiver_class_name)
            .cloned()
        {
            for (name, value) in &role_bindings {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), value.clone());
            }
        }

        // Skip invocant param, bind remaining
        let mut bind_params = Vec::new();
        let mut bind_param_defs = Vec::new();
        for (idx, param_name) in method_def.params.iter().enumerate() {
            let is_invocant = method_def
                .param_defs
                .get(idx)
                .map(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                .unwrap_or(false);
            if is_invocant {
                if let Some(pd) = method_def.param_defs.get(idx)
                    && let Some(constraint) = &pd.type_constraint
                {
                    if let Some(captured_name) = constraint.strip_prefix("::") {
                        self.interpreter.bind_type_capture(captured_name, &base);
                    } else {
                        let coercion_target = if let Some(open) = constraint.find('(') {
                            if constraint.ends_with(')') && open > 0 {
                                Some(&constraint[..open])
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        let expected = coercion_target.unwrap_or(constraint.as_str());
                        if coercion_target.is_some() {
                            let mut candidate = self
                                .interpreter
                                .try_coerce_value_for_constraint(constraint, base.clone())
                                .unwrap_or_else(|_| base.clone());
                            if !self.interpreter.type_matches_value(expected, &candidate)
                                && let Ok(coerced) = self.try_compiled_method_or_interpret(
                                    base.clone(),
                                    expected,
                                    vec![],
                                )
                            {
                                candidate = coerced;
                            }
                            if self.interpreter.type_matches_value(expected, &candidate) {
                                base = candidate;
                                self.interpreter
                                    .env_mut()
                                    .insert("self".to_string(), base.clone());
                            }
                        } else if !self.interpreter.type_matches_value(constraint, &base)
                            && let Ok(coerced) = self
                                .interpreter
                                .try_coerce_value_for_constraint(constraint, base.clone())
                        {
                            base = coerced;
                            self.interpreter
                                .env_mut()
                                .insert("self".to_string(), base.clone());
                        }
                        if !self.interpreter.type_matches_value(expected, &base) {
                            self.interpreter.restore_var_bindings(saved_var_bindings);
                            self.interpreter.pop_method_class();
                            self.stack.truncate(saved_stack_depth);
                            let frame = self.pop_call_frame();
                            *self.interpreter.env_mut() = frame.saved_env;
                            return Err(RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                                param_name,
                                constraint,
                                crate::runtime::value_type_name(&base)
                            )));
                        }
                    }
                }
                self.interpreter
                    .env_mut()
                    .insert(param_name.clone(), base.clone());
                continue;
            }
            bind_params.push(param_name.clone());
            if let Some(pd) = method_def.param_defs.get(idx) {
                bind_param_defs.push(pd.clone());
            }
        }

        // Bind attributes
        for (attr_name, attr_val) in &attributes {
            if let Some(actual_attr) = attr_name.strip_prefix(ATTR_ALIAS_META_PREFIX) {
                if let Value::Str(source_name) = attr_val {
                    // Set up bidirectional alias: !x ↔ alias_name
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_alias::!{}", actual_attr),
                        Value::str(source_name.to_string()),
                    );
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_readonly::!{}", actual_attr),
                        Value::Bool(false),
                    );
                    // Reverse alias: alias_name → !attr so writing to $x updates $!x
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_alias::{}", source_name),
                        Value::str(format!("!{}", actual_attr)),
                    );
                    self.interpreter.env_mut().insert(
                        format!("__mutsu_sigilless_readonly::{}", source_name),
                        Value::Bool(false),
                    );
                    // Also set up the alias name with the current attribute value
                    if let Some(attr_value) = attributes.get(actual_attr) {
                        self.interpreter
                            .env_mut()
                            .insert(source_name.to_string(), attr_value.clone());
                    }
                }
                continue;
            }
            self.interpreter
                .env_mut()
                .insert(format!("!{}", attr_name), attr_val.clone());
            self.interpreter
                .env_mut()
                .insert(format!(".{}", attr_name), attr_val.clone());
            match attr_val {
                Value::Array(..) => {
                    self.interpreter
                        .env_mut()
                        .insert(format!("@!{}", attr_name), attr_val.clone());
                    self.interpreter
                        .env_mut()
                        .insert(format!("@.{}", attr_name), attr_val.clone());
                }
                Value::Hash(..) => {
                    self.interpreter
                        .env_mut()
                        .insert(format!("%!{}", attr_name), attr_val.clone());
                    self.interpreter
                        .env_mut()
                        .insert(format!("%.{}", attr_name), attr_val.clone());
                }
                _ => {}
            }
        }

        // Bind method parameters
        match self
            .interpreter
            .bind_function_args_values(&bind_param_defs, &bind_params, &args)
        {
            Ok(_) => {}
            Err(e) => {
                self.interpreter.restore_var_bindings(saved_var_bindings);
                self.interpreter.pop_method_class();
                self.stack.truncate(saved_stack_depth);
                let frame = self.pop_call_frame();
                *self.interpreter.env_mut() = frame.saved_env;
                return Err(e);
            }
        }

        // Initialize locals from env
        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, local_name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        // Execute bytecode
        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        while ip < cc.ops.len() {
            match self.exec_one(cc, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{}::{}", owner_class, method_name);
                    let matches_frame = if let Some(_target_id) = e.leave_callable_id {
                        // Methods don't have callable IDs, so this won't match
                        false
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
                // Implicit return from env "_"
                self.interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil)
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back
        for (slot, key) in &cc.state_locals {
            let local_name = &cc.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }

        // Sync locals back to env
        for (i, local_name) in cc.locals.iter().enumerate() {
            if !local_name.is_empty() {
                self.interpreter
                    .env_mut()
                    .insert(local_name.clone(), self.locals[i].clone());
            }
        }

        writeback_attributes(self.interpreter.env(), &mut attributes);
        // Collect keys introduced by the method frame so they don't bleed
        // back into the caller's env (e.g. method param `$g` vs caller `$g`).
        let mut method_local_keys: HashSet<String> = HashSet::from_iter([
            "self".to_string(),
            "__ANON_STATE__".to_string(),
            "?CLASS".to_string(),
            "?ROLE".to_string(),
            "_".to_string(),
        ]);
        for p in &method_def.params {
            method_local_keys.insert(p.clone());
        }
        for attr_name in attributes.keys() {
            if let Some(actual_attr) = attr_name.strip_prefix(ATTR_ALIAS_META_PREFIX) {
                // Add the alias name itself to method_local_keys
                if let Some(Value::Str(alias_name)) = attributes.get(attr_name) {
                    method_local_keys.insert(alias_name.to_string());
                    method_local_keys.insert(actual_attr.to_string());
                }
                continue;
            }
            method_local_keys.insert(format!("!{}", attr_name));
            method_local_keys.insert(format!(".{}", attr_name));
            method_local_keys.insert(format!("@!{}", attr_name));
            method_local_keys.insert(format!("@.{}", attr_name));
            method_local_keys.insert(format!("%!{}", attr_name));
            method_local_keys.insert(format!("%.{}", attr_name));
        }
        for local_name in &cc.locals {
            if !local_name.is_empty() {
                method_local_keys.insert(local_name.clone());
            }
        }
        let merged_env = merge_method_env(
            &self.call_frames.last().unwrap().saved_env,
            self.interpreter.env(),
            &method_local_keys,
        );

        // Merge var_bindings: keep any new bindings set during method execution
        // (e.g. from $CALLER:: rebinding), then restore original bindings for
        // keys not touched during execution.
        let method_var_bindings = self.interpreter.take_var_bindings();
        let mut restored_bindings = saved_var_bindings;
        for (k, v) in method_var_bindings {
            restored_bindings.insert(k, v);
        }
        self.interpreter.restore_var_bindings(restored_bindings);

        self.interpreter.pop_method_class();
        let _frame = self.pop_call_frame();
        *self.interpreter.env_mut() = merged_env;

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };

        // Apply return type spec (e.g. `--> 5` returns literal 5 from empty body)
        let final_result = if let Some(ref return_spec) = method_def.return_type {
            self.interpreter
                .finalize_return_with_spec(final_result, Some(return_spec.as_str()))
        } else {
            match final_result {
                Ok(v) => Ok(v),
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                Err(e) => Err(e),
            }
        };

        // Adjust return value if it's the same instance (update attributes)
        final_result.map(|v| {
            let adjusted = match (&base, &v) {
                (
                    Value::Instance {
                        class_name,
                        id: base_id,
                        ..
                    },
                    Value::Instance { id: ret_id, .. },
                ) if base_id == ret_id => {
                    Value::make_instance_with_id(*class_name, attributes.clone(), *base_id)
                }
                _ => v,
            };
            (adjusted, attributes)
        })
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

    /// Evaluate truthiness of a value, including dispatch to user-defined Bool methods.
    /// For Package (type objects) and Instance values, checks if the class defines
    /// a custom Bool method and calls it. Falls back to Value::truthy() otherwise.
    pub(super) fn eval_truthy(&mut self, val: &Value) -> bool {
        match val {
            Value::Package(name) => {
                let class_name = name.resolve().to_string();
                if self
                    .interpreter
                    .resolve_method_with_owner(&class_name, "Bool", &[])
                    .is_some()
                    && let Ok(result) =
                        self.try_compiled_method_or_interpret(val.clone(), "Bool", vec![])
                {
                    return result.truthy();
                }
                val.truthy()
            }
            Value::Instance { class_name, .. } => {
                let cn = class_name.resolve().to_string();
                if self
                    .interpreter
                    .resolve_method_with_owner(&cn, "Bool", &[])
                    .is_some()
                    && let Ok(result) =
                        self.try_compiled_method_or_interpret(val.clone(), "Bool", vec![])
                {
                    return result.truthy();
                }
                val.truthy()
            }
            Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { is_regex: true, .. } => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                self.vm_smart_match(&topic, val)
            }
            _ => val.truthy(),
        }
    }

    /// VM-native dispatch for calling a value (Sub, Routine, Junction, etc.).
    ///
    /// This avoids the interpreter's `eval_call_on_value` for common cases:
    /// - Value::Sub with compiled_code → call_compiled_closure
    /// - Value::Sub without compiled_code → compile on-the-fly, then call_compiled_closure
    /// - Value::Routine → resolve to function name and dispatch
    /// - Value::Junction → thread over values
    /// - Value::WeakSub → upgrade to Sub and recurse
    ///
    /// Falls back to interpreter for Mixin (CALL-ME from roles) and Instance (CALL-ME).
    pub(super) fn vm_call_on_value(
        &mut self,
        target: Value,
        args: Vec<Value>,
        compiled_fns: Option<&HashMap<String, CompiledFunction>>,
    ) -> Result<Value, RuntimeError> {
        // Upgrade WeakSub to Sub transparently
        let target = if let Value::WeakSub(ref weak) = target {
            match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => return Err(RuntimeError::new("Callable has been freed")),
            }
        } else {
            target
        };

        // Fast path: Sub with compiled_code
        if let Value::Sub(ref data) = target
            && let Some(ref cc) = data.compiled_code
        {
            let cc = cc.clone();
            let data = data.clone();
            let empty_fns = HashMap::new();
            let fns = compiled_fns.unwrap_or(&empty_fns);
            return self.call_compiled_closure(&data, &cc, args, fns);
        }

        // Sub without compiled_code: compile on-the-fly then dispatch via VM
        if let Value::Sub(ref data) = target
            && !data.body.is_empty()
        {
            let cc = {
                let mut compiler = crate::compiler::Compiler::new();
                // Use routine closure body so `return` inside the sub works correctly
                compiler.compile_routine_closure_body(&data.params, &data.param_defs, &data.body)
            };
            let data = data.clone();
            let empty_fns = HashMap::new();
            let fns = compiled_fns.unwrap_or(&empty_fns);
            return self.call_compiled_closure(&data, &cc, args, fns);
        }

        // Routine: resolve to function name and dispatch
        if let Value::Routine { package, name, .. } = &target {
            let pkg = package.resolve();
            let name_str = name.resolve();
            if !pkg.is_empty() && pkg != "GLOBAL" {
                let fq = format!("{pkg}::{name_str}");
                if self.interpreter.has_function(&fq) {
                    return self.interpreter.call_function(&fq, args);
                }
            }
            return self.interpreter.call_function(&name_str, args);
        }

        // Junction: thread over values
        if let Value::Junction { kind, values } = target {
            let mut results = Vec::with_capacity(values.len());
            for callable in values.iter() {
                results.push(self.vm_call_on_value(
                    callable.clone(),
                    args.clone(),
                    compiled_fns,
                )?);
            }
            return Ok(Value::junction(kind, results));
        }

        // Mixin wrapping a Sub/Routine: try inner callable first
        if let Value::Mixin(ref inner, ref mixins) = target {
            // Check if any mixed-in role provides CALL-ME
            for key in mixins.keys() {
                if let Some(role_name) = key.strip_prefix("__mutsu_role__")
                    && self.interpreter.role_has_method(role_name, "CALL-ME")
                {
                    // TODO: complex case — fall back to interpreter for CALL-ME on Mixin
                    return self.try_compiled_method_or_interpret(target, "CALL-ME", args);
                }
            }
            // Delegate to inner callable
            return self.vm_call_on_value(inner.as_ref().clone(), args, compiled_fns);
        }

        // Instance or Package (type object): CALL-ME — try compiled method path first
        if matches!(target, Value::Instance { .. } | Value::Package(_)) {
            return self.try_compiled_method_or_interpret(target, "CALL-ME", args);
        }

        // Sub with empty body (no-op closure): call directly via interpreter's
        // call_sub_value, avoiding the eval_call_on_value indirection since we
        // already know the target is a Sub.
        if matches!(target, Value::Sub(_)) {
            return self.interpreter.call_sub_value(target, args, true);
        }

        Ok(Value::Nil)
    }
}

/// Write back attribute values from env after method execution.
///
/// Compares original attribute values with private (`!name`) and public (`.name`)
/// env entries, preferring public when private is unchanged and public differs.
fn writeback_attributes(env: &HashMap<String, Value>, attributes: &mut HashMap<String, Value>) {
    for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
        if attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
            continue;
        }
        let original = attributes.get(&attr_name).cloned().unwrap_or(Value::Nil);
        let env_key = format!("!{}", attr_name);
        let public_env_key = format!(".{}", attr_name);
        let env_array_private_key = format!("@!{}", attr_name);
        let env_array_public_key = format!("@.{}", attr_name);
        let env_hash_private_key = format!("%!{}", attr_name);
        let env_hash_public_key = format!("%.{}", attr_name);
        let env_private = env.get(&env_key).cloned();
        let env_public = env.get(&public_env_key).cloned();
        let env_array_private = env.get(&env_array_private_key).cloned();
        let env_array_public = env.get(&env_array_public_key).cloned();
        let env_hash_private = env.get(&env_hash_private_key).cloned();
        let env_hash_public = env.get(&env_hash_public_key).cloned();
        if let (Some(private_val), Some(public_val)) = (&env_array_private, &env_array_public) {
            if *private_val == original && *public_val != original {
                attributes.insert(attr_name.clone(), public_val.clone());
            } else {
                attributes.insert(attr_name.clone(), private_val.clone());
            }
            continue;
        }
        if let (Some(private_val), Some(public_val)) = (&env_hash_private, &env_hash_public) {
            if *private_val == original && *public_val != original {
                attributes.insert(attr_name.clone(), public_val.clone());
            } else {
                attributes.insert(attr_name.clone(), private_val.clone());
            }
            continue;
        }
        if let (Some(private_val), Some(public_val)) = (&env_private, &env_public) {
            if *private_val == original && *public_val != original {
                attributes.insert(attr_name.clone(), public_val.clone());
            } else {
                attributes.insert(attr_name.clone(), private_val.clone());
            }
            continue;
        }
        if let Some(val) = env_array_private.or(env_hash_private).or(env_private) {
            attributes.insert(attr_name.clone(), val);
            continue;
        }
        if let Some(val) = env_array_public.or(env_hash_public).or(env_public) {
            attributes.insert(attr_name.clone(), val);
        }
        let alias_env_key = format!("__mutsu_sigilless_alias::!{}", attr_name);
        if let Some(Value::Str(alias_name)) = env.get(&alias_env_key) {
            attributes.insert(
                format!("{}{}", ATTR_ALIAS_META_PREFIX, attr_name),
                Value::str(alias_name.to_string()),
            );
        }
    }
}

/// Merge method env back into the saved (caller) env.
///
/// Carries forward values for keys that existed in the saved env, plus any
/// dynamic/global keys (`&`-prefixed, `__mutsu_method_value::`-prefixed).
fn merge_method_env(saved: &Env, current: &Env, method_local_keys: &HashSet<String>) -> Env {
    let mut merged = saved.clone();
    for (k, v) in current.iter() {
        // Skip keys that were introduced by the method frame (params, self,
        // attributes, locals) — these must not leak back into the caller.
        if method_local_keys.contains(k) {
            continue;
        }
        if saved.contains_key(k) {
            merged.insert(k.clone(), v.clone());
        }
        if (k.starts_with('&') && !k.starts_with("&?"))
            || k.starts_with("__mutsu_method_value::")
            || k.starts_with("__mutsu_sigilless_alias::!")
            || k.starts_with("__mutsu_predictive_seq_iter::")
        {
            merged.insert(k.clone(), v.clone());
        }
    }
    merged
}
