use super::*;

impl VM {
    pub(super) fn exec_get_bare_word_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let val = if name == "Bool::True" {
            Value::Bool(true)
        } else if name == "Bool::False" {
            Value::Bool(false)
        } else if name == "Order::Less" {
            Value::Enum {
                enum_type: Symbol::intern("Order"),
                key: Symbol::intern("Less"),
                value: EnumValue::Int(-1),
                index: 0,
            }
        } else if name == "Order::Same" {
            Value::Enum {
                enum_type: Symbol::intern("Order"),
                key: Symbol::intern("Same"),
                value: EnumValue::Int(0),
                index: 1,
            }
        } else if name == "Order::More" {
            Value::Enum {
                enum_type: Symbol::intern("Order"),
                key: Symbol::intern("More"),
                value: EnumValue::Int(1),
                index: 2,
            }
        } else if (name.starts_with("infix:<")
            || name.starts_with("prefix:<")
            || name.starts_with("postfix:<"))
            && name.ends_with('>')
        {
            Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(name),
                is_regex: false,
            }
        } else if self.interpreter.is_name_suppressed(name) {
            // If we are inside the parent class of the suppressed nested class,
            // resolve the short name to the qualified name (e.g. Frog -> Forest::Frog).
            if let Some(qualified) = self.interpreter.resolve_suppressed_type(name) {
                Value::Package(Symbol::intern(&qualified))
            } else {
                return Err(RuntimeError::new(format!(
                    "X::Undeclared::Symbols: Undeclared name:\n    {} used at line 1",
                    name,
                )));
            }
        } else if let Some((pkg, sym)) = name.rsplit_once("::")
            && let Some(stripped_sym) = sym.strip_prefix('&')
        {
            let qualified_name = format!("{pkg}::{stripped_sym}");
            if self.interpreter.has_function(&qualified_name)
                || self.interpreter.has_multi_function(&qualified_name)
            {
                Value::Routine {
                    package: Symbol::intern(pkg),
                    name: Symbol::intern(&qualified_name),
                    is_regex: false,
                }
            } else {
                Value::str(name.to_string())
            }
        } else if let Some(v) = self.interpreter.env().get(name) {
            if matches!(v, Value::Enum { .. } | Value::Nil)
                || matches!(v, Value::Package(pkg) if pkg.resolve() != name)
            {
                v.clone()
            } else if self.interpreter.has_type(name) || Self::is_builtin_type(name) {
                Value::Package(Symbol::intern(Self::resolve_type_alias(name)))
            } else if name.contains("::")
                && !name.starts_with('$')
                && !name.starts_with('@')
                && !name.starts_with('%')
                && matches!(v, Value::Routine { .. } | Value::Sub(_) | Value::WeakSub(_))
            {
                // Pseudo-package names (SETTING::, OUTER::, CALLER::) need interpreter's
                // special resolution logic, so use call_function directly for those.
                // For regular qualified names, try compiled dispatch first.
                let result = if self.is_interpreter_handled_function(name) {
                    self.interpreter.call_function(name, Vec::new())?
                } else {
                    self.call_function_compiled_first(name, Vec::new(), compiled_fns)?
                };
                self.env_dirty = true;
                result
            } else if !name.starts_with('$') && !name.starts_with('@') && !name.starts_with('%') {
                v.clone()
            } else {
                Value::str(name.to_string())
            }
        } else if name.contains("::")
            && let Some(def) = self.interpreter.resolve_function_with_types(name, &[])
        {
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = def.package.resolve();
                let result =
                    self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?;
                self.env_dirty = true;
                result
            } else {
                let result = self.compile_and_call_function_def(&def, Vec::new(), compiled_fns)?;
                self.env_dirty = true;
                result
            }
        } else if self.interpreter.has_type(name)
            || Self::is_builtin_type(name)
            || Self::is_type_with_smiley(name, &self.interpreter)
        {
            Value::Package(Symbol::intern(Self::resolve_type_alias(name)))
        } else if let Some(callable) = self.interpreter.env().get(&format!("&{name}")).cloned()
            && matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            )
        {
            let result = self.vm_call_on_value(callable, Vec::new(), Some(compiled_fns))?;
            self.env_dirty = true;
            result
        } else if let Some(sub_id) = self.interpreter.wrap_sub_id_for_name(name)
            && !self.interpreter.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.interpreter.get_wrapped_sub(name)
        {
            let result = self.vm_call_on_value(sub_val, Vec::new(), Some(compiled_fns))?;
            self.env_dirty = true;
            result
        } else if Interpreter::is_test_function_name(name)
            && self.interpreter.test_mode_active()
            // Only try test function dispatch for hyphenated names (e.g.
            // `make-temp-dir`, `done-testing`). Single-word names like
            // `run`, `is`, `ok` are either builtins or need args, so they
            // should go through the normal function resolution path.
            && name.contains('-')
        {
            let result = self.interpreter.call_function(name, Vec::new())?;
            self.env_dirty = true;
            result
        } else if self.interpreter.has_function(name)
            || Interpreter::is_implicit_zero_arg_builtin(name)
            || self.interpreter.has_multi_function(name)
        {
            // User-defined functions (with non-empty body in the function registry)
            // shadow native builtins. Check if there's a user definition first.
            let has_user_def = self
                .interpreter
                .resolve_function_with_types(name, &[])
                .is_some_and(|def| !def.body.is_empty());
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = self.interpreter.current_package().to_string();
                let result =
                    self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?;
                self.env_dirty = true;
                result
            } else if has_user_def {
                // User-defined function takes priority over native builtins
                let result = self.interpreter.call_function_fallback(name, &[])?;
                self.env_dirty = true;
                result
            } else if let Some(native_result) =
                self.try_native_function(crate::symbol::Symbol::intern(name), &[])
            {
                native_result?
            } else {
                let result = self.interpreter.call_function(name, Vec::new())?;
                self.env_dirty = true;
                result
            }
        } else if name == "callsame"
            || name == "nextsame"
            || name == "callwith"
            || name == "nextwith"
            || name == "nextcallee"
        {
            let result = self.interpreter.call_function(name, Vec::new())?;
            self.env_dirty = true;
            result
        } else if name == "i" {
            Value::Complex(0.0, 1.0)
        } else if name == "NaN" {
            Value::Num(f64::NAN)
        } else if name == "Inf" {
            Value::Num(f64::INFINITY)
        } else if name == "Empty" {
            Value::Slip(std::sync::Arc::new(vec![]))
        } else if name.starts_with("Metamodel::") {
            // Meta-object protocol type objects
            Value::Package(Symbol::intern(name))
        } else if name.contains("::") {
            // Check if this is an access to a non-existent enum variant
            if let Some((pkg, sym)) = name.rsplit_once("::")
                && self.interpreter.has_enum_type(pkg)
                && !self.interpreter.has_enum_variant(pkg, sym)
            {
                return Err(RuntimeError::new(format!(
                    "Could not find symbol '&{}' in '{}'",
                    sym, pkg,
                )));
            }
            // Strip pseudo-package prefixes (OUR::, GLOBAL::, MY::, etc.)
            // and resolve to the bare type name if it exists.
            let bare = Interpreter::strip_pseudo_packages(name);
            if bare != name
                && (self.interpreter.has_type(bare)
                    || Self::is_builtin_type(bare)
                    || Self::is_type_with_smiley(bare, &self.interpreter))
            {
                Value::Package(Symbol::intern(Self::resolve_type_alias(bare)))
            } else {
                Value::Package(Symbol::intern(name))
            }
        } else if name.chars().count() == 1 {
            // Single unicode character — check for vulgar fractions etc.
            let ch = name.chars().next().unwrap();
            if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
                Value::Rat(n, d)
            } else if let Some(val) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
                Value::Int(val)
            } else {
                Value::str(name.to_string())
            }
        } else if name == "self" {
            // `self` used outside of a method context
            let mut err = RuntimeError::new("'self' used where no object is available".to_string());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str("'self' used where no object is available".to_string()),
            );
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Syntax::Self::WithoutObject"),
                attrs,
            )));
            return Err(err);
        } else {
            Value::str(name.to_string())
        };
        self.stack.push(val);
        Ok(())
    }
}
