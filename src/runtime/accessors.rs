use super::*;

impl Interpreter {
    pub(crate) fn env_mut(&mut self) -> &mut HashMap<String, Value> {
        &mut self.env
    }

    pub(crate) fn get_state_var(&self, key: &str) -> Option<&Value> {
        self.state_vars.get(key)
    }

    pub(crate) fn set_state_var(&mut self, key: String, value: Value) {
        self.state_vars.insert(key, value);
    }

    pub(crate) fn when_matched(&self) -> bool {
        self.when_matched
    }

    pub(crate) fn set_when_matched(&mut self, v: bool) {
        self.when_matched = v;
    }

    pub(crate) fn smart_match_values(&mut self, left: &Value, right: &Value) -> bool {
        self.smart_match(left, right)
    }

    pub(crate) fn eval_sequence_values(
        &mut self,
        left: Value,
        right: Value,
        exclude_end: bool,
    ) -> Result<Value, RuntimeError> {
        self.eval_sequence(left, right, exclude_end)
    }

    pub(crate) fn resolve_code_var(&self, name: &str) -> Value {
        if name.starts_with("infix:<") && name.ends_with('>') {
            return Value::Routine {
                package: "GLOBAL".to_string(),
                name: name.to_string(),
            };
        }
        // Handle package-qualified names: strip pseudo-package prefixes and
        // resolve the bare function name.
        let bare_name = Self::strip_pseudo_packages(name);
        let has_packages = bare_name != name;
        // When SETTING:: (or similar) pseudo-packages are present, resolve to
        // the builtin directly — these refer to the outer setting scope, not
        // user-defined overrides.
        // When pseudo-package qualifiers are present (e.g. SETTING::), resolve
        // to the builtin directly, bypassing user-defined overrides.
        if has_packages && Self::is_builtin_function(bare_name) {
            return Value::Routine {
                package: "GLOBAL".to_string(),
                name: bare_name.to_string(),
            };
        }
        // Check if stored as a variable first (my &f = ...)
        let var_key = format!("&{}", bare_name);
        if let Some(val) = self.env.get(&var_key) {
            return val.clone();
        }
        // Look up as a function reference (including multi subs)
        let def = self.resolve_function(bare_name);
        let is_multi = if def.is_none() {
            // Check if there are multi-dispatch variants (stored with arity/type suffixes)
            let prefix_local = format!("{}::{}/", self.current_package, bare_name);
            let prefix_global = format!("GLOBAL::{}/", bare_name);
            self.functions
                .keys()
                .any(|k| k.starts_with(&prefix_local) || k.starts_with(&prefix_global))
        } else {
            false
        };
        if is_multi {
            // Multi subs should resolve via the dispatcher at call time
            Value::Routine {
                package: self.current_package.clone(),
                name: bare_name.to_string(),
            }
        } else if let Some(def) = def {
            Value::Sub {
                package: def.package,
                name: def.name,
                params: def.params,
                body: def.body,
                env: self.env.clone(),
                id: next_instance_id(),
            }
        } else if Self::is_builtin_function(bare_name) {
            Value::Routine {
                package: "GLOBAL".to_string(),
                name: bare_name.to_string(),
            }
        } else {
            Value::Nil
        }
    }

    /// Strip pseudo-package prefixes (SETTING::, OUTER::, CALLER::, CORE::, etc.)
    /// from a qualified name and return the final bare function name.
    fn strip_pseudo_packages(name: &str) -> &str {
        let pseudo = [
            "SETTING", "CALLER", "OUTER", "CORE", "GLOBAL", "MY", "OUR", "DYNAMIC", "UNIT",
        ];
        let mut rest = name;
        loop {
            let mut found = false;
            for pkg in &pseudo {
                if let Some(after) = rest.strip_prefix(pkg)
                    && let Some(after) = after.strip_prefix("::")
                {
                    rest = after;
                    found = true;
                    break;
                }
            }
            if !found {
                break;
            }
        }
        rest
    }

    pub(crate) fn routine_stack_top(&self) -> Option<&(String, String)> {
        self.routine_stack.last()
    }

    pub(crate) fn push_routine(&mut self, package: String, name: String) {
        self.routine_stack.push((package, name));
    }

    pub(crate) fn pop_routine(&mut self) {
        self.routine_stack.pop();
    }

    pub(crate) fn block_stack_top(&self) -> Option<&Value> {
        self.block_stack.last()
    }

    /// Stringify a value, calling the `.Str` method for Instance and Package types.
    pub(crate) fn stringify_value(&mut self, value: Value) -> Result<String, RuntimeError> {
        match &value {
            Value::Instance { .. } | Value::Package(_) => {
                let result = self.call_method_with_values(value, "Str", vec![])?;
                Ok(result.to_string_value())
            }
            _ => Ok(value.to_string_value()),
        }
    }

    /// Check if a value can respond to a given method name.
    pub(crate) fn value_can_method(&mut self, value: &Value, method: &str) -> bool {
        // Check builtin 0-arg method (covers most built-in methods)
        if crate::builtins::native_method_0arg(value, method).is_some() {
            return true;
        }
        // For instances, check class methods
        if let Value::Instance { class_name, .. } = value {
            return self.class_has_method(class_name, method);
        }
        // Universal methods available on all values
        matches!(
            method,
            "WHAT"
                | "say"
                | "print"
                | "put"
                | "gist"
                | "Str"
                | "Int"
                | "Num"
                | "Bool"
                | "Numeric"
                | "Real"
                | "so"
                | "not"
                | "defined"
                | "isa"
                | "does"
                | "ACCEPTS"
                | "raku"
                | "perl"
                | "clone"
                | "new"
        )
    }

    pub(crate) fn regex_find_first_bridge(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<(usize, usize)> {
        self.regex_find_first(pattern, text)
    }

    pub(crate) fn take_value(&mut self, val: Value) {
        if let Some(items) = self.gather_items.last_mut() {
            items.push(val);
        }
    }

    pub(crate) fn current_package(&self) -> &str {
        &self.current_package
    }

    pub(crate) fn set_current_package(&mut self, pkg: String) {
        self.current_package = pkg;
    }

    pub(crate) fn push_end_phaser(&mut self, body: Vec<Stmt>) {
        let captured_env = self.env.clone();
        self.end_phasers.push((body, captured_env));
    }

    /// Push a saved variable value for `let` scope management.
    pub(crate) fn let_saves_push(&mut self, name: String, value: Value) {
        self.let_saves.push((name, value));
    }

    /// Current length of let_saves stack (used as a mark).
    pub(crate) fn let_saves_len(&self) -> usize {
        self.let_saves.len()
    }

    /// Restore variables from let_saves starting at `mark`, then truncate.
    pub(crate) fn restore_let_saves(&mut self, mark: usize) {
        for i in (mark..self.let_saves.len()).rev() {
            let (name, old_val) = self.let_saves[i].clone();
            self.env.insert(name, old_val);
        }
        self.let_saves.truncate(mark);
    }

    /// Discard let_saves from `mark` without restoring (block succeeded).
    pub(crate) fn discard_let_saves(&mut self, mark: usize) {
        self.let_saves.truncate(mark);
    }
}
