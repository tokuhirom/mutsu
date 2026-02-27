use super::*;

impl Interpreter {
    fn inferred_operator_arity(name: &str) -> Option<usize> {
        if name.starts_with("infix:<") && name.ends_with('>') {
            return Some(2);
        }
        if (name.starts_with("prefix:<") || name.starts_with("postfix:<")) && name.ends_with('>') {
            return Some(1);
        }
        None
    }

    fn callable_return_type_inner(callable: &Value) -> Option<String> {
        match callable {
            Value::Sub(data) => match data.env.get("__mutsu_return_type") {
                Some(Value::Str(rt)) => Some(rt.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    pub(crate) fn callable_return_type(&self, callable: &Value) -> Option<String> {
        Self::callable_return_type_inner(callable)
    }

    pub(crate) fn callable_signature(&self, callable: &Value) -> (Vec<String>, Vec<ParamDef>) {
        match callable {
            Value::Sub(data) => (data.params.clone(), data.param_defs.clone()),
            Value::Routine { name, .. } => {
                if let Some(def) = self.resolve_function(name) {
                    return (def.params, def.param_defs);
                }
                if let Some(arity) = Self::inferred_operator_arity(name) {
                    let params = (0..arity).map(|i| format!("arg{}", i)).collect();
                    return (params, Vec::new());
                }
                (vec!["arg0".to_string()], Vec::new())
            }
            _ => (vec!["arg0".to_string()], Vec::new()),
        }
    }

    pub(crate) fn compose_callables(&self, left: Value, right: Value) -> Value {
        use std::sync::atomic::{AtomicU64, Ordering};

        static COMPOSE_ID: AtomicU64 = AtomicU64::new(1_000_000);
        let id = COMPOSE_ID.fetch_add(1, Ordering::Relaxed);

        let (mut params, param_defs) = self.callable_signature(&right);
        if params.is_empty() {
            if !param_defs.is_empty() {
                params = param_defs.iter().map(|pd| pd.name.clone()).collect();
            } else {
                params = vec!["arg0".to_string()];
            }
        }

        let left_return_type = self.callable_return_type(&left);
        let mut env = std::collections::HashMap::new();
        env.insert("__mutsu_compose_left".to_string(), left);
        env.insert("__mutsu_compose_right".to_string(), right);
        if let Some(rt) = left_return_type {
            env.insert("__mutsu_return_type".to_string(), Value::Str(rt));
        }

        Value::make_sub_with_id(
            String::new(),
            "<composed>".to_string(),
            params,
            param_defs,
            Vec::new(),
            false,
            env,
            id,
        )
    }

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

    pub(crate) fn is_role(&self, name: &str) -> bool {
        self.roles.contains_key(name)
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
        if (name.starts_with("infix:<")
            || name.starts_with("prefix:<")
            || name.starts_with("postfix:<"))
            && name.ends_with('>')
        {
            return Value::Routine {
                package: "GLOBAL".to_string(),
                name: name.to_string(),
                is_regex: false,
            };
        }
        // Handle package-qualified names: strip pseudo-package prefixes and
        // resolve the bare function name.
        let bare_name = Self::strip_pseudo_packages(name);
        let has_packages = bare_name != name;
        let lookup_name = bare_name.strip_prefix('*').unwrap_or(bare_name);
        if bare_name == "?ROUTINE" {
            if let Some((package, routine)) = self.routine_stack.last() {
                return Value::Routine {
                    package: package.clone(),
                    name: routine.clone(),
                    is_regex: false,
                };
            }
            return Value::Nil;
        }
        // When SETTING:: (or similar) pseudo-packages are present, resolve to
        // the builtin directly — these refer to the outer setting scope, not
        // user-defined overrides.
        // When pseudo-package qualifiers are present (e.g. SETTING::), resolve
        // to the builtin directly, bypassing user-defined overrides.
        if has_packages && Self::is_builtin_function(lookup_name) {
            return Value::Routine {
                package: "GLOBAL".to_string(),
                name: lookup_name.to_string(),
                is_regex: false,
            };
        }
        // Check if stored as a variable first (my &f = ...)
        let var_key = format!("&{}", bare_name);
        if let Some(val) = self.env.get(&var_key) {
            // Upgrade WeakSub references (e.g., &?BLOCK) to strong Sub
            if let Value::WeakSub(weak) = val {
                return match weak.upgrade() {
                    Some(strong) => Value::Sub(strong),
                    None => Value::Nil,
                };
            }
            return val.clone();
        }
        // Look up as a function reference (including multi subs)
        let def = self.resolve_function(lookup_name);
        let is_multi = if def.is_none() {
            // Check if there are multi-dispatch variants (stored with arity/type suffixes)
            let prefix_local = format!("{}::{}/", self.current_package, lookup_name);
            let prefix_global = format!("GLOBAL::{}/", lookup_name);
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
                name: lookup_name.to_string(),
                is_regex: false,
            }
        } else if self.has_proto(lookup_name)
            || self.resolve_token_defs(lookup_name).is_some()
            || self.has_proto_token(lookup_name)
        {
            Value::Routine {
                package: self.current_package.clone(),
                name: lookup_name.to_string(),
                is_regex: self.resolve_token_defs(lookup_name).is_some()
                    || self.has_proto_token(lookup_name),
            }
        } else if let Some(def) = def {
            Value::make_sub(
                def.package,
                def.name,
                def.params,
                def.param_defs,
                def.body,
                def.is_rw,
                self.env.clone(),
            )
        } else if Self::is_builtin_function(lookup_name) {
            Value::Routine {
                package: "GLOBAL".to_string(),
                name: lookup_name.to_string(),
                is_regex: false,
            }
        } else if bare_name.starts_with('*') {
            // Dynamic code vars (&*foo) can point to routines that are resolved
            // at call time (including builtins not listed in is_builtin_function).
            Value::Routine {
                package: "GLOBAL".to_string(),
                name: lookup_name.to_string(),
                is_regex: false,
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

    pub(crate) fn package_stash_value(&self, package: &str) -> Value {
        let mut symbols: HashMap<String, Value> = HashMap::new();
        let prefix = format!("{package}::");

        for (key, val) in &self.env {
            if let Some(rest) = key.strip_prefix(&prefix) {
                let stash_key = if rest.starts_with('$')
                    || rest.starts_with('@')
                    || rest.starts_with('%')
                    || rest.starts_with('&')
                {
                    rest.to_string()
                } else {
                    format!("${rest}")
                };
                symbols.insert(stash_key, val.clone());
            }
        }

        for (key, def) in &self.functions {
            if let Some(rest) = key.strip_prefix(&prefix) {
                if rest.contains('/') || rest.contains(':') {
                    continue;
                }
                symbols
                    .entry(format!("&{rest}"))
                    .or_insert_with(|| Value::Routine {
                        package: def.package.clone(),
                        name: def.name.clone(),
                        is_regex: false,
                    });
            }
        }

        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::Str(package.to_string()));
        attrs.insert("symbols".to_string(), Value::hash(symbols));
        Value::make_instance("Stash".to_string(), attrs)
    }

    pub(crate) fn push_end_phaser(&mut self, body: Vec<Stmt>) {
        let captured_env = self.env.clone();
        self.end_phasers.push((body, captured_env));
    }

    pub(crate) fn snapshot_routine_registry(&self) -> RoutineRegistrySnapshot {
        (
            self.functions.clone(),
            self.proto_functions.clone(),
            self.token_defs.clone(),
            self.proto_subs.clone(),
            self.proto_tokens.clone(),
        )
    }

    pub(crate) fn restore_routine_registry(&mut self, snapshot: RoutineRegistrySnapshot) {
        let (functions, proto_functions, token_defs, proto_subs, proto_tokens) = snapshot;
        self.functions = functions;
        self.proto_functions = proto_functions;
        self.token_defs = token_defs;
        self.proto_subs = proto_subs;
        self.proto_tokens = proto_tokens;
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
