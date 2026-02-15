use super::*;

impl Interpreter {
    pub(super) fn resolve_function_with_alias(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        if let Some(def) = self.resolve_function_with_types(name, arg_values) {
            return Some(def);
        }
        if name.contains(':') || name.contains("::") {
            return None;
        }
        for alias in [format!("prefix:<{name}>"), format!("postfix:<{name}>")] {
            if let Some(def) = self.resolve_function_with_types(&alias, arg_values) {
                return Some(def);
            }
        }
        None
    }

    pub(super) fn resolve_function_with_arity(
        &self,
        name: &str,
        arity: usize,
    ) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        // Try multi-dispatch with arity first
        let multi_local = format!("{}::{}/{}", self.current_package, name, arity);
        if let Some(def) = self.functions.get(&multi_local) {
            return Some(def.clone());
        }
        let multi_global = format!("GLOBAL::{}/{}", name, arity);
        if let Some(def) = self.functions.get(&multi_global) {
            return Some(def.clone());
        }
        // Fall back to regular lookup
        self.resolve_function(name)
    }

    pub(super) fn resolve_function_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        let arity = arg_values.len();
        let type_sig: Vec<&str> = arg_values
            .iter()
            .map(|v| super::value_type_name(v))
            .collect();
        let typed_key = format!(
            "{}::{}/{}:{}",
            self.current_package,
            name,
            arity,
            type_sig.join(",")
        );
        if let Some(def) = self.functions.get(&typed_key) {
            return Some(def.clone());
        }
        let typed_global = format!("GLOBAL::{}/{}:{}", name, arity, type_sig.join(","));
        if let Some(def) = self.functions.get(&typed_global) {
            return Some(def.clone());
        }
        // Try matching against all typed candidates for this name/arity
        let prefix_local = format!("{}::{}/{}:", self.current_package, name, arity);
        let prefix_global = format!("GLOBAL::{}/{}:", name, arity);
        let candidates: Vec<FunctionDef> = self
            .functions
            .iter()
            .filter(|(key, _)| key.starts_with(&prefix_local) || key.starts_with(&prefix_global))
            .map(|(_, def)| def.clone())
            .collect();
        for def in candidates {
            if self.args_match_param_types(arg_values, &def.param_defs) {
                return Some(def);
            }
        }
        // Fall back to arity-only if no proto declared
        if self.has_proto(name) {
            None
        } else {
            self.resolve_function_with_arity(name, arity)
        }
    }

    pub(super) fn eval_token_call_values(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Result<Option<String>, RuntimeError> {
        let defs = match self.resolve_token_defs(name) {
            Some(defs) => defs,
            None => return Ok(None),
        };
        let subject = match self.env.get("_") {
            Some(Value::Str(s)) => Some(s.clone()),
            _ => None,
        };
        let mut best: Option<(usize, String)> = None;
        for def in defs {
            if let Some(pattern) = self.eval_token_def(&def, arg_values)? {
                if let Some(ref text) = subject {
                    if let Some(len) = self.regex_match_len_at_start(&pattern, text) {
                        let better = best
                            .as_ref()
                            .map(|(best_len, _)| len > *best_len)
                            .unwrap_or(true);
                        if better {
                            best = Some((len, pattern));
                        }
                    }
                } else if best.is_none() {
                    best = Some((0, pattern));
                }
            }
        }
        if let Some((_, pattern)) = best {
            return Ok(Some(pattern));
        }
        if self.has_proto_token(name) {
            return Err(RuntimeError::new(format!(
                "No matching candidates for proto token: {}",
                name
            )));
        }
        Ok(None)
    }

    pub(super) fn eval_token_def(
        &mut self,
        def: &FunctionDef,
        arg_values: &[Value],
    ) -> Result<Option<String>, RuntimeError> {
        let saved_env = self.env.clone();
        self.bind_function_args_values(&def.param_defs, &def.params, arg_values)?;
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        let result = self.eval_block_value(&def.body);
        self.routine_stack.pop();
        self.env = saved_env;
        let value = match result {
            Ok(v) => v,
            Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
            Err(e) => return Err(e),
        };
        match value {
            Value::Regex(pat) => Ok(Some(pat)),
            Value::Str(s) => Ok(Some(s)),
            Value::Nil => Ok(None),
            other => Ok(Some(other.to_string_value())),
        }
    }

    pub(super) fn has_proto(&self, name: &str) -> bool {
        if name.contains("::") {
            return self.proto_subs.contains(name);
        }
        let local = format!("{}::{}", self.current_package, name);
        if self.proto_subs.contains(&local) {
            return true;
        }
        self.proto_subs.contains(&format!("GLOBAL::{}", name))
    }
}
