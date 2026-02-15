use super::*;

impl Interpreter {
    pub(crate) fn env_mut(&mut self) -> &mut HashMap<String, Value> {
        &mut self.env
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
        // Check if stored as a variable first (my &f = ...)
        let var_key = format!("&{}", name);
        if let Some(val) = self.env.get(&var_key) {
            return val.clone();
        }
        // Look up as a function reference (including multi subs)
        let def = self.resolve_function(name).or_else(|| {
            let prefix_local = format!("{}::{}/", self.current_package, name);
            let prefix_global = format!("GLOBAL::{}/", name);
            self.functions
                .iter()
                .find(|(k, _)| k.starts_with(&prefix_local) || k.starts_with(&prefix_global))
                .map(|(_, v)| v.clone())
        });
        if let Some(def) = def {
            Value::Sub {
                package: def.package,
                name: def.name,
                params: def.params,
                body: def.body,
                env: self.env.clone(),
                id: next_instance_id(),
            }
        } else if Self::is_builtin_function(name) {
            Value::Routine {
                package: "GLOBAL".to_string(),
                name: name.to_string(),
            }
        } else {
            Value::Nil
        }
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
}
