use super::*;

impl Interpreter {
    pub(super) fn compute_class_mro(
        &mut self,
        class_name: &str,
        stack: &mut Vec<String>,
    ) -> Result<Vec<String>, RuntimeError> {
        if stack.iter().any(|name| name == class_name) {
            return Err(RuntimeError::new(format!(
                "C3 MRO cycle detected at {}",
                class_name
            )));
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return Ok(class_def.mro.clone());
        }
        stack.push(class_name.to_string());
        let parents = self
            .classes
            .get(class_name)
            .map(|c| c.parents.clone())
            .unwrap_or_default();
        let mut seqs: Vec<Vec<String>> = Vec::new();
        for parent in &parents {
            if self.classes.contains_key(parent) {
                let mro = self.compute_class_mro(parent, stack)?;
                seqs.push(mro);
            } else {
                seqs.push(vec![parent.clone()]);
            }
        }
        seqs.push(parents.clone());
        let mut result = vec![class_name.to_string()];
        while seqs.iter().any(|s| !s.is_empty()) {
            let mut candidate = None;
            for seq in &seqs {
                if seq.is_empty() {
                    continue;
                }
                let head = &seq[0];
                let mut in_tail = false;
                for other in &seqs {
                    if other.len() > 1 && other[1..].contains(head) {
                        in_tail = true;
                        break;
                    }
                }
                if !in_tail {
                    candidate = Some(head.clone());
                    break;
                }
            }
            if let Some(head) = candidate {
                result.push(head.clone());
                for seq in seqs.iter_mut() {
                    if !seq.is_empty() && seq[0] == head {
                        seq.remove(0);
                    }
                }
            } else {
                stack.pop();
                return Err(RuntimeError::new(format!(
                    "Inconsistent class hierarchy for {}",
                    class_name
                )));
            }
        }
        stack.pop();
        Ok(result)
    }

    pub(super) fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && (class_def.methods.contains_key(method_name)
                    || class_def.native_methods.contains(method_name))
            {
                return true;
            }
        }
        false
    }

    pub(super) fn is_native_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && class_def.native_methods.contains(method_name)
            {
                return true;
            }
        }
        false
    }

    pub(super) fn has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && class_def.methods.contains_key(method_name)
            {
                return true;
            }
        }
        false
    }

    pub(super) fn collect_class_attributes(
        &mut self,
        class_name: &str,
    ) -> Vec<(String, bool, Option<Expr>)> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<(String, bool, Option<Expr>)> = Vec::new();
        for cn in mro.iter().rev() {
            if let Some(class_def) = self.classes.get(cn) {
                for attr in &class_def.attributes {
                    if let Some(pos) = attrs.iter().position(|(n, _, _)| n == &attr.0) {
                        attrs.remove(pos);
                    }
                    attrs.push(attr.clone());
                }
            }
        }
        attrs
    }

    pub(super) fn run_instance_method(
        &mut self,
        class_name: &str,
        mut attributes: HashMap<String, Value>,
        method_name: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        let method_def = self
            .resolve_method(class_name, method_name, &args)
            .ok_or_else(|| {
                RuntimeError::new(format!(
                    "No matching candidates for method: {}",
                    method_name
                ))
            })?;
        let base = Value::make_instance(class_name.to_string(), attributes.clone());
        let saved_env = self.env.clone();
        self.env.insert("self".to_string(), base.clone());
        for (attr_name, attr_val) in &attributes {
            self.env.insert(format!("!{}", attr_name), attr_val.clone());
            self.env.insert(format!(".{}", attr_name), attr_val.clone());
        }
        for (i, param) in method_def.params.iter().enumerate() {
            if let Some(val) = args.get(i) {
                self.env.insert(param.clone(), val.clone());
            } else if let Some(pd) = method_def.param_defs.get(i)
                && let Some(default_expr) = &pd.default
            {
                let val = self.eval_block_value(&[Stmt::Expr(default_expr.clone())])?;
                self.env.insert(param.clone(), val);
            }
        }
        let block_result = self.run_block(&method_def.body);
        let implicit_return = self.env.get("_").cloned();
        let result = match block_result {
            Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
        };
        for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
            let env_key = format!("!{}", attr_name);
            if let Some(val) = self.env.get(&env_key) {
                attributes.insert(attr_name, val.clone());
            }
        }
        self.env = saved_env;
        result.map(|v| (v, attributes))
    }
}
