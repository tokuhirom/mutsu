use super::*;

impl Interpreter {
    pub(super) fn resolve_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        self.functions
            .get(&local)
            .cloned()
            .or_else(|| self.functions.get(&format!("GLOBAL::{}", name)).cloned())
    }

    pub(super) fn insert_token_def(&mut self, name: &str, def: FunctionDef, multi: bool) {
        let key = format!("{}::{}", self.current_package, name);
        if multi {
            self.token_defs.entry(key).or_default().push(def);
        } else {
            self.token_defs.insert(key, vec![def]);
        }
    }

    pub(super) fn resolve_token_defs(&self, name: &str) -> Option<Vec<FunctionDef>> {
        if name.contains("::") {
            return self.token_defs.get(name).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        self.token_defs
            .get(&local)
            .cloned()
            .or_else(|| self.token_defs.get(&format!("GLOBAL::{}", name)).cloned())
    }

    pub(super) fn has_proto_token(&self, name: &str) -> bool {
        if name.contains("::") {
            return self.proto_tokens.contains(name);
        }
        let local = format!("{}::{}", self.current_package, name);
        if self.proto_tokens.contains(&local) {
            return true;
        }
        self.proto_tokens.contains(&format!("GLOBAL::{}", name))
    }

    pub(super) fn resolve_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<MethodDef> {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in overloads {
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some(def);
                    }
                }
            }
        }
        None
    }

    pub(super) fn class_mro(&mut self, class_name: &str) -> Vec<String> {
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return class_def.mro.clone();
        }
        let mut stack = Vec::new();
        match self.compute_class_mro(class_name, &mut stack) {
            Ok(mro) => {
                if let Some(class_def) = self.classes.get_mut(class_name) {
                    class_def.mro = mro.clone();
                }
                mro
            }
            Err(_) => vec![class_name.to_string()],
        }
    }

    pub(super) fn force_lazy_list(&mut self, list: &LazyList) -> Result<Vec<Value>, RuntimeError> {
        if let Some(cached) = list.cache.borrow().clone() {
            return Ok(cached);
        }
        let saved_env = self.env.clone();
        let saved_len = self.gather_items.len();
        self.env = list.env.clone();
        self.gather_items.push(Vec::new());
        let run_res = self.run_block(&list.body);
        let items = self.gather_items.pop().unwrap_or_default();
        while self.gather_items.len() > saved_len {
            self.gather_items.pop();
        }
        let mut merged_env = saved_env;
        for (k, v) in self.env.iter() {
            merged_env.insert(k.clone(), v.clone());
        }
        self.env = merged_env;
        run_res?;
        *list.cache.borrow_mut() = Some(items.clone());
        Ok(items)
    }

    pub(crate) fn force_lazy_list_bridge(
        &mut self,
        list: &crate::value::LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        self.force_lazy_list(list)
    }

    pub(super) fn split_block_phasers(&self, stmts: &[Stmt]) -> (Vec<Stmt>, Vec<Stmt>, Vec<Stmt>) {
        let mut enter_ph = Vec::new();
        let mut leave_ph = Vec::new();
        let mut body_main = Vec::new();
        for stmt in stmts {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo => {
                        leave_ph.push(Stmt::Block(body.clone()))
                    }
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }
        (enter_ph, leave_ph, body_main)
    }

    pub(super) fn make_promise_instance(&self, status: &str, result: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::Str(status.to_string()));
        attrs.insert("result".to_string(), result);
        Value::make_instance("Promise".to_string(), attrs)
    }

    pub(super) fn make_supply_instance(&self) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::Array(Vec::new()));
        attrs.insert("taps".to_string(), Value::Array(Vec::new()));
        Value::make_instance("Supply".to_string(), attrs)
    }

    pub(super) fn call_sub_value(
        &mut self,
        func: Value,
        args: Vec<Value>,
        merge_all: bool,
    ) -> Result<Value, RuntimeError> {
        if let Value::Routine { name, .. } = &func {
            return self.call_function(name, args);
        }
        if let Value::Sub {
            package,
            name,
            params,
            body,
            env,
            ..
        } = func
        {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in env {
                if merge_all {
                    new_env.entry(k).or_insert(v);
                    continue;
                }
                if matches!(new_env.get(&k), Some(Value::Array(_))) && matches!(v, Value::Array(_))
                {
                    continue;
                }
                new_env.insert(k, v);
            }
            for (i, param_name) in params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    new_env.insert(param_name.clone(), value.clone());
                }
            }
            let placeholders = collect_placeholders(&body);
            if !placeholders.is_empty() {
                for (i, ph) in placeholders.iter().enumerate() {
                    if let Some(val) = args.get(i) {
                        new_env.insert(ph.clone(), val.clone());
                    }
                }
            }
            let block_sub = Value::Sub {
                package: package.clone(),
                name: name.clone(),
                params: vec![],
                body: body.clone(),
                env: new_env.clone(),
                id: next_instance_id(),
            };
            self.env = new_env;
            self.routine_stack.push((package.clone(), name.clone()));
            self.block_stack.push(block_sub);
            let result = self.eval_block_value(&body);
            self.block_stack.pop();
            self.routine_stack.pop();
            let mut merged = saved_env;
            if merge_all {
                for (k, v) in self.env.iter() {
                    merged.insert(k.clone(), v.clone());
                }
            } else {
                for (k, v) in self.env.iter() {
                    if matches!(v, Value::Array(_)) {
                        merged.insert(k.clone(), v.clone());
                    }
                }
            }
            self.env = merged;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        Err(RuntimeError::new("Callable expected"))
    }

    pub(super) fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        if body.is_empty() {
            return Ok(Value::Nil);
        }
        let compiler = crate::compiler::Compiler::new();
        let (code, compiled_fns) = compiler.compile(body);
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (interp, result) = vm.run(&code, &compiled_fns);
        let value = interp.env().get("_").cloned().unwrap_or(Value::Nil);
        *self = interp;
        result.map(|_| value)
    }

    pub(super) fn eval_map_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Sub {
            params, body, env, ..
        }) = func
        {
            let placeholders = collect_placeholders(&body);
            let arity = if !params.is_empty() {
                params.len()
            } else if !placeholders.is_empty() {
                placeholders.len()
            } else {
                1
            };
            let mut result = Vec::new();
            let mut i = 0usize;
            while i < list_items.len() {
                if arity > 1 && i + arity > list_items.len() {
                    return Err(RuntimeError::new("Not enough elements for map block arity"));
                }
                let saved = self.env.clone();
                for (k, v) in &env {
                    self.env.insert(k.clone(), v.clone());
                }
                if arity == 1 {
                    let item = list_items[i].clone();
                    if let Some(p) = params.first() {
                        self.env.insert(p.clone(), item.clone());
                    }
                    if let Some(ph) = placeholders.first() {
                        self.env.insert(ph.clone(), item.clone());
                    }
                    self.env.insert("_".to_string(), item);
                } else {
                    for (idx, p) in params.iter().enumerate() {
                        if i + idx < list_items.len() {
                            self.env.insert(p.clone(), list_items[i + idx].clone());
                        }
                    }
                    for (idx, ph) in placeholders.iter().enumerate() {
                        if i + idx < list_items.len() {
                            self.env.insert(ph.clone(), list_items[i + idx].clone());
                        }
                    }
                    self.env.insert("_".to_string(), list_items[i].clone());
                }
                match self.eval_block_value(&body) {
                    Ok(Value::Slip(elems)) => {
                        self.env = saved;
                        result.extend(elems);
                    }
                    Ok(val) => {
                        self.env = saved;
                        result.push(val);
                    }
                    Err(e) if e.is_next => {
                        self.env = saved;
                    }
                    Err(e) if e.is_last => {
                        self.env = saved;
                        break;
                    }
                    Err(e) => {
                        self.env = saved;
                        return Err(e);
                    }
                }
                i += arity;
            }
            return Ok(Value::Array(result));
        }
        Ok(Value::Array(list_items))
    }

    pub(super) fn eval_grep_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Sub {
            params, body, env, ..
        }) = func
        {
            let placeholders = collect_placeholders(&body);
            let mut result = Vec::new();
            for item in list_items {
                let saved = self.env.clone();
                for (k, v) in &env {
                    self.env.insert(k.clone(), v.clone());
                }
                if let Some(p) = params.first() {
                    self.env.insert(p.clone(), item.clone());
                }
                if let Some(ph) = placeholders.first() {
                    self.env.insert(ph.clone(), item.clone());
                }
                self.env.insert("_".to_string(), item.clone());
                let val = self.eval_block_value(&body)?;
                self.env = saved;
                if val.truthy() {
                    result.push(item);
                }
            }
            return Ok(Value::Array(result));
        }
        Ok(Value::Array(list_items))
    }
}
