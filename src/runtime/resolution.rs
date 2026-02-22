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
        if let Some(cached) = list.cache.lock().unwrap().clone() {
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
        *list.cache.lock().unwrap() = Some(items.clone());
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
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        Value::make_instance("Supply".to_string(), attrs)
    }

    pub(super) fn call_sub_value(
        &mut self,
        func: Value,
        args: Vec<Value>,
        merge_all: bool,
    ) -> Result<Value, RuntimeError> {
        // Upgrade WeakSub to Sub transparently
        let func = match func {
            Value::WeakSub(ref weak) => match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => return Err(RuntimeError::new("Callable has been freed")),
            },
            other => other,
        };
        if let Value::Routine { name, .. } = &func {
            return self.call_function(name, args);
        }
        if let Value::Sub(data) = func {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in &data.env {
                if merge_all {
                    new_env.entry(k.clone()).or_insert(v.clone());
                    continue;
                }
                if matches!(new_env.get(k), Some(Value::Array(_))) && matches!(v, Value::Array(_)) {
                    continue;
                }
                new_env.insert(k.clone(), v.clone());
            }
            for (i, param_name) in data.params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    new_env.insert(param_name.clone(), value.clone());
                }
            }
            // Bind implicit $_ for bare blocks called with arguments
            if data.params.is_empty() && !args.is_empty() {
                new_env.insert("_".to_string(), args[0].clone());
            }
            // &?BLOCK: weak self-reference to break reference cycles
            let block_arc = std::sync::Arc::new(crate::value::SubData {
                package: data.package.clone(),
                name: data.name.clone(),
                params: data.params.clone(),
                body: data.body.clone(),
                env: new_env.clone(),
                id: crate::value::next_instance_id(),
            });
            new_env.insert(
                "&?BLOCK".to_string(),
                Value::WeakSub(std::sync::Arc::downgrade(&block_arc)),
            );
            let block_sub = Value::make_sub(
                data.package.clone(),
                data.name.clone(),
                vec![],
                data.body.clone(),
                new_env.clone(),
            );
            self.env = new_env;
            self.routine_stack
                .push((data.package.clone(), data.name.clone()));
            self.block_stack.push(block_sub);
            let let_mark = self.let_saves.len();
            let result = self.eval_block_value(&data.body);
            self.block_stack.pop();
            self.routine_stack.pop();
            // Manage let saves based on sub result
            match &result {
                Ok(_) => {
                    // Explicit return or successful completion — discard saves
                    self.discard_let_saves(let_mark);
                }
                Err(e) if e.return_value.is_some() => {
                    // Explicit return — discard saves
                    self.discard_let_saves(let_mark);
                }
                Err(_) => {
                    // Exception/fail — restore saves
                    self.restore_let_saves(let_mark);
                }
            }
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
                Err(e) if e.is_fail => Ok(Value::Nil),
                other => other,
            };
        }
        Err(RuntimeError::new("Callable expected"))
    }

    pub(crate) fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        if body.is_empty() {
            return Ok(Value::Nil);
        }
        let compiler = crate::compiler::Compiler::new();
        let (code, compiled_fns) = compiler.compile(body);
        self.run_compiled_block(&code, &compiled_fns)
    }

    /// Run pre-compiled bytecode and return the `$_` topic value.
    fn run_compiled_block(
        &mut self,
        code: &crate::opcode::CompiledCode,
        compiled_fns: &std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (interp, result) = vm.run(code, compiled_fns);
        let value = interp.env().get("_").cloned().unwrap_or(Value::Nil);
        *self = interp;
        result.map(|_| value)
    }

    pub(super) fn eval_map_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Sub(data)) = func {
            let arity = if !data.params.is_empty() {
                data.params.len()
            } else {
                1
            };
            let mut result = Vec::new();

            // Compile once, reuse VM for every iteration
            let compiler = crate::compiler::Compiler::new();
            let (code, compiled_fns) = compiler.compile(&data.body);

            let underscore = "_".to_string();

            // Save original values for keys we'll modify
            let mut touched_keys: Vec<String> =
                Vec::with_capacity(data.env.len() + data.params.len() + 1);
            for k in data.env.keys() {
                touched_keys.push(k.clone());
            }
            for p in &data.params {
                if !touched_keys.contains(p) {
                    touched_keys.push(p.clone());
                }
            }
            if !touched_keys.iter().any(|k| k == "_") {
                touched_keys.push(underscore.clone());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            // Pre-insert closure env
            for (k, v) in &data.env {
                self.env.insert(k.clone(), v.clone());
            }

            // Create VM once, reuse across iterations
            let interp = std::mem::take(self);
            let mut vm = crate::vm::VM::new(interp);

            let mut i = 0usize;
            while i < list_items.len() {
                if arity > 1 && i + arity > list_items.len() {
                    *self = vm.into_interpreter();
                    for (k, orig) in &saved {
                        match orig {
                            Some(v) => {
                                self.env.insert(k.clone(), v.clone());
                            }
                            None => {
                                self.env.remove(k);
                            }
                        }
                    }
                    return Err(RuntimeError::new("Not enough elements for map block arity"));
                }
                {
                    let interp = vm.interpreter_mut();
                    if arity == 1 {
                        let item = list_items[i].clone();
                        if let Some(p) = data.params.first() {
                            interp.env_insert(p.clone(), item.clone());
                        }
                        interp.env_insert(underscore.clone(), item);
                    } else {
                        for (idx, p) in data.params.iter().enumerate() {
                            if i + idx < list_items.len() {
                                interp.env_insert(p.clone(), list_items[i + idx].clone());
                            }
                        }
                        interp.env_insert(underscore.clone(), list_items[i].clone());
                    }
                }
                match vm.run_reuse(&code, &compiled_fns) {
                    Ok(()) => {
                        let val = vm
                            .interpreter()
                            .env()
                            .get("_")
                            .cloned()
                            .unwrap_or(Value::Nil);
                        match val {
                            Value::Slip(elems) => result.extend(elems.iter().cloned()),
                            v => result.push(v),
                        }
                    }
                    Err(e) if e.is_next => {}
                    Err(e) if e.is_last => break,
                    Err(e) => {
                        *self = vm.into_interpreter();
                        for (k, orig) in &saved {
                            match orig {
                                Some(v) => {
                                    self.env.insert(k.clone(), v.clone());
                                }
                                None => {
                                    self.env.remove(k);
                                }
                            }
                        }
                        return Err(e);
                    }
                }
                i += arity;
            }

            *self = vm.into_interpreter();
            // Restore original values
            for (k, orig) in saved {
                match orig {
                    Some(v) => {
                        self.env.insert(k, v);
                    }
                    None => {
                        self.env.remove(&k);
                    }
                }
            }
            return Ok(Value::array(result));
        }
        Ok(Value::array(list_items))
    }

    pub(super) fn eval_grep_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(Value::Sub(data)) = func {
            let mut result = Vec::new();

            // Compile once, reuse VM for every iteration
            let compiler = crate::compiler::Compiler::new();
            let (code, compiled_fns) = compiler.compile(&data.body);

            let underscore = "_".to_string();

            let mut touched_keys: Vec<String> = Vec::with_capacity(data.env.len() + 2);
            for k in data.env.keys() {
                touched_keys.push(k.clone());
            }
            if let Some(p) = data.params.first()
                && !touched_keys.contains(p)
            {
                touched_keys.push(p.clone());
            }
            if !touched_keys.iter().any(|k| k == "_") {
                touched_keys.push(underscore.clone());
            }
            let saved: Vec<(String, Option<Value>)> = touched_keys
                .iter()
                .map(|k| (k.clone(), self.env.get(k).cloned()))
                .collect();

            // Pre-insert closure env
            for (k, v) in &data.env {
                self.env.insert(k.clone(), v.clone());
            }

            // Create VM once, reuse across iterations
            let interp = std::mem::take(self);
            let mut vm = crate::vm::VM::new(interp);

            for item in list_items {
                {
                    let interp = vm.interpreter_mut();
                    if let Some(p) = data.params.first() {
                        interp.env_insert(p.clone(), item.clone());
                    }
                    interp.env_insert(underscore.clone(), item.clone());
                }
                match vm.run_reuse(&code, &compiled_fns) {
                    Ok(()) => {
                        let val = vm
                            .interpreter()
                            .env()
                            .get("_")
                            .cloned()
                            .unwrap_or(Value::Nil);
                        if val.truthy() {
                            result.push(item);
                        }
                    }
                    Err(e) => {
                        *self = vm.into_interpreter();
                        for (k, orig) in &saved {
                            match orig {
                                Some(v) => {
                                    self.env.insert(k.clone(), v.clone());
                                }
                                None => {
                                    self.env.remove(k);
                                }
                            }
                        }
                        return Err(e);
                    }
                }
            }

            *self = vm.into_interpreter();
            for (k, orig) in saved {
                match orig {
                    Some(v) => {
                        self.env.insert(k, v);
                    }
                    None => {
                        self.env.remove(&k);
                    }
                }
            }
            return Ok(Value::array(result));
        }
        if let Some(pattern) = func {
            let mut result = Vec::new();
            for item in list_items {
                if self.smart_match(&item, &pattern) {
                    result.push(item);
                }
            }
            return Ok(Value::array(result));
        }
        Ok(Value::array(list_items))
    }
}
