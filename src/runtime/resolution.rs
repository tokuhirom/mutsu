use super::*;

impl Interpreter {
    fn is_stub_method_body(body: &[Stmt]) -> bool {
        body.len() == 1
            && matches!(
                &body[0],
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn"
            )
    }

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
            let mut defs = Vec::new();
            if let Some(exact) = self.token_defs.get(name) {
                defs.extend(exact.clone());
            }
            let sym_prefix = format!("{name}:sym<");
            let mut sym_keys: Vec<&String> = self
                .token_defs
                .keys()
                .filter(|key| key.starts_with(&sym_prefix))
                .collect();
            sym_keys.sort();
            for key in sym_keys {
                if let Some(sym_defs) = self.token_defs.get(key) {
                    defs.extend(sym_defs.clone());
                }
            }
            return if defs.is_empty() { None } else { Some(defs) };
        }
        let mut defs = Vec::new();
        for scope in [&self.current_package, "GLOBAL"] {
            let exact_key = format!("{scope}::{name}");
            if let Some(exact) = self.token_defs.get(&exact_key) {
                defs.extend(exact.clone());
            }
            let sym_prefix = format!("{scope}::{name}:sym<");
            let mut sym_keys: Vec<&String> = self
                .token_defs
                .keys()
                .filter(|key| key.starts_with(&sym_prefix))
                .collect();
            sym_keys.sort();
            for key in sym_keys {
                if let Some(sym_defs) = self.token_defs.get(key) {
                    defs.extend(sym_defs.clone());
                }
            }
        }
        if defs.is_empty() { None } else { Some(defs) }
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
        self.resolve_method_with_owner(class_name, method_name, arg_values)
            .map(|(_, def)| def)
    }

    pub(super) fn resolve_method_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some((cn.clone(), def));
                    }
                }
                // Method name is present on this class, but no candidate matched.
                // Do not continue to parent classes; this preserves dispatch
                // consistency for hidden/interface cases.
                return None;
            }
        }
        None
    }

    pub(super) fn resolve_all_methods_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Vec<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        let mut matches = Vec::new();
        for cn in mro {
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        matches.push((cn.clone(), def));
                    }
                }
            }
        }
        matches
    }

    pub(super) fn should_skip_defer_method_candidate(
        &self,
        receiver_class: &str,
        candidate_owner: &str,
    ) -> bool {
        if receiver_class != candidate_owner && self.hidden_classes.contains(candidate_owner) {
            return true;
        }
        self.hidden_defer_parents
            .get(receiver_class)
            .is_some_and(|hidden| hidden.contains(candidate_owner))
    }

    pub(super) fn resolve_private_method_with_owner(
        &mut self,
        class_name: &str,
        owner_class: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if cn != owner_class {
                continue;
            }
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in overloads {
                    if !def.is_private {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some((cn.clone(), def));
                    }
                }
            }
        }
        None
    }

    pub(super) fn resolve_private_method_any_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in &overloads {
                    if !def.is_private {
                        continue;
                    }
                    if Self::is_stub_method_body(&def.body) {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some((cn.clone(), def.clone()));
                    }
                }
                for def in overloads {
                    if !def.is_private {
                        continue;
                    }
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some((cn.clone(), def));
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
            let mut call_args = args.clone();
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
                // Fill bare `*` primers from incoming positional args in order.
                // Remaining positional args are appended after all fixed primers.
                let mut incoming_idx = 0usize;
                for assumed in &data.assumed_positional {
                    let is_placeholder = matches!(assumed, Value::Num(f) if f.is_infinite())
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
                call_args = positional;
                for (key, value) in named {
                    call_args.push(Value::Pair(key, Box::new(value)));
                }
            }
            // Routine wrapper from .assuming() on a multi-dispatch sub
            if let Some(Value::Str(routine_name)) = data.env.get("__mutsu_routine_name").cloned() {
                return self.call_function(&routine_name, call_args);
            }
            if let (Some(left), Some(right)) = (
                data.env.get("__mutsu_compose_left").cloned(),
                data.env.get("__mutsu_compose_right").cloned(),
            ) {
                let right_result = self.call_sub_value(right, call_args, false)?;
                let (left_params, left_param_defs) = self.callable_signature(&left);
                let left_variadic = left_param_defs.iter().any(|pd| pd.slurpy && !pd.named);
                let left_expects_single = match &left {
                    Value::Sub(left_data) if left_params.is_empty() => {
                        let (uses_positional, _) = Self::auto_signature_uses(&left_data.body);
                        !uses_positional
                    }
                    _ => !left_variadic && left_params.len() == 1,
                };
                let left_args = Self::composed_result_to_args(right_result, left_expects_single);
                return self.call_sub_value(left, left_args, false);
            }
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in &data.env {
                if merge_all {
                    new_env.entry(k.clone()).or_insert(v.clone());
                    continue;
                }
                if matches!(new_env.get(k), Some(Value::Array(..))) && matches!(v, Value::Array(..))
                {
                    continue;
                }
                new_env.insert(k.clone(), v.clone());
            }
            self.env = new_env.clone();
            let rw_bindings =
                self.bind_function_args_values(&data.param_defs, &data.params, &call_args)?;
            new_env = self.env.clone();
            if data.params.is_empty() {
                for arg in &args {
                    if let Value::Pair(name, value) = arg {
                        new_env.insert(format!(":{}", name), *value.clone());
                    }
                }
            }
            // Bind implicit $_ for bare blocks called with arguments
            if data.params.is_empty()
                && !args.is_empty()
                && let Some(first_positional) =
                    args.iter().find(|v| !matches!(v, Value::Pair(_, _)))
            {
                new_env.insert("_".to_string(), first_positional.clone());
            }
            // &?BLOCK: weak self-reference to break reference cycles
            let block_arc = std::sync::Arc::new(crate::value::SubData {
                package: data.package.clone(),
                name: data.name.clone(),
                params: data.params.clone(),
                param_defs: data.param_defs.clone(),
                body: data.body.clone(),
                env: new_env.clone(),
                assumed_positional: data.assumed_positional.clone(),
                assumed_named: data.assumed_named.clone(),
                id: crate::value::next_instance_id(),
                empty_sig: data.empty_sig,
            });
            new_env.insert(
                "&?BLOCK".to_string(),
                Value::WeakSub(std::sync::Arc::downgrade(&block_arc)),
            );
            let block_sub = Value::make_sub(
                data.package.clone(),
                data.name.clone(),
                vec![],
                Vec::new(),
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
                    if matches!(v, Value::Array(..)) {
                        merged.insert(k.clone(), v.clone());
                    }
                }
            }
            self.merge_sigilless_alias_writes(&mut merged, &self.env);
            // Apply rw bindings after merge so they take precedence
            self.apply_rw_bindings_to_env(&rw_bindings, &mut merged);
            self.env = merged;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                Err(e) if e.is_fail => Ok(Value::Nil),
                other => other,
            };
        }
        Err(RuntimeError::new("Callable expected"))
    }

    fn composed_result_to_args(value: Value, prefer_single: bool) -> Vec<Value> {
        if prefer_single {
            return match value {
                Value::Seq(items) | Value::Slip(items) => vec![Value::array(items.to_vec())],
                other => vec![other],
            };
        }
        match value {
            Value::Array(items, _) => items.to_vec(),
            Value::Seq(items) => items.to_vec(),
            Value::Slip(items) => items.to_vec(),
            Value::Capture { positional, named } => {
                let mut args = positional;
                for (k, v) in named {
                    args.push(Value::Pair(k, Box::new(v)));
                }
                args
            }
            other => vec![other],
        }
    }

    pub(crate) fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        if body.is_empty() {
            return Ok(Value::Nil);
        }
        let let_mark = self.let_saves_len();
        let mut compiler = crate::compiler::Compiler::new();
        let scope = if let Some((pkg, routine)) = self.routine_stack.last() {
            format!("{}::&{}", pkg, routine)
        } else {
            self.current_package.clone()
        };
        compiler.set_current_package(scope);
        let (code, compiled_fns) = compiler.compile(body);
        let result = self.run_compiled_block(&code, &compiled_fns);
        // Blocks are scope boundaries for temp/let saves.
        self.restore_let_saves(let_mark);
        result
    }

    /// Run pre-compiled bytecode and return the `$_` topic value.
    fn run_compiled_block(
        &mut self,
        code: &crate::opcode::CompiledCode,
        compiled_fns: &std::collections::HashMap<String, crate::opcode::CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (mut interp, result) = vm.run(code, compiled_fns);
        // Persist state variables for top-level compiled blocks too.
        for (slot, key) in &code.state_locals {
            if let Some(name) = code.locals.get(*slot)
                && let Some(val) = interp.env().get(name).cloned()
            {
                interp.set_state_var(key.clone(), val);
            }
        }
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
                // Account for assumed positional args (from .assuming)
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            if data.env.contains_key("__mutsu_compose_left")
                && data.env.contains_key("__mutsu_compose_right")
            {
                let mut result = Vec::new();
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        return Err(RuntimeError::new("Not enough elements for map block arity"));
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let value = self.call_sub_value(Value::Sub(data.clone()), chunk, false)?;
                    match value {
                        Value::Slip(elems) => result.extend(elems.iter().cloned()),
                        v => result.push(v),
                    }
                    i += arity;
                }
                return Ok(Value::array(result));
            }
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
                    let assumed_count = data.assumed_positional.len();
                    // Bind assumed positional args first
                    for (idx, val) in data.assumed_positional.iter().enumerate() {
                        if let Some(p) = data.params.get(idx) {
                            interp.env_insert(p.clone(), val.clone());
                        }
                    }
                    if arity == 1 {
                        let item = list_items[i].clone();
                        if let Some(p) = data.params.get(assumed_count) {
                            interp.env_insert(p.clone(), item.clone());
                        }
                        interp.env_insert(underscore.clone(), item);
                    } else {
                        for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
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
            let arity = if !data.params.is_empty() {
                let effective = data
                    .params
                    .len()
                    .saturating_sub(data.assumed_positional.len());
                if effective == 0 { 1 } else { effective }
            } else {
                1
            };
            if data.env.contains_key("__mutsu_compose_left")
                && data.env.contains_key("__mutsu_compose_right")
            {
                let mut i = 0usize;
                while i < list_items.len() {
                    if arity > 1 && i + arity > list_items.len() {
                        break;
                    }
                    let chunk: Vec<Value> = if arity == 1 {
                        vec![list_items[i].clone()]
                    } else {
                        list_items[i..i + arity].to_vec()
                    };
                    let pred =
                        self.call_sub_value(Value::Sub(data.clone()), chunk.clone(), false)?;
                    if pred.truthy() {
                        if arity == 1 {
                            result.push(chunk[0].clone());
                        } else {
                            result.push(Value::array(chunk));
                        }
                    }
                    i += arity;
                }
                return Ok(Value::array(result));
            }

            // Compile once, reuse VM for every iteration
            let compiler = crate::compiler::Compiler::new();
            let (code, compiled_fns) = compiler.compile(&data.body);

            let underscore = "_".to_string();

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
                    break;
                }
                let chunk: Vec<Value> = if arity == 1 {
                    vec![list_items[i].clone()]
                } else {
                    list_items[i..i + arity].to_vec()
                };
                {
                    let interp = vm.interpreter_mut();
                    let assumed_count = data.assumed_positional.len();
                    for (idx, val) in data.assumed_positional.iter().enumerate() {
                        if let Some(p) = data.params.get(idx) {
                            interp.env_insert(p.clone(), val.clone());
                        }
                    }
                    if arity == 1 {
                        if let Some(p) = data.params.get(assumed_count) {
                            interp.env_insert(p.clone(), chunk[0].clone());
                        }
                        interp.env_insert(underscore.clone(), chunk[0].clone());
                    } else {
                        for (idx, p) in data.params.iter().skip(assumed_count).enumerate() {
                            if idx < chunk.len() {
                                interp.env_insert(p.clone(), chunk[idx].clone());
                            }
                        }
                        interp.env_insert(underscore.clone(), chunk[0].clone());
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
                        if val.truthy() {
                            if arity == 1 {
                                result.push(chunk[0].clone());
                            } else {
                                result.push(Value::array(chunk));
                            }
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
                i += arity;
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

    /// Like `eval_grep_over_items` but returns only the first matching item (or Nil).
    pub(super) fn eval_first_over_items(
        &mut self,
        func: Option<Value>,
        list_items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(matcher) = func {
            if matches!(&matcher, Value::Sub(_)) {
                // Use call_sub_value directly for proper closure variable propagation
                for item in list_items {
                    let result = self.call_sub_value(matcher.clone(), vec![item.clone()], true)?;
                    if result.truthy() {
                        return Ok(item);
                    }
                }
                return Ok(Value::Nil);
            }
            for item in list_items {
                if self.smart_match(&item, &matcher) {
                    return Ok(item);
                }
            }
            return Ok(Value::Nil);
        }
        Ok(list_items.into_iter().next().unwrap_or(Value::Nil))
    }
}
