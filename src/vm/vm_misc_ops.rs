use super::*;

impl VM {
    pub(super) fn exec_make_range_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::Range(*a, *b),
            (Value::Int(a), Value::Num(b)) if b.is_infinite() && b.is_sign_positive() => {
                Value::Range(*a, i64::MAX)
            }
            (Value::Num(a), Value::Int(b)) if a.is_infinite() && a.is_sign_negative() => {
                Value::Range(i64::MIN, *b)
            }
            (Value::Str(a), Value::Str(b)) => Value::GenericRange {
                start: Box::new(Value::Str(a.clone())),
                end: Box::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            (l, r) if l.is_numeric() && r.is_numeric() => Value::GenericRange {
                start: Box::new(l.clone()),
                end: Box::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (Value::Str(a), r) if r.is_numeric() => Value::GenericRange {
                start: Box::new(Value::Str(a.clone())),
                end: Box::new(r.clone()),
                excl_start: false,
                excl_end: false,
            },
            (l, Value::Str(b)) if l.is_numeric() => Value::GenericRange {
                start: Box::new(l.clone()),
                end: Box::new(Value::Str(b.clone())),
                excl_start: false,
                excl_end: false,
            },
            _ => Value::Nil,
        };
        self.stack.push(result);
    }

    pub(super) fn exec_make_range_excl_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExcl(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Box::new(left.clone()),
                end: Box::new(right.clone()),
                excl_start: false,
                excl_end: true,
            },
            _ => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: false,
                excl_end: true,
            },
        };
        self.stack.push(result);
    }

    pub(super) fn exec_make_range_excl_start_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExclStart(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Box::new(left.clone()),
                end: Box::new(right.clone()),
                excl_start: true,
                excl_end: false,
            },
            _ => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: true,
                excl_end: false,
            },
        };
        self.stack.push(result);
    }

    pub(super) fn exec_make_range_excl_both_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (&left, &right) {
            (Value::Int(a), Value::Int(b)) => Value::RangeExclBoth(*a, *b),
            (l, r) if l.is_numeric() || r.is_numeric() => Value::GenericRange {
                start: Box::new(left.clone()),
                end: Box::new(right.clone()),
                excl_start: true,
                excl_end: true,
            },
            _ => Value::GenericRange {
                start: Box::new(left),
                end: Box::new(right),
                excl_start: true,
                excl_end: true,
            },
        };
        self.stack.push(result);
    }

    pub(super) fn exec_num_coerce_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let result = match val {
            Value::Int(i) => Value::Int(i),
            Value::Num(f) => Value::Num(f),
            Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
            Value::Array(items, ..) => Value::Int(items.len() as i64),
            Value::Rat(n, d) => {
                if d == 0 {
                    if n > 0 {
                        Value::Num(f64::INFINITY)
                    } else if n < 0 {
                        Value::Num(f64::NEG_INFINITY)
                    } else {
                        Value::Num(f64::NAN)
                    }
                } else {
                    Value::Rat(n, d)
                }
            }
            Value::Str(s) => {
                if let Ok(i) = s.parse::<i64>() {
                    Value::Int(i)
                } else if let Ok(f) = s.parse::<f64>() {
                    Value::Num(f)
                } else {
                    Value::Int(0)
                }
            }
            Value::Enum { value, .. } => Value::Int(value),
            _ => Value::Int(0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_str_coerce_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::Str(val.to_string_value()));
    }

    pub(super) fn exec_upto_range_op(&mut self) {
        let val = self.stack.pop().unwrap();
        let n = match val {
            Value::Int(i) => i,
            _ => 0,
        };
        self.stack.push(Value::RangeExcl(0, n));
    }

    pub(super) fn exec_pre_increment_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self
            .interpreter
            .env()
            .get(name)
            .cloned()
            .unwrap_or(Value::Int(0));
        let new_val = match val {
            Value::Int(i) => Value::Int(i + 1),
            Value::Bool(_) => Value::Bool(true),
            Value::Rat(n, d) => make_rat(n + d, d),
            _ => Value::Int(1),
        };
        self.interpreter
            .env_mut()
            .insert(name.to_string(), new_val.clone());
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
    }

    pub(super) fn exec_pre_decrement_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self
            .interpreter
            .env()
            .get(name)
            .cloned()
            .unwrap_or(Value::Int(0));
        let new_val = match val {
            Value::Int(i) => Value::Int(i - 1),
            Value::Bool(_) => Value::Bool(false),
            Value::Rat(n, d) => make_rat(n - d, d),
            _ => Value::Int(-1),
        };
        self.interpreter
            .env_mut()
            .insert(name.to_string(), new_val.clone());
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
    }

    pub(super) fn exec_get_capture_var_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self
            .interpreter
            .env()
            .get(name)
            .cloned()
            .unwrap_or(Value::Nil);
        self.stack.push(val);
    }

    pub(super) fn exec_get_code_var_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = self.interpreter.resolve_code_var(name);
        self.stack.push(val);
    }

    pub(super) fn exec_indirect_code_lookup_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let func_name = Self::const_str(code, name_idx).to_string();
        // Pop the package name from the stack (result of evaluating the package expr)
        let package = self.stack.pop().unwrap_or(Value::Nil);
        // Construct a qualified name: "SETTING::OUTER::...::not"
        // resolve_code_var will strip pseudo-package prefixes and resolve to builtin
        let pkg_str = package.to_string_value();
        let qualified = if pkg_str.is_empty() {
            func_name
        } else {
            format!("{}::{}", pkg_str, func_name)
        };
        let val = self.interpreter.resolve_code_var(&qualified);
        self.stack.push(val);
    }

    pub(super) fn exec_assign_expr_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = match &code.constants[name_idx as usize] {
            Value::Str(s) => s.clone(),
            _ => unreachable!("AssignExpr name must be a string constant"),
        };
        let val = self.stack.last().unwrap().clone();
        self.update_local_if_exists(code, &name, &val);
        self.interpreter.env_mut().insert(name, val);
    }

    pub(super) fn exec_get_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        let val = if let Some(value) = std::env::var_os(key) {
            Value::Str(value.to_string_lossy().to_string())
        } else {
            Value::Nil
        };
        self.stack.push(val);
    }

    pub(super) fn exec_exists_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        self.stack
            .push(Value::Bool(std::env::var_os(key).is_some()));
    }

    pub(super) fn exec_exists_expr_op(&mut self) {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        self.stack.push(Value::Bool(val.truthy()));
    }

    pub(super) fn exec_reduction_op(
        &mut self,
        code: &CompiledCode,
        op_idx: u32,
    ) -> Result<(), RuntimeError> {
        let op = Self::const_str(code, op_idx).to_string();
        // Handle negated reduction operators like [!after], [!before]
        let (negate, base_op) = if let Some(stripped) = op.strip_prefix('!') {
            (true, stripped.to_string())
        } else {
            (false, op.clone())
        };
        let list_value = self.stack.pop().unwrap_or(Value::Nil);
        let list = if let Value::LazyList(ref ll) = list_value {
            self.interpreter.force_lazy_list_bridge(ll)?
        } else {
            runtime::value_to_list(&list_value)
        };
        if list.is_empty() {
            self.stack.push(runtime::reduction_identity(&base_op));
        } else {
            let is_comparison = matches!(
                base_op.as_str(),
                "eq" | "ne"
                    | "lt"
                    | "gt"
                    | "le"
                    | "ge"
                    | "after"
                    | "before"
                    | "=="
                    | "!="
                    | "<"
                    | ">"
                    | "<="
                    | ">="
                    | "==="
                    | "eqv"
                    | "cmp"
                    | "leg"
            );
            if is_comparison {
                let mut result = true;
                for i in 0..list.len() - 1 {
                    let v = Interpreter::apply_reduction_op(&base_op, &list[i], &list[i + 1])?;
                    let truthy = if negate { !v.truthy() } else { v.truthy() };
                    if !truthy {
                        result = false;
                        break;
                    }
                }
                self.stack.push(Value::Bool(result));
            } else {
                let mut acc = list[0].clone();
                for item in &list[1..] {
                    let v = Interpreter::apply_reduction_op(&base_op, &acc, item)?;
                    acc = if negate { Value::Bool(!v.truthy()) } else { v };
                }
                self.stack.push(acc);
            }
        }
        Ok(())
    }

    pub(super) fn exec_routine_magic_op(&mut self) -> Result<(), RuntimeError> {
        if let Some((package, name)) = self.interpreter.routine_stack_top() {
            self.stack.push(Value::Routine {
                package: package.clone(),
                name: name.clone(),
            });
        } else {
            return Err(RuntimeError::new("X::Undeclared::Symbols"));
        }
        Ok(())
    }

    pub(super) fn exec_block_magic_op(&mut self) -> Result<(), RuntimeError> {
        if let Some(val) = self.interpreter.block_stack_top().cloned() {
            if matches!(val, Value::Sub(_)) {
                self.stack.push(val);
            } else {
                return Err(RuntimeError::new("X::Undeclared::Symbols"));
            }
        } else {
            return Err(RuntimeError::new("X::Undeclared::Symbols"));
        }
        Ok(())
    }

    pub(super) fn exec_take_op(&mut self) {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        self.interpreter.take_value(val);
    }

    pub(super) fn exec_package_scope_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let body_end = body_end as usize;
        let saved = self.interpreter.current_package().to_string();
        self.interpreter.set_current_package(name);
        self.run_range(code, *ip + 1, body_end, compiled_fns)?;
        self.interpreter.set_current_package(saved);
        *ip = body_end;
        Ok(())
    }

    pub(super) fn exec_phaser_end_op(&mut self, code: &CompiledCode, idx: u32) {
        let stmt = &code.stmt_pool[idx as usize];
        if let crate::ast::Stmt::Phaser { body, .. } = stmt {
            self.interpreter.push_end_phaser(body.clone());
        }
    }

    pub(super) fn exec_type_check_op(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
    ) -> Result<(), RuntimeError> {
        let constraint = Self::const_str(code, tc_idx);
        let value = self.stack.last().expect("TypeCheck: empty stack");
        if !matches!(value, Value::Nil)
            && runtime::is_known_type_constraint(constraint)
            && !self.interpreter.type_matches_value(constraint, value)
        {
            return Err(RuntimeError::new("X::Syntax::Number::LiteralType"));
        }
        Ok(())
    }

    pub(super) fn exec_eval_ast_expr_op(
        &mut self,
        code: &CompiledCode,
        stmt_idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = code.stmt_pool[stmt_idx as usize].clone();
        let result = self.interpreter.eval_block_value(&[stmt])?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_indirect_type_lookup_op(&mut self) {
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        self.stack.push(Value::Package(name));
    }

    pub(super) fn exec_state_var_init_op(&mut self, code: &CompiledCode, slot: u32, key_idx: u32) {
        let init_val = self.stack.pop().unwrap_or(Value::Nil);
        let key = Self::const_str(code, key_idx);
        let val = if let Some(stored) = self.interpreter.get_state_var(key) {
            stored.clone()
        } else {
            self.interpreter
                .set_state_var(key.to_string(), init_val.clone());
            init_val
        };
        let slot_idx = slot as usize;
        self.locals[slot_idx] = val.clone();
        let name = code.locals[slot_idx].clone();
        self.interpreter.env_mut().insert(name, val);
    }

    pub(super) fn exec_block_scope_op(
        &mut self,
        code: &CompiledCode,
        enter_end: u32,
        body_end: u32,
        end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let enter_start = *ip + 1;
        let body_start = enter_end as usize;
        let leave_start = body_end as usize;
        let end = end as usize;

        self.run_range(code, enter_start, body_start, compiled_fns)?;
        let mut body_err = None;
        if let Err(e) = self.run_range(code, body_start, leave_start, compiled_fns) {
            body_err = Some(e);
        }
        let leave_res = self.run_range(code, leave_start, end, compiled_fns);
        if let Err(e) = leave_res
            && body_err.is_none()
        {
            return Err(e);
        }
        if let Some(e) = body_err {
            return Err(e);
        }
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_do_block_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        label: &Option<String>,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        let label = label.clone();
        loop {
            match self.run_range(code, body_start, end, compiled_fns) {
                Ok(()) => break,
                Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => continue,
                Err(e) if e.is_next && Self::label_matches(&e.label, &label) => {
                    self.stack.push(Value::array(vec![]));
                    break;
                }
                Err(e) if e.is_last && Self::label_matches(&e.label, &label) => {
                    self.stack
                        .push(e.return_value.unwrap_or(Value::array(vec![])));
                    break;
                }
                Err(e) => return Err(e),
            }
        }
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_let_save_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        index_mode: bool,
    ) {
        let name = Self::const_str(code, name_idx).to_string();
        if index_mode {
            let _idx_val = self.stack.pop().unwrap_or(Value::Int(0));
        }
        let old_val = self
            .get_env_with_main_alias(&name)
            .or_else(|| {
                code.locals
                    .iter()
                    .position(|n| n == &name)
                    .map(|i| self.locals[i].clone())
            })
            .unwrap_or(Value::Nil);
        self.interpreter.let_saves_push(name, old_val);
    }

    pub(super) fn exec_let_block_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mark = self.interpreter.let_saves_len();
        let body_start = *ip + 1;
        let end = body_end as usize;
        match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => {
                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                if Self::is_let_success(&topic) {
                    self.interpreter.discard_let_saves(mark);
                } else {
                    self.interpreter.restore_let_saves(mark);
                    self.sync_locals_from_env(code);
                }
            }
            Err(e) => {
                self.interpreter.restore_let_saves(mark);
                self.sync_locals_from_env(code);
                return Err(e);
            }
        }
        *ip = end;
        Ok(())
    }
}
