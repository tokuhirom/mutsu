use std::collections::HashMap;

use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, OpCode};
use crate::value::{make_rat, RuntimeError, Value};

pub(crate) struct VM {
    interpreter: Interpreter,
    stack: Vec<Value>,
}

impl VM {
    pub(crate) fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            stack: Vec::new(),
        }
    }

    /// Run the compiled bytecode. Always returns the interpreter back
    /// (even on error) so the caller can restore it.
    pub(crate) fn run(mut self, code: &CompiledCode) -> (Interpreter, Result<(), RuntimeError>) {
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip) {
                return (self.interpreter, Err(e));
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        (self.interpreter, Ok(()))
    }

    /// Execute opcodes in [start..end), used by loop compound opcodes.
    fn run_range(
        &mut self,
        code: &CompiledCode,
        start: usize,
        end: usize,
    ) -> Result<(), RuntimeError> {
        let mut ip = start;
        while ip < end {
            self.exec_one(code, &mut ip)?;
            if self.interpreter.is_halted() {
                break;
            }
        }
        Ok(())
    }

    /// Resolve a string constant name from index.
    fn const_str<'a>(code: &'a CompiledCode, idx: u32) -> &'a str {
        match &code.constants[idx as usize] {
            Value::Str(s) => s.as_str(),
            _ => unreachable!("expected string constant"),
        }
    }

    /// Execute one opcode at *ip, advancing ip for the next instruction.
    fn exec_one(&mut self, code: &CompiledCode, ip: &mut usize) -> Result<(), RuntimeError> {
        match &code.ops[*ip] {
            // -- Constants --
            OpCode::LoadConst(idx) => {
                self.stack.push(code.constants[*idx as usize].clone());
                *ip += 1;
            }
            OpCode::LoadNil => {
                self.stack.push(Value::Nil);
                *ip += 1;
            }
            OpCode::LoadTrue => {
                self.stack.push(Value::Bool(true));
                *ip += 1;
            }
            OpCode::LoadFalse => {
                self.stack.push(Value::Bool(false));
                *ip += 1;
            }

            // -- Variables --
            OpCode::GetGlobal(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .interpreter
                    .env()
                    .get(name)
                    .cloned()
                    .unwrap_or(Value::Nil);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::SetGlobal(name_idx) => {
                let name = match &code.constants[*name_idx as usize] {
                    Value::Str(s) => s.clone(),
                    _ => unreachable!("SetGlobal name must be a string constant"),
                };
                let val = self.stack.pop().unwrap();
                let val = if name.starts_with('%') {
                    Interpreter::coerce_to_hash(val)
                } else {
                    val
                };
                self.interpreter.env_mut().insert(name, val);
                *ip += 1;
            }

            // -- Arithmetic (delegate to interpreter's eval_binary) --
            OpCode::Add => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Plus)?;
            }
            OpCode::Sub => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Minus)?;
            }
            OpCode::Mul => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Star)?;
            }
            OpCode::Div => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Slash)?;
            }
            OpCode::Mod => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Percent)?;
            }
            OpCode::Pow => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::StarStar)?;
            }
            OpCode::Negate => {
                let val = self.stack.pop().unwrap();
                let result = match val {
                    Value::Int(i) => Value::Int(-i),
                    Value::Num(f) => Value::Num(-f),
                    Value::Rat(n, d) => Value::Rat(-n, d),
                    Value::Complex(r, i) => Value::Complex(-r, -i),
                    Value::Str(ref s) => {
                        if let Ok(i) = s.trim().parse::<i64>() {
                            Value::Int(-i)
                        } else if let Ok(f) = s.trim().parse::<f64>() {
                            Value::Num(-f)
                        } else {
                            return Err(RuntimeError::new("Unary - expects numeric"));
                        }
                    }
                    _ => return Err(RuntimeError::new("Unary - expects numeric")),
                };
                self.stack.push(result);
                *ip += 1;
            }

            // -- Logic / coercion --
            OpCode::Not => {
                let val = self.stack.pop().unwrap();
                self.stack.push(Value::Bool(!val.truthy()));
                *ip += 1;
            }
            OpCode::BoolCoerce => {
                let val = self.stack.pop().unwrap();
                self.stack.push(Value::Bool(val.truthy()));
                *ip += 1;
            }

            // -- String --
            OpCode::Concat => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Tilde)?;
            }

            // -- Numeric comparison --
            OpCode::NumEq => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::EqEq)?;
            }
            OpCode::NumNe => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::BangEq)?;
            }
            OpCode::NumLt => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Lt)?;
            }
            OpCode::NumLe => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Lte)?;
            }
            OpCode::NumGt => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Gt)?;
            }
            OpCode::NumGe => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Gte)?;
            }

            // -- String comparison (delegated to eval_binary for junction support) --
            OpCode::StrEq => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("eq".to_string()))?;
            }
            OpCode::StrNe => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("ne".to_string()))?;
            }

            // -- Nil check --
            OpCode::IsNil => {
                let val = self.stack.pop().unwrap();
                self.stack.push(Value::Bool(matches!(val, Value::Nil)));
                *ip += 1;
            }

            // -- Control flow --
            OpCode::Jump(target) => {
                *ip = *target as usize;
            }
            OpCode::JumpIfFalse(target) => {
                let val = self.stack.pop().unwrap();
                if !val.truthy() {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfTrue(target) => {
                let val = self.stack.last().unwrap();
                if val.truthy() {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfNil(target) => {
                let val = self.stack.last().unwrap();
                if matches!(val, Value::Nil) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfNotNil(target) => {
                let val = self.stack.last().unwrap();
                if !matches!(val, Value::Nil) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }

            // -- Stack manipulation --
            OpCode::Dup => {
                let val = self.stack.last().unwrap().clone();
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::Pop => {
                self.stack.pop();
                *ip += 1;
            }

            // -- Composite --
            OpCode::MakeArray(n) => {
                let n = *n as usize;
                let start = self.stack.len() - n;
                let elems: Vec<Value> = self.stack.drain(start..).collect();
                self.stack.push(Value::Array(elems));
                *ip += 1;
            }
            OpCode::MakeHash(n) => {
                let n = *n as usize;
                let start = self.stack.len() - n * 2;
                let items: Vec<Value> = self.stack.drain(start..).collect();
                let mut map = HashMap::new();
                for pair in items.chunks(2) {
                    let key = pair[0].to_string_value();
                    let val = pair[1].clone();
                    map.insert(key, val);
                }
                self.stack.push(Value::Hash(map));
                *ip += 1;
            }

            // -- I/O --
            OpCode::Say(n) => {
                let n = *n as usize;
                let start = self.stack.len() - n;
                let values: Vec<Value> = self.stack.drain(start..).collect();
                let mut parts = Vec::new();
                for v in &values {
                    parts.push(self.interpreter.gist_value(v));
                }
                let line = parts.join(" ");
                self.interpreter
                    .write_to_named_handle("$*OUT", &line, true)?;
                *ip += 1;
            }
            OpCode::Print(n) => {
                let n = *n as usize;
                let start = self.stack.len() - n;
                let values: Vec<Value> = self.stack.drain(start..).collect();
                let mut content = String::new();
                for v in &values {
                    content.push_str(&v.to_string_value());
                }
                self.interpreter
                    .write_to_named_handle("$*OUT", &content, false)?;
                *ip += 1;
            }

            // -- Calls --
            OpCode::CallFunc { name_idx, arity } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let arity = *arity as usize;
                let start = self.stack.len() - arity;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                let result = self.interpreter.eval_call_with_values(&name, args)?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::CallMethod { name_idx, arity } => {
                let method = Self::const_str(code, *name_idx).to_string();
                let arity = *arity as usize;
                let start = self.stack.len() - arity;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                let target = self.stack.pop().unwrap();
                let result = self.interpreter.eval_method_call_with_values(target, &method, args)?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::ExecCall { name_idx, arity } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let arity = *arity as usize;
                let start = self.stack.len() - arity;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                self.interpreter.exec_call_with_values(&name, args)?;
                *ip += 1;
            }

            // -- Indexing --
            OpCode::Index => {
                let index = self.stack.pop().unwrap();
                let target = self.stack.pop().unwrap();
                // Delegate to interpreter's Expr::Index evaluation
                let result = self.interpreter.eval_expr(&crate::ast::Expr::Index {
                    target: Box::new(crate::ast::Expr::Literal(target)),
                    index: Box::new(crate::ast::Expr::Literal(index)),
                })?;
                self.stack.push(result);
                *ip += 1;
            }

            // -- String interpolation --
            OpCode::StringConcat(n) => {
                let n = *n as usize;
                let start = self.stack.len() - n;
                let values: Vec<Value> = self.stack.drain(start..).collect();
                let mut result = String::new();
                for v in values {
                    result.push_str(&v.to_string_value());
                }
                self.stack.push(Value::Str(result));
                *ip += 1;
            }

            // -- Loop control --
            OpCode::Last(label) => {
                let mut sig = RuntimeError::last_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Next(label) => {
                let mut sig = RuntimeError::next_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            OpCode::Redo => {
                return Err(RuntimeError::redo_signal());
            }

            // -- Postfix operators --
            OpCode::PostIncrement(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.env().get(name).cloned().unwrap_or(Value::Int(0));
                let new_val = match &val {
                    Value::Int(i) => Value::Int(i + 1),
                    Value::Rat(n, d) => make_rat(n + d, *d),
                    _ => Value::Int(1),
                };
                self.interpreter.env_mut().insert(name.to_string(), new_val);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::PostDecrement(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.env().get(name).cloned().unwrap_or(Value::Int(0));
                let new_val = match &val {
                    Value::Int(i) => Value::Int(i - 1),
                    Value::Rat(n, d) => make_rat(n - d, *d),
                    _ => Value::Int(-1),
                };
                self.interpreter.env_mut().insert(name.to_string(), new_val);
                self.stack.push(val);
                *ip += 1;
            }

            // -- Assignment as expression --
            OpCode::AssignExpr(name_idx) => {
                let name = match &code.constants[*name_idx as usize] {
                    Value::Str(s) => s.clone(),
                    _ => unreachable!("AssignExpr name must be a string constant"),
                };
                let val = self.stack.last().unwrap().clone();
                self.interpreter.env_mut().insert(name, val);
                *ip += 1;
            }

            // -- Loops --
            OpCode::WhileLoop { cond_end, body_end, label } => {
                let cond_start = *ip + 1;
                let body_start = *cond_end as usize;
                let loop_end = *body_end as usize;
                let label = label.clone();

                'while_loop: loop {
                    // Evaluate condition
                    self.run_range(code, cond_start, body_start)?;
                    let cond_val = self.stack.pop().unwrap();
                    if !cond_val.truthy() {
                        break;
                    }
                    // Execute body with redo support
                    'body_redo: loop {
                        match self.run_range(code, body_start, loop_end) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => continue 'body_redo,
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => break 'while_loop,
                            Err(e) if e.is_next && Self::label_matches(&e.label, &label) => break 'body_redo,
                            Err(e) => return Err(e),
                        }
                    }
                    if self.interpreter.is_halted() {
                        break;
                    }
                }
                *ip = loop_end;
            }
            OpCode::ForLoop {
                param_idx,
                body_end,
                label,
            } => {
                let iterable = self.stack.pop().unwrap();
                let items = self.interpreter.list_from_value(iterable)?;
                let body_start = *ip + 1;
                let loop_end = *body_end as usize;
                let label = label.clone();

                let param_name = param_idx.map(|idx| match &code.constants[idx as usize] {
                    Value::Str(s) => s.clone(),
                    _ => unreachable!("ForLoop param must be a string constant"),
                });

                'for_loop: for item in items {
                    self.interpreter
                        .env_mut()
                        .insert("_".to_string(), item.clone());
                    if let Some(ref name) = param_name {
                        self.interpreter
                            .env_mut()
                            .insert(name.clone(), item);
                    }
                    'body_redo: loop {
                        match self.run_range(code, body_start, loop_end) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => continue 'body_redo,
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => break 'for_loop,
                            Err(e) if e.is_next && Self::label_matches(&e.label, &label) => break 'body_redo,
                            Err(e) => return Err(e),
                        }
                    }
                    if self.interpreter.is_halted() {
                        break;
                    }
                }
                *ip = loop_end;
            }
            OpCode::CStyleLoop {
                cond_end,
                step_start,
                body_end,
                label,
            } => {
                let cond_start = *ip + 1;
                let body_start = *cond_end as usize;
                let step_begin = *step_start as usize;
                let loop_end = *body_end as usize;
                let label = label.clone();

                'c_loop: loop {
                    // Evaluate condition
                    self.run_range(code, cond_start, body_start)?;
                    let cond_val = self.stack.pop().unwrap();
                    if !cond_val.truthy() {
                        break;
                    }
                    // Execute body with redo support
                    'body_redo: loop {
                        match self.run_range(code, body_start, step_begin) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => continue 'body_redo,
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => break 'c_loop,
                            Err(e) if e.is_next && Self::label_matches(&e.label, &label) => break 'body_redo,
                            Err(e) => return Err(e),
                        }
                    }
                    if self.interpreter.is_halted() {
                        break;
                    }
                    // Execute step
                    self.run_range(code, step_begin, loop_end)?;
                }
                *ip = loop_end;
            }

            // -- Functions --
            OpCode::Return => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                return Err(RuntimeError {
                    message: String::new(),
                    return_value: Some(val),
                    is_last: false,
                    is_next: false,
                    is_redo: false,
                    is_proceed: false,
                    is_succeed: false,
                    label: None,
                });
            }

            // -- Fallback to tree-walker --
            OpCode::InterpretStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                self.interpreter.exec_stmt(stmt)?;
                *ip += 1;
            }
            OpCode::InterpretExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                let val = self.interpreter.eval_expr(expr)?;
                self.stack.push(val);
                *ip += 1;
            }

            // Not yet used
            OpCode::GetLocal(_) | OpCode::SetLocal(_) => {
                unreachable!("Local variable opcodes not yet implemented");
            }
        }
        Ok(())
    }

    /// Check if a loop control error's label matches this loop's label.
    /// Matches if: error has no label (targets innermost), or labels are equal.
    fn label_matches(error_label: &Option<String>, loop_label: &Option<String>) -> bool {
        match error_label {
            None => true,
            Some(el) => loop_label.as_ref().is_some_and(|ll| ll == el),
        }
    }

    /// Helper for binary operations that delegate to interpreter.eval_binary.
    fn binary_op(
        &mut self,
        _code: &CompiledCode,
        ip: &mut usize,
        token: &crate::lexer::TokenKind,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.interpreter.eval_binary(left, token, right)?;
        self.stack.push(result);
        *ip += 1;
        Ok(())
    }
}
