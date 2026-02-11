use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, OpCode};
use crate::value::{RuntimeError, Value};

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
                let name = match &code.constants[*name_idx as usize] {
                    Value::Str(s) => s.as_str(),
                    _ => unreachable!("GetGlobal name must be a string constant"),
                };
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

            // -- String comparison --
            OpCode::StrEq => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(Value::Bool(
                    left.to_string_value() == right.to_string_value(),
                ));
                *ip += 1;
            }
            OpCode::StrNe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(Value::Bool(
                    left.to_string_value() != right.to_string_value(),
                ));
                *ip += 1;
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
