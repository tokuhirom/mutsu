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
            match &code.ops[ip] {
                // -- Constants --
                OpCode::LoadConst(idx) => {
                    self.stack.push(code.constants[*idx as usize].clone());
                }
                OpCode::LoadNil => {
                    self.stack.push(Value::Nil);
                }
                OpCode::LoadTrue => {
                    self.stack.push(Value::Bool(true));
                }
                OpCode::LoadFalse => {
                    self.stack.push(Value::Bool(false));
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
                }
                OpCode::SetGlobal(name_idx) => {
                    let name = match &code.constants[*name_idx as usize] {
                        Value::Str(s) => s.clone(),
                        _ => unreachable!("SetGlobal name must be a string constant"),
                    };
                    let val = self.stack.pop().unwrap();
                    // Hash coercion for % variables
                    let val = if name.starts_with('%') {
                        Interpreter::coerce_to_hash(val)
                    } else {
                        val
                    };
                    self.interpreter.env_mut().insert(name, val);
                }

                // -- Arithmetic --
                OpCode::Add => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Plus, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::Sub => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Minus, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::Mul => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Star, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::Div => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Slash, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::Mod => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Percent, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::Pow => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::StarStar, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::Negate => {
                    let val = self.stack.pop().unwrap();
                    let result = match val {
                        Value::Int(i) => Value::Int(-i),
                        Value::Num(f) => Value::Num(-f),
                        Value::Rat(n, d) => Value::Rat(-n, d),
                        Value::Complex(r, i) => Value::Complex(-r, -i),
                        _ => {
                            return (
                                self.interpreter,
                                Err(RuntimeError::new("Unary - expects numeric")),
                            );
                        }
                    };
                    self.stack.push(result);
                }

                // -- Logic / coercion --
                OpCode::Not => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(!val.truthy()));
                }
                OpCode::BoolCoerce => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(val.truthy()));
                }

                // -- String --
                OpCode::Concat => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Tilde, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }

                // -- Numeric comparison --
                OpCode::NumEq => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::EqEq, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::NumNe => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::BangEq, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::NumLt => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Lt, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::NumLe => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Lte, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::NumGt => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Gt, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }
                OpCode::NumGe => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    match self.interpreter.eval_binary(left, &crate::lexer::TokenKind::Gte, right) {
                        Ok(v) => self.stack.push(v),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }

                // -- String comparison --
                OpCode::StrEq => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(
                        left.to_string_value() == right.to_string_value(),
                    ));
                }
                OpCode::StrNe => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(
                        left.to_string_value() != right.to_string_value(),
                    ));
                }

                // -- Nil check --
                OpCode::IsNil => {
                    let val = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(matches!(val, Value::Nil)));
                }

                // -- Control flow --
                OpCode::Jump(target) => {
                    ip = *target as usize;
                    continue;
                }
                OpCode::JumpIfFalse(target) => {
                    let val = self.stack.pop().unwrap();
                    if !val.truthy() {
                        ip = *target as usize;
                        continue;
                    }
                }
                OpCode::JumpIfTrue(target) => {
                    // Non-destructive peek: value stays on stack
                    let val = self.stack.last().unwrap();
                    if val.truthy() {
                        ip = *target as usize;
                        continue;
                    }
                }
                OpCode::JumpIfNil(target) => {
                    let val = self.stack.last().unwrap();
                    if matches!(val, Value::Nil) {
                        ip = *target as usize;
                        continue;
                    }
                }
                OpCode::JumpIfNotNil(target) => {
                    let val = self.stack.last().unwrap();
                    if !matches!(val, Value::Nil) {
                        ip = *target as usize;
                        continue;
                    }
                }

                // -- Stack manipulation --
                OpCode::Dup => {
                    let val = self.stack.last().unwrap().clone();
                    self.stack.push(val);
                }
                OpCode::Pop => {
                    self.stack.pop();
                }

                // -- Composite --
                OpCode::MakeArray(n) => {
                    let n = *n as usize;
                    let start = self.stack.len() - n;
                    let elems: Vec<Value> = self.stack.drain(start..).collect();
                    self.stack.push(Value::Array(elems));
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
                    if let Err(e) = self.interpreter.write_to_named_handle("$*OUT", &line, true) {
                        return (self.interpreter, Err(e));
                    }
                }
                OpCode::Print(n) => {
                    let n = *n as usize;
                    let start = self.stack.len() - n;
                    let values: Vec<Value> = self.stack.drain(start..).collect();
                    let mut content = String::new();
                    for v in &values {
                        content.push_str(&v.to_string_value());
                    }
                    if let Err(e) =
                        self.interpreter
                            .write_to_named_handle("$*OUT", &content, false)
                    {
                        return (self.interpreter, Err(e));
                    }
                }

                // -- Functions --
                OpCode::Return => {
                    let val = self.stack.pop().unwrap_or(Value::Nil);
                    return (
                        self.interpreter,
                        Err(RuntimeError {
                            message: String::new(),
                            return_value: Some(val),
                            is_last: false,
                            is_next: false,
                            is_redo: false,
                            is_proceed: false,
                            is_succeed: false,
                            label: None,
                        }),
                    );
                }

                // -- Fallback to tree-walker --
                OpCode::InterpretStmt(idx) => {
                    let stmt = &code.stmt_pool[*idx as usize];
                    if let Err(e) = self.interpreter.exec_stmt(stmt) {
                        return (self.interpreter, Err(e));
                    }
                    if self.interpreter.is_halted() {
                        break;
                    }
                }
                OpCode::InterpretExpr(idx) => {
                    let expr = &code.expr_pool[*idx as usize];
                    match self.interpreter.eval_expr(expr) {
                        Ok(val) => self.stack.push(val),
                        Err(e) => return (self.interpreter, Err(e)),
                    }
                }

                // Not yet used
                OpCode::GetLocal(_) | OpCode::SetLocal(_) => {
                    unreachable!("Local variable opcodes not yet implemented");
                }
            }
            ip += 1;
        }
        (self.interpreter, Ok(()))
    }
}
