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
            OpCode::GetArrayVar(name_idx) => {
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
            OpCode::GetHashVar(name_idx) => {
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
            OpCode::GetBareWord(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = if let Some(v) = self.interpreter.env().get(name) {
                    if matches!(v, Value::Enum { .. } | Value::Nil) {
                        v.clone()
                    } else if self.interpreter.has_class(name) {
                        Value::Package(name.to_string())
                    } else {
                        Value::Str(name.to_string())
                    }
                } else if self.interpreter.has_class(name) {
                    Value::Package(name.to_string())
                } else {
                    Value::Str(name.to_string())
                };
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
            OpCode::StrLt => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("lt".to_string()))?;
            }
            OpCode::StrGt => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("gt".to_string()))?;
            }
            OpCode::StrLe => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("le".to_string()))?;
            }
            OpCode::StrGe => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("ge".to_string()))?;
            }

            // -- Smart match --
            OpCode::SmartMatch => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SmartMatch)?;
            }
            OpCode::NotMatch => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::BangTilde)?;
            }

            // -- Three-way comparison --
            OpCode::Spaceship => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::LtEqGt)?;
            }
            OpCode::Cmp => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("cmp".to_string()))?;
            }
            OpCode::Leg => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("leg".to_string()))?;
            }

            // -- Identity/value equality --
            OpCode::StrictEq => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::EqEqEq)?;
            }
            OpCode::Eqv => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("eqv".to_string()))?;
            }

            // -- Divisibility --
            OpCode::DivisibleBy => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::PercentPercent)?;
            }

            // -- Keyword math --
            OpCode::IntDiv => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("div".to_string()))?;
            }
            OpCode::IntMod => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("mod".to_string()))?;
            }
            OpCode::Gcd => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("gcd".to_string()))?;
            }
            OpCode::Lcm => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("lcm".to_string()))?;
            }

            // -- Repetition --
            OpCode::StringRepeat => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("x".to_string()))?;
            }
            OpCode::ListRepeat => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::Ident("xx".to_string()))?;
            }

            // -- Pair --
            OpCode::MakePair => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::FatArrow)?;
            }

            // -- Bitwise --
            OpCode::BitAnd => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::BitAnd)?;
            }
            OpCode::BitOr => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::BitOr)?;
            }
            OpCode::BitXor => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::BitXor)?;
            }
            OpCode::BitShiftLeft => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::BitShiftLeft)?;
            }
            OpCode::BitShiftRight => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::BitShiftRight)?;
            }

            // -- Set operations --
            OpCode::SetElem => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetElem)?;
            }
            OpCode::SetCont => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetCont)?;
            }
            OpCode::SetUnion => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetUnion)?;
            }
            OpCode::SetIntersect => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetIntersect)?;
            }
            OpCode::SetDiff => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetDiff)?;
            }
            OpCode::SetSymDiff => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetSymDiff)?;
            }
            OpCode::SetSubset => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetSubset)?;
            }
            OpCode::SetSuperset => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetSuperset)?;
            }
            OpCode::SetStrictSubset => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetStrictSubset)?;
            }
            OpCode::SetStrictSuperset => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::SetStrictSuperset)?;
            }

            // -- Sequence --
            OpCode::Sequence => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::DotDotDot)?;
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

            // -- Range creation --
            OpCode::MakeRange => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::DotDot)?;
            }
            OpCode::MakeRangeExcl => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::DotDotCaret)?;
            }
            OpCode::MakeRangeExclStart => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::CaretDotDot)?;
            }
            OpCode::MakeRangeExclBoth => {
                self.binary_op(code, ip, &crate::lexer::TokenKind::CaretDotDotCaret)?;
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
            OpCode::CallMethodMut { name_idx, arity, target_name_idx } => {
                let method = Self::const_str(code, *name_idx).to_string();
                let target_name = Self::const_str(code, *target_name_idx).to_string();
                let arity = *arity as usize;
                let start = self.stack.len() - arity;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                self.stack.pop(); // discard target value; interpreter re-evaluates from env
                let result = self.interpreter.eval_method_call_mut_with_values(&target_name, &method, args)?;
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

            // -- Given/When control --
            OpCode::Proceed => {
                return Err(RuntimeError::proceed_signal());
            }
            OpCode::Succeed => {
                return Err(RuntimeError::succeed_signal());
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

            // -- Unary coercion --
            OpCode::NumCoerce => {
                let val = self.stack.pop().unwrap();
                let result = match val {
                    Value::Int(i) => Value::Int(i),
                    Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
                    Value::Array(items) => Value::Int(items.len() as i64),
                    Value::Str(s) => Value::Int(s.parse::<i64>().unwrap_or(0)),
                    Value::Enum { value, .. } => Value::Int(value),
                    _ => Value::Int(0),
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::StrCoerce => {
                let val = self.stack.pop().unwrap();
                self.stack.push(Value::Str(val.to_string_value()));
                *ip += 1;
            }
            OpCode::UptoRange => {
                let val = self.stack.pop().unwrap();
                let n = match val {
                    Value::Int(i) => i,
                    _ => 0,
                };
                self.stack.push(Value::RangeExcl(0, n));
                *ip += 1;
            }

            // -- Prefix increment/decrement --
            OpCode::PreIncrement(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.env().get(name).cloned().unwrap_or(Value::Int(0));
                let new_val = match val {
                    Value::Int(i) => Value::Int(i + 1),
                    Value::Rat(n, d) => make_rat(n + d, d),
                    _ => Value::Int(1),
                };
                self.interpreter.env_mut().insert(name.to_string(), new_val.clone());
                self.stack.push(new_val);
                *ip += 1;
            }
            OpCode::PreDecrement(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.env().get(name).cloned().unwrap_or(Value::Int(0));
                let new_val = match val {
                    Value::Int(i) => Value::Int(i - 1),
                    Value::Rat(n, d) => make_rat(n - d, d),
                    _ => Value::Int(-1),
                };
                self.interpreter.env_mut().insert(name.to_string(), new_val.clone());
                self.stack.push(new_val);
                *ip += 1;
            }

            // -- Variable access --
            OpCode::GetCaptureVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.env().get(name).cloned().unwrap_or(Value::Nil);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetCodeVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self.interpreter.resolve_code_var(name);
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

            // -- Given/When/Default --
            OpCode::Given { body_end } => {
                let topic = self.stack.pop().unwrap();
                let body_start = *ip + 1;
                let end = *body_end as usize;

                // Save and set topic + when_matched
                let saved_topic = self.interpreter.env().get("_").cloned();
                let saved_when = self.interpreter.when_matched();
                self.interpreter.env_mut().insert("_".to_string(), topic);
                self.interpreter.set_when_matched(false);

                // Run body, stop if when_matched
                let mut inner_ip = body_start;
                while inner_ip < end {
                    if let Err(e) = self.exec_one(code, &mut inner_ip) {
                        // Restore state before propagating
                        self.interpreter.set_when_matched(saved_when);
                        if let Some(v) = saved_topic {
                            self.interpreter.env_mut().insert("_".to_string(), v);
                        } else {
                            self.interpreter.env_mut().remove("_");
                        }
                        return Err(e);
                    }
                    if self.interpreter.when_matched() || self.interpreter.is_halted() {
                        break;
                    }
                }

                // Restore
                self.interpreter.set_when_matched(saved_when);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                *ip = end;
            }
            OpCode::When { body_end } => {
                let cond_val = self.stack.pop().unwrap();
                let body_start = *ip + 1;
                let end = *body_end as usize;

                let topic = self.interpreter.env().get("_").cloned().unwrap_or(Value::Nil);
                if self.interpreter.smart_match_values(&topic, &cond_val) {
                    let mut did_proceed = false;
                    match self.run_range(code, body_start, end) {
                        Ok(()) => {}
                        Err(e) if e.is_proceed => {
                            did_proceed = true;
                        }
                        Err(e) if e.is_succeed => {
                            // succeed: stop but mark matched
                        }
                        Err(e) => return Err(e),
                    }
                    if !did_proceed {
                        self.interpreter.set_when_matched(true);
                    }
                }
                *ip = end;
            }
            OpCode::Default { body_end } => {
                let body_start = *ip + 1;
                let end = *body_end as usize;
                self.run_range(code, body_start, end)?;
                self.interpreter.set_when_matched(true);
                *ip = end;
            }

            // -- Repeat loop --
            OpCode::RepeatLoop { cond_end, body_end, label } => {
                let body_start = *ip + 1;
                let cond_start = *cond_end as usize;
                let loop_end = *body_end as usize;
                let label = label.clone();

                let mut first = true;
                'repeat_loop: loop {
                    if !first {
                        // Evaluate condition
                        self.run_range(code, cond_start, loop_end)?;
                        let cond_val = self.stack.pop().unwrap();
                        if !cond_val.truthy() {
                            break;
                        }
                    }
                    first = false;
                    // Execute body with redo support
                    'body_redo: loop {
                        match self.run_range(code, body_start, cond_start) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => continue 'body_redo,
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => break 'repeat_loop,
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

            // -- Error handling --
            OpCode::Die => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                return Err(RuntimeError::new(&val.to_string_value()));
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

            // -- Environment variable access --
            OpCode::GetEnvIndex(key_idx) => {
                let key = Self::const_str(code, *key_idx);
                let val = if let Some(value) = std::env::var_os(key) {
                    Value::Str(value.to_string_lossy().to_string())
                } else {
                    Value::Nil
                };
                self.stack.push(val);
                *ip += 1;
            }

            // -- Exists check --
            OpCode::ExistsEnvIndex(key_idx) => {
                let key = Self::const_str(code, *key_idx);
                self.stack.push(Value::Bool(std::env::var_os(key).is_some()));
                *ip += 1;
            }
            OpCode::ExistsExpr => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                self.stack.push(Value::Bool(val.truthy()));
                *ip += 1;
            }

            // -- Reduction ([+] @arr) --
            OpCode::Reduction(op_idx) => {
                let op = Self::const_str(code, *op_idx).to_string();
                let list_value = self.stack.pop().unwrap_or(Value::Nil);
                let list = self.interpreter.list_from_value(list_value)?;
                if list.is_empty() {
                    self.stack.push(self.interpreter.reduction_identity_value(&op));
                } else {
                    let mut acc = list[0].clone();
                    for item in &list[1..] {
                        acc = self.interpreter.apply_reduction_op_values(&op, &acc, item)?;
                    }
                    self.stack.push(acc);
                }
                *ip += 1;
            }

            // -- Magic variables --
            OpCode::RoutineMagic => {
                if let Some((package, name)) = self.interpreter.routine_stack_top() {
                    self.stack.push(Value::Routine {
                        package: package.clone(),
                        name: name.clone(),
                    });
                } else {
                    return Err(RuntimeError::new("X::Undeclared::Symbols"));
                }
                *ip += 1;
            }
            OpCode::BlockMagic => {
                if let Some(val) = self.interpreter.block_stack_top().cloned() {
                    if matches!(val, Value::Sub { .. }) {
                        self.stack.push(val);
                    } else {
                        return Err(RuntimeError::new("X::Undeclared::Symbols"));
                    }
                } else {
                    return Err(RuntimeError::new("X::Undeclared::Symbols"));
                }
                *ip += 1;
            }

            // -- Substitution (s///) --
            OpCode::Subst { pattern_idx, replacement_idx } => {
                let pattern = Self::const_str(code, *pattern_idx).to_string();
                let replacement = Self::const_str(code, *replacement_idx).to_string();
                let target = self.interpreter.env().get("_").cloned().unwrap_or(Value::Nil);
                let text = target.to_string_value();
                if let Some((start, end)) = self.interpreter.regex_find_first_bridge(&pattern, &text) {
                    let start_b = Interpreter::char_idx_to_byte_bridge(&text, start);
                    let end_b = Interpreter::char_idx_to_byte_bridge(&text, end);
                    let mut out = String::new();
                    out.push_str(&text[..start_b]);
                    out.push_str(&replacement);
                    out.push_str(&text[end_b..]);
                    let result = Value::Str(out);
                    self.interpreter.env_mut().insert("_".to_string(), result.clone());
                    self.stack.push(result);
                } else {
                    self.stack.push(Value::Str(text));
                }
                *ip += 1;
            }

            // -- Take (gather/take) --
            OpCode::Take => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                self.interpreter.take_value(val);
                *ip += 1;
            }

            // -- Package scope --
            OpCode::PackageScope { name_idx, body_end } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let body_end = *body_end as usize;
                let saved = self.interpreter.current_package().to_string();
                self.interpreter.set_current_package(name);
                self.run_range(code, *ip + 1, body_end)?;
                self.interpreter.set_current_package(saved);
                *ip = body_end;
            }

            // -- Phaser END (defer body for later execution) --
            OpCode::PhaserEnd(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let crate::ast::Stmt::Phaser { body, .. } = stmt {
                    self.interpreter.push_end_phaser(body.clone());
                }
                *ip += 1;
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
