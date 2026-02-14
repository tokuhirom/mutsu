#![allow(clippy::result_large_err)]
use std::collections::HashMap;

use crate::ast::{CallArg, Expr, Stmt};
use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::value::{JunctionKind, RuntimeError, Value, make_rat};
use num_traits::{Signed, Zero};

pub(crate) struct VM {
    interpreter: Interpreter,
    stack: Vec<Value>,
    locals: Vec<Value>,
}

impl VM {
    pub(crate) fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            stack: Vec::new(),
            locals: Vec::new(),
        }
    }

    /// Run the compiled bytecode. Always returns the interpreter back
    /// (even on error) so the caller can restore it.
    pub(crate) fn run(
        mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> (Interpreter, Result<(), RuntimeError>) {
        // Initialize local variable slots
        self.locals = vec![Value::Nil; code.locals.len()];
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
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
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mut ip = start;
        while ip < end {
            self.exec_one(code, &mut ip, compiled_fns)?;
            if self.interpreter.is_halted() {
                break;
            }
        }
        Ok(())
    }

    /// Resolve a string constant name from index.
    fn const_str(code: &CompiledCode, idx: u32) -> &str {
        match &code.constants[idx as usize] {
            Value::Str(s) => s.as_str(),
            _ => unreachable!("expected string constant"),
        }
    }

    fn call_arg_needs_raw_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Block(_) => true,
            Expr::Hash(pairs) => pairs
                .iter()
                .any(|(_, v)| v.as_ref().is_some_and(Self::call_arg_needs_raw_expr)),
            _ => false,
        }
    }

    fn is_builtin_type(name: &str) -> bool {
        matches!(
            name,
            "Hash"
                | "Array"
                | "Int"
                | "Num"
                | "Rat"
                | "FatRat"
                | "Complex"
                | "Str"
                | "Bool"
                | "Pair"
                | "Map"
                | "Set"
                | "Bag"
                | "Mix"
                | "List"
                | "Seq"
                | "Range"
                | "Any"
                | "Mu"
                | "Cool"
                | "Real"
                | "Numeric"
                | "Stringy"
                | "Positional"
                | "Associative"
                | "Failure"
                | "Exception"
                | "Order"
                | "Version"
                | "Nil"
                | "Regex"
                | "Block"
                | "Routine"
                | "Sub"
                | "Method"
                | "IO"
                | "Proc"
                | "Slip"
        )
    }

    /// Execute one opcode at *ip, advancing ip for the next instruction.
    fn exec_one(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        crate::trace::trace_log!(
            "vm",
            "exec_one[{}]: {:?}",
            ip,
            std::mem::discriminant(&code.ops[*ip])
        );
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
                    } else if self.interpreter.has_class(name) || Self::is_builtin_type(name) {
                        Value::Package(name.to_string())
                    } else if !name.starts_with('$')
                        && !name.starts_with('@')
                        && !name.starts_with('%')
                    {
                        // Sigil-less variable (e.g., from constant declaration)
                        v.clone()
                    } else {
                        Value::Str(name.to_string())
                    }
                } else if self.interpreter.has_class(name) || Self::is_builtin_type(name) {
                    Value::Package(name.to_string())
                } else if self.interpreter.has_function(name) {
                    // Bare function call with no args.
                    if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                        let pkg = self.interpreter.current_package().to_string();
                        let result = self.call_compiled_function_named(
                            cf,
                            Vec::new(),
                            compiled_fns,
                            &pkg,
                            name,
                        )?;
                        self.sync_locals_from_env(code);
                        result
                    } else if let Some(native_result) = Self::try_native_function(name, &[]) {
                        native_result?
                    } else {
                        let result = self.interpreter.eval_call_with_values(name, Vec::new())?;
                        self.sync_locals_from_env(code);
                        result
                    }
                } else if name == "NaN" {
                    Value::Num(f64::NAN)
                } else if name == "Inf" {
                    Value::Num(f64::INFINITY)
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

            // -- Arithmetic (native) --
            OpCode::Add => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(crate::builtins::arith_add(left, right)?);
                *ip += 1;
            }
            OpCode::Sub => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(crate::builtins::arith_sub(left, right));
                *ip += 1;
            }
            OpCode::Mul => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(crate::builtins::arith_mul(left, right));
                *ip += 1;
            }
            OpCode::Div => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(crate::builtins::arith_div(left, right)?);
                *ip += 1;
            }
            OpCode::Mod => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(crate::builtins::arith_mod(left, right)?);
                *ip += 1;
            }
            OpCode::Pow => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(crate::builtins::arith_pow(left, right));
                *ip += 1;
            }
            OpCode::Negate => {
                let val = self.stack.pop().unwrap();
                self.stack.push(crate::builtins::arith_negate(val)?);
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

            // -- String (native) --
            OpCode::Concat => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(Value::Str(format!(
                    "{}{}",
                    left.to_string_value(),
                    right.to_string_value()
                )));
                *ip += 1;
            }

            // -- Numeric comparison (native with junction threading) --
            OpCode::NumEq => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    if matches!(l, Value::Nil) || matches!(r, Value::Nil) {
                        Ok(Value::Bool(
                            Interpreter::to_float_value(&l) == Interpreter::to_float_value(&r),
                        ))
                    } else {
                        Ok(Value::Bool(l == r))
                    }
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::NumNe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    if matches!(l, Value::Nil) || matches!(r, Value::Nil) {
                        Ok(Value::Bool(
                            Interpreter::to_float_value(&l) != Interpreter::to_float_value(&r),
                        ))
                    } else {
                        Ok(Value::Bool(l != r))
                    }
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::NumLt => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Interpreter::compare(l, r, |o| o < 0)
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::NumLe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Interpreter::compare(l, r, |o| o <= 0)
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::NumGt => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Interpreter::compare(l, r, |o| o > 0)
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::NumGe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Interpreter::compare(l, r, |o| o >= 0)
                })?;
                self.stack.push(result);
                *ip += 1;
            }

            // -- String comparison (native with junction threading) --
            OpCode::StrEq => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Ok(Value::Bool(l.to_string_value() == r.to_string_value()))
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::StrNe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Ok(Value::Bool(l.to_string_value() != r.to_string_value()))
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::StrLt => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Ok(Value::Bool(l.to_string_value() < r.to_string_value()))
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::StrGt => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Ok(Value::Bool(l.to_string_value() > r.to_string_value()))
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::StrLe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Ok(Value::Bool(l.to_string_value() <= r.to_string_value()))
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::StrGe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Ok(Value::Bool(l.to_string_value() >= r.to_string_value()))
                })?;
                self.stack.push(result);
                *ip += 1;
            }

            // Smart match (~~, !~~) handled via interpreter fallback (needs $_ binding)

            // -- Three-way comparison (native) --
            OpCode::Spaceship => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let ord = match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Rat(_, _), _)
                    | (_, Value::Rat(_, _))
                    | (Value::FatRat(_, _), _)
                    | (_, Value::FatRat(_, _)) => {
                        if let (Some((an, ad)), Some((bn, bd))) = (
                            Interpreter::to_rat_parts(&left),
                            Interpreter::to_rat_parts(&right),
                        ) {
                            (an.wrapping_mul(bd)).cmp(&(bn.wrapping_mul(ad)))
                        } else {
                            left.to_string_value().cmp(&right.to_string_value())
                        }
                    }
                    (Value::Num(a), Value::Num(b)) => {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Int(a), Value::Num(b)) => (*a as f64)
                        .partial_cmp(b)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a
                        .partial_cmp(&(*b as f64))
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                        Interpreter::version_cmp_parts(ap, bp)
                    }
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                self.stack.push(Interpreter::make_order(ord));
                *ip += 1;
            }
            OpCode::Cmp => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let ord = match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Rat(_, _), _)
                    | (_, Value::Rat(_, _))
                    | (Value::FatRat(_, _), _)
                    | (_, Value::FatRat(_, _)) => {
                        if let (Some((an, ad)), Some((bn, bd))) = (
                            Interpreter::to_rat_parts(&left),
                            Interpreter::to_rat_parts(&right),
                        ) {
                            (an.wrapping_mul(bd)).cmp(&(bn.wrapping_mul(ad)))
                        } else {
                            left.to_string_value().cmp(&right.to_string_value())
                        }
                    }
                    (Value::Num(a), Value::Num(b)) => {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Int(a), Value::Num(b)) => (*a as f64)
                        .partial_cmp(b)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a
                        .partial_cmp(&(*b as f64))
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                        Interpreter::version_cmp_parts(ap, bp)
                    }
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                self.stack.push(Interpreter::make_order(ord));
                *ip += 1;
            }
            OpCode::Leg => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let ord = left.to_string_value().cmp(&right.to_string_value());
                self.stack.push(Interpreter::make_order(ord));
                *ip += 1;
            }

            // -- Identity/value equality (native) --
            OpCode::StrictEq => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack.push(Value::Bool(left == right));
                *ip += 1;
            }
            OpCode::Eqv => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
                    Ok(Value::Bool(l.eqv(&r)))
                })?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::SmartMatchExpr { rhs_end, negate } => {
                let left = self.stack.pop().unwrap();
                let rhs_start = *ip + 1;
                let rhs_end = *rhs_end as usize;
                let saved_topic = self.interpreter.env().get("_").cloned();
                self.interpreter
                    .env_mut()
                    .insert("_".to_string(), left.clone());
                self.run_range(code, rhs_start, rhs_end, compiled_fns)?;
                let right = self.stack.pop().unwrap_or(Value::Nil);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                let op = if *negate {
                    crate::lexer::TokenKind::BangTilde
                } else {
                    crate::lexer::TokenKind::SmartMatch
                };
                let out = self.interpreter.eval_binary(left, &op, right)?;
                self.stack.push(out);
                self.sync_locals_from_env(code);
                *ip = rhs_end;
            }

            // -- Divisibility (native) --
            OpCode::DivisibleBy => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result = match (l, r) {
                    (Value::Int(_), Value::Int(0)) => {
                        return Err(RuntimeError::new("Divisibility by zero"));
                    }
                    (Value::Int(a), Value::Int(b)) => Value::Bool(a % b == 0),
                    _ => Value::Bool(false),
                };
                self.stack.push(result);
                *ip += 1;
            }

            // -- Keyword math (native) --
            OpCode::IntDiv => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Int(a), Value::Int(b)) if b != 0 => Value::Int(a.div_euclid(b)),
                    (Value::Int(_), Value::Int(_)) => {
                        return Err(RuntimeError::new("Division by zero"));
                    }
                    _ => return Err(RuntimeError::new("div expects Int")),
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::IntMod => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Int(a), Value::Int(b)) if b != 0 => Value::Int(a.rem_euclid(b)),
                    (Value::Int(_), Value::Int(_)) => {
                        return Err(RuntimeError::new("Modulo by zero"));
                    }
                    _ => return Err(RuntimeError::new("mod expects Int")),
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::Gcd => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                // Use BigInt arithmetic to handle arbitrary-precision integers
                let a = left.to_bigint().abs();
                let b = right.to_bigint().abs();
                let g = num_integer::Integer::gcd(&a, &b);
                self.stack.push(Value::from_bigint(g));
                *ip += 1;
            }
            OpCode::Lcm => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let a = left.to_bigint().abs();
                let b = right.to_bigint().abs();
                let result = if a.is_zero() && b.is_zero() {
                    Value::Int(0)
                } else {
                    let g = num_integer::Integer::gcd(&a, &b);
                    Value::from_bigint(&a / &g * &b)
                };
                self.stack.push(result);
                *ip += 1;
            }

            // -- Repetition (native) --
            OpCode::StringRepeat => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let s = left.to_string_value();
                let n = match right {
                    Value::Int(n) => n.max(0) as usize,
                    _ => 0,
                };
                self.stack.push(Value::Str(s.repeat(n)));
                *ip += 1;
            }
            OpCode::ListRepeat => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let n = match right {
                    Value::Int(n) => n.max(0) as usize,
                    _ => 0,
                };
                let items: Vec<Value> = std::iter::repeat_n(left, n).collect();
                self.stack.push(Value::Array(items));
                *ip += 1;
            }

            // -- Mixin (native) --
            OpCode::ButMixin => {
                let _right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                // Simplified: return left value unchanged
                self.stack.push(left);
                *ip += 1;
            }

            // -- Pair (native) --
            OpCode::MakePair => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let key = left.to_string_value();
                self.stack.push(Value::Pair(key, Box::new(right)));
                *ip += 1;
            }

            // -- Bitwise (native) --
            OpCode::BitAnd => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a & b),
                    _ => Value::Int(0),
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::BitOr => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a | b),
                    _ => Value::Int(0),
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::BitXor => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a ^ b),
                    _ => Value::Int(0),
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::BitShiftLeft => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a << b),
                    _ => Value::Int(0),
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::BitShiftRight => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a >> b),
                    _ => Value::Int(0),
                };
                self.stack.push(result);
                *ip += 1;
            }

            // -- Set operations (native) --
            OpCode::SetElem => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let key = left.to_string_value();
                let result = match &right {
                    Value::Set(s) => s.contains(&key),
                    Value::Bag(b) => b.contains_key(&key),
                    Value::Mix(m) => m.contains_key(&key),
                    _ => false,
                };
                self.stack.push(Value::Bool(result));
                *ip += 1;
            }
            OpCode::SetCont => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let key = right.to_string_value();
                let result = match &left {
                    Value::Set(s) => s.contains(&key),
                    Value::Bag(b) => b.contains_key(&key),
                    Value::Mix(m) => m.contains_key(&key),
                    _ => false,
                };
                self.stack.push(Value::Bool(result));
                *ip += 1;
            }
            OpCode::SetUnion => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Set(mut a), Value::Set(b)) => {
                        for elem in b {
                            a.insert(elem);
                        }
                        Value::Set(a)
                    }
                    (Value::Bag(mut a), Value::Bag(b)) => {
                        for (k, v) in b {
                            let e = a.entry(k).or_insert(0);
                            *e = (*e).max(v);
                        }
                        Value::Bag(a)
                    }
                    (Value::Mix(mut a), Value::Mix(b)) => {
                        for (k, v) in b {
                            let e = a.entry(k).or_insert(0.0);
                            *e = e.max(v);
                        }
                        Value::Mix(a)
                    }
                    (Value::Set(a), Value::Bag(mut b)) => {
                        for elem in a {
                            b.entry(elem).or_insert(1);
                        }
                        Value::Bag(b)
                    }
                    (Value::Bag(mut a), Value::Set(b)) => {
                        for elem in b {
                            a.entry(elem).or_insert(1);
                        }
                        Value::Bag(a)
                    }
                    (l, r) => {
                        let a = Interpreter::coerce_to_set(&l);
                        let b = Interpreter::coerce_to_set(&r);
                        let mut result = a;
                        for elem in b {
                            result.insert(elem);
                        }
                        Value::Set(result)
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::SetIntersect => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Set(a), Value::Set(b)) => {
                        Value::Set(a.intersection(&b).cloned().collect())
                    }
                    (Value::Bag(a), Value::Bag(b)) => {
                        let mut result = HashMap::new();
                        for (k, v) in &a {
                            if let Some(bv) = b.get(k) {
                                result.insert(k.clone(), (*v).min(*bv));
                            }
                        }
                        Value::Bag(result)
                    }
                    (Value::Mix(a), Value::Mix(b)) => {
                        let mut result = HashMap::new();
                        for (k, v) in &a {
                            if let Some(bv) = b.get(k) {
                                result.insert(k.clone(), v.min(*bv));
                            }
                        }
                        Value::Mix(result)
                    }
                    (l, r) => {
                        let a = Interpreter::coerce_to_set(&l);
                        let b = Interpreter::coerce_to_set(&r);
                        Value::Set(a.intersection(&b).cloned().collect())
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::SetDiff => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Set(a), Value::Set(b)) => {
                        Value::Set(a.difference(&b).cloned().collect())
                    }
                    (Value::Bag(a), Value::Bag(b)) => {
                        let mut result = HashMap::new();
                        for (k, v) in a {
                            let bv = b.get(&k).copied().unwrap_or(0);
                            if v > bv {
                                result.insert(k, v - bv);
                            }
                        }
                        Value::Bag(result)
                    }
                    (Value::Mix(a), Value::Mix(b)) => {
                        let mut result = HashMap::new();
                        for (k, v) in a {
                            let bv = b.get(&k).copied().unwrap_or(0.0);
                            if v > bv {
                                result.insert(k, v - bv);
                            }
                        }
                        Value::Mix(result)
                    }
                    (l, r) => {
                        let a = Interpreter::coerce_to_set(&l);
                        let b = Interpreter::coerce_to_set(&r);
                        Value::Set(a.difference(&b).cloned().collect())
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::SetSymDiff => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Set(a), Value::Set(b)) => {
                        Value::Set(a.symmetric_difference(&b).cloned().collect())
                    }
                    (l, r) => {
                        let a = Interpreter::coerce_to_set(&l);
                        let b = Interpreter::coerce_to_set(&r);
                        Value::Set(a.symmetric_difference(&b).cloned().collect())
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::SetSubset => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let a = Interpreter::coerce_to_set(&left);
                let b = Interpreter::coerce_to_set(&right);
                self.stack.push(Value::Bool(a.is_subset(&b)));
                *ip += 1;
            }
            OpCode::SetSuperset => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let a = Interpreter::coerce_to_set(&left);
                let b = Interpreter::coerce_to_set(&right);
                self.stack.push(Value::Bool(a.is_superset(&b)));
                *ip += 1;
            }
            OpCode::SetStrictSubset => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let a = Interpreter::coerce_to_set(&left);
                let b = Interpreter::coerce_to_set(&right);
                self.stack
                    .push(Value::Bool(a.is_subset(&b) && a.len() < b.len()));
                *ip += 1;
            }
            OpCode::SetStrictSuperset => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let a = Interpreter::coerce_to_set(&left);
                let b = Interpreter::coerce_to_set(&right);
                self.stack
                    .push(Value::Bool(a.is_superset(&b) && a.len() > b.len()));
                *ip += 1;
            }
            OpCode::JunctionAny => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack
                    .push(Interpreter::merge_junction(JunctionKind::Any, left, right));
                *ip += 1;
            }
            OpCode::JunctionAll => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack
                    .push(Interpreter::merge_junction(JunctionKind::All, left, right));
                *ip += 1;
            }
            OpCode::JunctionOne => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                self.stack
                    .push(Interpreter::merge_junction(JunctionKind::One, left, right));
                *ip += 1;
            }

            // -- Sequence (...) / (...^) --
            OpCode::Sequence { exclude_end } => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let op = if *exclude_end {
                    crate::lexer::TokenKind::DotDotDotCaret
                } else {
                    crate::lexer::TokenKind::DotDotDot
                };
                let out = self.interpreter.eval_binary(left, &op, right)?;
                self.stack.push(out);
                self.sync_locals_from_env(code);
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

            // -- Range creation (native) --
            OpCode::MakeRange => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => Value::Range(*a, *b),
                    (Value::Str(a), Value::Str(b)) if a.len() == 1 && b.len() == 1 => {
                        let start = a.chars().next().unwrap();
                        let end = b.chars().next().unwrap();
                        let items: Vec<Value> = (start as u32..=end as u32)
                            .filter_map(char::from_u32)
                            .map(|c| Value::Str(c.to_string()))
                            .collect();
                        Value::Array(items)
                    }
                    _ => Value::Nil,
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::MakeRangeExcl => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Value::RangeExcl(a, b),
                    _ => Value::Nil,
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::MakeRangeExclStart => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Value::RangeExclStart(a, b),
                    _ => Value::Nil,
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::MakeRangeExclBoth => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = match (left, right) {
                    (Value::Int(a), Value::Int(b)) => Value::RangeExclBoth(a, b),
                    _ => Value::Nil,
                };
                self.stack.push(result);
                *ip += 1;
            }

            // -- Composite --
            OpCode::MakeArray(n) => {
                let n = *n as usize;
                let start = self.stack.len() - n;
                let raw: Vec<Value> = self.stack.drain(start..).collect();
                // Flatten Slips (including Empty) in list context
                let mut elems = Vec::with_capacity(raw.len());
                for val in raw {
                    match val {
                        Value::Slip(items) => elems.extend(items),
                        other => elems.push(other),
                    }
                }
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
                    parts.push(Interpreter::gist_value(v));
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
                if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
                    let pkg = self.interpreter.current_package().to_string();
                    let result =
                        self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
                    self.stack.push(result);
                    self.sync_locals_from_env(code);
                } else if let Some(native_result) = Self::try_native_function(&name, &args) {
                    self.stack.push(native_result?);
                } else {
                    let result = self.interpreter.eval_call_with_values(&name, args)?;
                    self.stack.push(result);
                    self.sync_locals_from_env(code);
                }
                *ip += 1;
            }
            OpCode::CallMethod { name_idx, arity } => {
                let method = Self::const_str(code, *name_idx).to_string();
                let arity = *arity as usize;
                let start = self.stack.len() - arity;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                let target = self.stack.pop().unwrap();
                if let Some(native_result) = Self::try_native_method(&target, &method, &args) {
                    self.stack.push(native_result?);
                } else {
                    let result = self
                        .interpreter
                        .eval_method_call_with_values(target, &method, args)?;
                    self.stack.push(result);
                    self.sync_locals_from_env(code);
                }
                *ip += 1;
            }
            OpCode::CallMethodMut {
                name_idx,
                arity,
                target_name_idx,
            } => {
                let method = Self::const_str(code, *name_idx).to_string();
                let target_name = Self::const_str(code, *target_name_idx).to_string();
                let arity = *arity as usize;
                let start = self.stack.len() - arity;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                let target = self.stack.pop().unwrap();
                // Try native dispatch first (non-mutating methods only)
                if let Some(native_result) = Self::try_native_method(&target, &method, &args) {
                    self.stack.push(native_result?);
                } else {
                    // Fall back to interpreter bridge (may mutate target)
                    let result = self.interpreter.eval_method_call_mut_with_values(
                        &target_name,
                        &method,
                        args,
                    )?;
                    self.stack.push(result);
                    self.sync_locals_from_env(code);
                }
                *ip += 1;
            }
            OpCode::ExecCall { name_idx, arity } => {
                let name = Self::const_str(code, *name_idx).to_string();
                let arity = *arity as usize;
                let start = self.stack.len() - arity;
                let args: Vec<Value> = self.stack.drain(start..).collect();
                if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
                    let pkg = self.interpreter.current_package().to_string();
                    let _result =
                        self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
                    self.sync_locals_from_env(code);
                } else if let Some(native_result) = Self::try_native_function(&name, &args) {
                    native_result?;
                } else {
                    self.interpreter.exec_call_with_values(&name, args)?;
                    self.sync_locals_from_env(code);
                }
                *ip += 1;
            }
            OpCode::ExecCallMixed(stmt_idx) => {
                let stmt = &code.stmt_pool[*stmt_idx as usize];
                if let Stmt::Call { name, args } = stmt {
                    let mut rebuilt_rev = Vec::with_capacity(args.len());
                    for arg in args.iter().rev() {
                        match arg {
                            CallArg::Positional(expr) => {
                                if Self::call_arg_needs_raw_expr(expr) {
                                    rebuilt_rev.push(CallArg::Positional(expr.clone()));
                                } else {
                                    let value = self.stack.pop().unwrap_or(Value::Nil);
                                    rebuilt_rev.push(CallArg::Positional(Expr::Literal(value)));
                                }
                            }
                            CallArg::Named { name, value } => {
                                let rebuilt = if value.is_some() {
                                    if let Some(expr) = value {
                                        if Self::call_arg_needs_raw_expr(expr) {
                                            CallArg::Named {
                                                name: name.clone(),
                                                value: Some(expr.clone()),
                                            }
                                        } else {
                                            let v = self.stack.pop().unwrap_or(Value::Nil);
                                            CallArg::Named {
                                                name: name.clone(),
                                                value: Some(Expr::Literal(v)),
                                            }
                                        }
                                    } else {
                                        unreachable!()
                                    }
                                } else {
                                    CallArg::Named {
                                        name: name.clone(),
                                        value: None,
                                    }
                                };
                                rebuilt_rev.push(rebuilt);
                            }
                        }
                    }
                    rebuilt_rev.reverse();
                    self.interpreter
                        .exec_call_with_call_args(name, rebuilt_rev)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("ExecCallMixed expects Call"));
                }
            }

            // -- Indexing --
            OpCode::Index => {
                let index = self.stack.pop().unwrap();
                let mut target = self.stack.pop().unwrap();
                if let Value::LazyList(ref ll) = target {
                    target = Value::Array(self.interpreter.force_lazy_list_bridge(ll)?);
                }
                let result = match (target, index) {
                    (Value::Array(items), Value::Int(i)) => {
                        if i < 0 {
                            Value::Nil
                        } else {
                            items.get(i as usize).cloned().unwrap_or(Value::Nil)
                        }
                    }
                    (Value::Array(items), Value::Range(a, b)) => {
                        let start = a.max(0) as usize;
                        let end = b.max(-1) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end = end.min(items.len().saturating_sub(1));
                            items[start..=end].to_vec()
                        };
                        Value::Array(slice)
                    }
                    (Value::Array(items), Value::RangeExcl(a, b)) => {
                        let start = a.max(0) as usize;
                        let end_excl = b.max(0) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end_excl = end_excl.min(items.len());
                            if start >= end_excl {
                                Vec::new()
                            } else {
                                items[start..end_excl].to_vec()
                            }
                        };
                        Value::Array(slice)
                    }
                    (Value::Hash(items), Value::Num(f)) if f.is_infinite() && f > 0.0 => {
                        // Whatever slice: %h{*} returns all values
                        Value::Array(items.values().cloned().collect())
                    }
                    (Value::Hash(items), Value::Nil) => {
                        // Zen slice: %h{} returns the hash itself
                        Value::Hash(items)
                    }
                    (Value::Hash(items), Value::Array(keys)) => {
                        // Hash slicing: %hash{"one", "three"} returns array of values
                        Value::Array(
                            keys.iter()
                                .map(|k| {
                                    let key = k.to_string_value();
                                    items.get(&key).cloned().unwrap_or(Value::Nil)
                                })
                                .collect(),
                        )
                    }
                    (Value::Hash(items), Value::Str(key)) => {
                        items.get(&key).cloned().unwrap_or(Value::Nil)
                    }
                    (Value::Hash(items), Value::Int(key)) => {
                        items.get(&key.to_string()).cloned().unwrap_or(Value::Nil)
                    }
                    (Value::Set(s), Value::Str(key)) => Value::Bool(s.contains(&key)),
                    (Value::Set(s), idx) => Value::Bool(s.contains(&idx.to_string_value())),
                    (Value::Bag(b), Value::Str(key)) => Value::Int(*b.get(&key).unwrap_or(&0)),
                    (Value::Bag(b), idx) => {
                        Value::Int(*b.get(&idx.to_string_value()).unwrap_or(&0))
                    }
                    (Value::Mix(m), Value::Str(key)) => Value::Num(*m.get(&key).unwrap_or(&0.0)),
                    (Value::Mix(m), idx) => {
                        Value::Num(*m.get(&idx.to_string_value()).unwrap_or(&0.0))
                    }
                    _ => Value::Nil,
                };
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
            OpCode::Redo(label) => {
                let mut sig = RuntimeError::redo_signal();
                sig.label = label.clone();
                return Err(sig);
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
                let val = self
                    .interpreter
                    .env()
                    .get(name)
                    .cloned()
                    .unwrap_or(Value::Int(0));
                let new_val = match &val {
                    Value::Int(i) => Value::Int(i + 1),
                    Value::Rat(n, d) => make_rat(n + d, *d),
                    _ => Value::Int(1),
                };
                self.interpreter
                    .env_mut()
                    .insert(name.to_string(), new_val.clone());
                self.update_local_if_exists(code, name, &new_val);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::PostDecrement(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .interpreter
                    .env()
                    .get(name)
                    .cloned()
                    .unwrap_or(Value::Int(0));
                let new_val = match &val {
                    Value::Int(i) => Value::Int(i - 1),
                    Value::Rat(n, d) => make_rat(n - d, *d),
                    _ => Value::Int(-1),
                };
                self.interpreter
                    .env_mut()
                    .insert(name.to_string(), new_val.clone());
                self.update_local_if_exists(code, name, &new_val);
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
                let val = self
                    .interpreter
                    .env()
                    .get(name)
                    .cloned()
                    .unwrap_or(Value::Int(0));
                let new_val = match val {
                    Value::Int(i) => Value::Int(i + 1),
                    Value::Rat(n, d) => make_rat(n + d, d),
                    _ => Value::Int(1),
                };
                self.interpreter
                    .env_mut()
                    .insert(name.to_string(), new_val.clone());
                self.update_local_if_exists(code, name, &new_val);
                self.stack.push(new_val);
                *ip += 1;
            }
            OpCode::PreDecrement(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .interpreter
                    .env()
                    .get(name)
                    .cloned()
                    .unwrap_or(Value::Int(0));
                let new_val = match val {
                    Value::Int(i) => Value::Int(i - 1),
                    Value::Rat(n, d) => make_rat(n - d, d),
                    _ => Value::Int(-1),
                };
                self.interpreter
                    .env_mut()
                    .insert(name.to_string(), new_val.clone());
                self.update_local_if_exists(code, name, &new_val);
                self.stack.push(new_val);
                *ip += 1;
            }

            // -- Variable access --
            OpCode::GetCaptureVar(name_idx) => {
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
                self.update_local_if_exists(code, &name, &val);
                self.interpreter.env_mut().insert(name, val);
                *ip += 1;
            }

            // -- Loops --
            OpCode::WhileLoop {
                cond_end,
                body_end,
                label,
            } => {
                let cond_start = *ip + 1;
                let body_start = *cond_end as usize;
                let loop_end = *body_end as usize;
                let label = label.clone();

                'while_loop: loop {
                    // Evaluate condition
                    self.run_range(code, cond_start, body_start, compiled_fns)?;
                    let cond_val = self.stack.pop().unwrap();
                    if !cond_val.truthy() {
                        break;
                    }
                    // Execute body with redo support
                    'body_redo: loop {
                        match self.run_range(code, body_start, loop_end, compiled_fns) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => {
                                continue 'body_redo;
                            }
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => {
                                break 'while_loop;
                            }
                            Err(e) if e.is_next && Self::label_matches(&e.label, &label) => {
                                break 'body_redo;
                            }
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
                param_local,
                body_end,
                label,
            } => {
                let iterable = self.stack.pop().unwrap();
                let items = if let Value::LazyList(ref ll) = iterable {
                    self.interpreter.force_lazy_list_bridge(ll)?
                } else {
                    Interpreter::value_to_list(&iterable)
                };
                self.sync_locals_from_env(code);
                let body_start = *ip + 1;
                let loop_end = *body_end as usize;
                let label = label.clone();
                let param_local = *param_local;

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
                            .insert(name.clone(), item.clone());
                    }
                    if let Some(slot) = param_local {
                        self.locals[slot as usize] = item.clone();
                    }
                    'body_redo: loop {
                        match self.run_range(code, body_start, loop_end, compiled_fns) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => {
                                // Restore loop variable before re-executing body
                                self.interpreter
                                    .env_mut()
                                    .insert("_".to_string(), item.clone());
                                if let Some(ref name) = param_name {
                                    self.interpreter
                                        .env_mut()
                                        .insert(name.clone(), item.clone());
                                }
                                if let Some(slot) = param_local {
                                    self.locals[slot as usize] = item.clone();
                                }
                                continue 'body_redo;
                            }
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => {
                                break 'for_loop;
                            }
                            Err(e) if e.is_next && Self::label_matches(&e.label, &label) => {
                                break 'body_redo;
                            }
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
                    self.run_range(code, cond_start, body_start, compiled_fns)?;
                    let cond_val = self.stack.pop().unwrap();
                    if !cond_val.truthy() {
                        break;
                    }
                    // Execute body with redo support
                    'body_redo: loop {
                        match self.run_range(code, body_start, step_begin, compiled_fns) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => {
                                continue 'body_redo;
                            }
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => {
                                break 'c_loop;
                            }
                            Err(e) if e.is_next && Self::label_matches(&e.label, &label) => {
                                break 'body_redo;
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    if self.interpreter.is_halted() {
                        break;
                    }
                    // Execute step
                    self.run_range(code, step_begin, loop_end, compiled_fns)?;
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
                    if let Err(e) = self.exec_one(code, &mut inner_ip, compiled_fns) {
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

                let topic = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                if self.interpreter.smart_match_values(&topic, &cond_val) {
                    let mut did_proceed = false;
                    match self.run_range(code, body_start, end, compiled_fns) {
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
                self.run_range(code, body_start, end, compiled_fns)?;
                self.interpreter.set_when_matched(true);
                *ip = end;
            }

            // -- Repeat loop --
            OpCode::RepeatLoop {
                cond_end,
                body_end,
                label,
            } => {
                let body_start = *ip + 1;
                let cond_start = *cond_end as usize;
                let loop_end = *body_end as usize;
                let label = label.clone();

                let mut first = true;
                'repeat_loop: loop {
                    if !first {
                        // Evaluate condition
                        self.run_range(code, cond_start, loop_end, compiled_fns)?;
                        let cond_val = self.stack.pop().unwrap();
                        if !cond_val.truthy() {
                            break;
                        }
                    }
                    first = false;
                    // Execute body with redo support
                    'body_redo: loop {
                        match self.run_range(code, body_start, cond_start, compiled_fns) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => {
                                continue 'body_redo;
                            }
                            Err(e) if e.is_last && Self::label_matches(&e.label, &label) => {
                                break 'repeat_loop;
                            }
                            Err(e) if e.is_next && Self::label_matches(&e.label, &label) => {
                                break 'body_redo;
                            }
                            Err(e) => return Err(e),
                        }
                    }
                    if self.interpreter.is_halted() {
                        break;
                    }
                }
                *ip = loop_end;
            }

            // -- Exception handling (try/catch) --
            OpCode::TryCatch {
                catch_start,
                body_end,
            } => {
                let saved_depth = self.stack.len();
                let body_start = *ip + 1;
                let catch_begin = *catch_start as usize;
                let end = *body_end as usize;
                match self.run_range(code, body_start, catch_begin, compiled_fns) {
                    Ok(()) => {
                        // Success — body result is on stack, skip catch
                        *ip = end;
                    }
                    Err(e) if e.return_value.is_some() => return Err(e),
                    Err(e)
                        if e.is_last || e.is_next || e.is_redo || e.is_proceed || e.is_succeed =>
                    {
                        return Err(e);
                    }
                    Err(e) => {
                        // Error caught
                        self.stack.truncate(saved_depth);
                        // Set $! to an Exception instance so that $! ~~ Exception works
                        let mut exc_attrs = std::collections::HashMap::new();
                        exc_attrs.insert("message".to_string(), Value::Str(e.message.clone()));
                        let err_val = Value::make_instance("Exception".to_string(), exc_attrs);
                        let saved_topic = self.interpreter.env().get("_").cloned();
                        self.interpreter
                            .env_mut()
                            .insert("!".to_string(), err_val.clone());
                        self.interpreter.env_mut().insert("_".to_string(), err_val);
                        // Run catch block
                        let saved_when = self.interpreter.when_matched();
                        self.interpreter.set_when_matched(false);
                        self.run_range(code, catch_begin, end, compiled_fns)?;
                        self.interpreter.set_when_matched(saved_when);
                        // Restore $_ but leave $! set (try semantics: $! persists after try)
                        if let Some(v) = saved_topic {
                            self.interpreter.env_mut().insert("_".to_string(), v);
                        } else {
                            self.interpreter.env_mut().remove("_");
                        }
                        // catch result is Nil (pushed by compiler)
                        *ip = end;
                    }
                }
            }

            // -- Error handling --
            OpCode::Die => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                return Err(RuntimeError::new(val.to_string_value()));
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
                self.stack
                    .push(Value::Bool(std::env::var_os(key).is_some()));
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
                let list = if let Value::LazyList(ref ll) = list_value {
                    self.interpreter.force_lazy_list_bridge(ll)?
                } else {
                    Interpreter::value_to_list(&list_value)
                };
                if list.is_empty() {
                    self.stack.push(Interpreter::reduction_identity(&op));
                } else {
                    let is_comparison = matches!(
                        op.as_str(),
                        "eq" | "ne"
                            | "lt"
                            | "gt"
                            | "le"
                            | "ge"
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
                        // For comparison operators, check all adjacent pairs
                        let mut result = true;
                        for i in 0..list.len() - 1 {
                            let v = Interpreter::apply_reduction_op(&op, &list[i], &list[i + 1])?;
                            if !v.truthy() {
                                result = false;
                                break;
                            }
                        }
                        self.stack.push(Value::Bool(result));
                    } else {
                        let mut acc = list[0].clone();
                        for item in &list[1..] {
                            acc = Interpreter::apply_reduction_op(&op, &acc, item)?;
                        }
                        self.stack.push(acc);
                    }
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
            OpCode::Subst {
                pattern_idx,
                replacement_idx,
            } => {
                let pattern = Self::const_str(code, *pattern_idx).to_string();
                let replacement = Self::const_str(code, *replacement_idx).to_string();
                let target = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                let text = target.to_string_value();
                if let Some((start, end)) =
                    self.interpreter.regex_find_first_bridge(&pattern, &text)
                {
                    let start_b = Interpreter::char_idx_to_byte(&text, start);
                    let end_b = Interpreter::char_idx_to_byte(&text, end);
                    let mut out = String::new();
                    out.push_str(&text[..start_b]);
                    out.push_str(&replacement);
                    out.push_str(&text[end_b..]);
                    let result = Value::Str(out);
                    self.interpreter
                        .env_mut()
                        .insert("_".to_string(), result.clone());
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
                self.run_range(code, *ip + 1, body_end, compiled_fns)?;
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

            // -- HyperOp (>>op<<) --
            OpCode::HyperOp {
                op_idx,
                dwim_left,
                dwim_right,
            } => {
                let right = self.stack.pop().unwrap_or(Value::Nil);
                let left = self.stack.pop().unwrap_or(Value::Nil);
                let op = Self::const_str(code, *op_idx);
                let result =
                    Interpreter::eval_hyper_op(op, &left, &right, *dwim_left, *dwim_right)?;
                self.stack.push(result);
                *ip += 1;
            }

            // -- MetaOp (Rop, Xop, Zop) --
            OpCode::MetaOp { meta_idx, op_idx } => {
                let right = self.stack.pop().unwrap_or(Value::Nil);
                let left = self.stack.pop().unwrap_or(Value::Nil);
                let meta = Self::const_str(code, *meta_idx).to_string();
                let op = Self::const_str(code, *op_idx).to_string();
                let result = match meta.as_str() {
                    "R" => Interpreter::apply_reduction_op(&op, &right, &left)?,
                    "X" => {
                        let left_list = Interpreter::value_to_list(&left);
                        let right_list = Interpreter::value_to_list(&right);
                        let mut results = Vec::new();
                        for l in &left_list {
                            for r in &right_list {
                                results.push(Interpreter::apply_reduction_op(&op, l, r)?);
                            }
                        }
                        Value::Array(results)
                    }
                    "Z" => {
                        let left_list = Interpreter::value_to_list(&left);
                        let right_list = Interpreter::value_to_list(&right);
                        let len = left_list.len().min(right_list.len());
                        let mut results = Vec::new();
                        if op.is_empty() || op == "," {
                            for i in 0..len {
                                results.push(Value::Array(vec![
                                    left_list[i].clone(),
                                    right_list[i].clone(),
                                ]));
                            }
                        } else if op == "=>" {
                            for i in 0..len {
                                let key = left_list[i].to_string_value();
                                results.push(Value::Pair(key, Box::new(right_list[i].clone())));
                            }
                        } else {
                            for i in 0..len {
                                results.push(Interpreter::apply_reduction_op(
                                    &op,
                                    &left_list[i],
                                    &right_list[i],
                                )?);
                            }
                        }
                        Value::Array(results)
                    }
                    _ => {
                        return Err(RuntimeError::new(format!(
                            "Unknown meta operator: {}",
                            meta
                        )));
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }

            // -- InfixFunc (atan2, sprintf) --
            OpCode::InfixFunc {
                name_idx,
                right_arity,
                modifier_idx,
            } => {
                let arity = *right_arity as usize;
                let mut right_vals: Vec<Value> = Vec::with_capacity(arity);
                for _ in 0..arity {
                    right_vals.push(self.stack.pop().unwrap_or(Value::Nil));
                }
                right_vals.reverse();
                let left_val = self.stack.pop().unwrap_or(Value::Nil);
                let name = Self::const_str(code, *name_idx).to_string();
                let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
                let result = if name == "atan2" {
                    let mut x = right_vals
                        .first()
                        .and_then(Interpreter::to_float_value)
                        .unwrap_or(0.0);
                    let mut y = Interpreter::to_float_value(&left_val).unwrap_or(0.0);
                    if modifier.as_deref() == Some("R") {
                        std::mem::swap(&mut x, &mut y);
                    }
                    Value::Num(y.atan2(x))
                } else if name == "sprintf" {
                    let fmt = match left_val {
                        Value::Str(s) => s,
                        _ => String::new(),
                    };
                    if modifier.as_deref() == Some("X") {
                        let mut parts = Vec::new();
                        for val in &right_vals {
                            parts.push(Interpreter::format_sprintf(&fmt, Some(val)));
                        }
                        Value::Str(parts.join(" "))
                    } else {
                        let arg = right_vals.first();
                        let rendered = Interpreter::format_sprintf(&fmt, arg);
                        Value::Str(rendered)
                    }
                } else {
                    Value::Nil
                };
                self.stack.push(result);
                *ip += 1;
            }

            // -- Type checking --
            OpCode::TypeCheck(tc_idx) => {
                let constraint = Self::const_str(code, *tc_idx);
                let value = self.stack.last().expect("TypeCheck: empty stack");
                if !matches!(value, Value::Nil)
                    && Interpreter::is_known_type_constraint(constraint)
                    && !self.interpreter.type_matches_value(constraint, value)
                {
                    return Err(RuntimeError::new("X::Syntax::Number::LiteralType"));
                }
                *ip += 1;
            }

            OpCode::RunWhileStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::While { cond, body, label } = stmt {
                    self.interpreter.run_while_stmt(cond, body, label)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunWhileStmt expects While"));
                }
            }
            OpCode::RunForStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::For {
                    iterable,
                    param,
                    params,
                    body,
                    label,
                } = stmt
                {
                    self.interpreter
                        .run_for_stmt(iterable, param, params, body, label)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunForStmt expects For"));
                }
            }
            OpCode::RunLoopStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Loop {
                    init,
                    cond,
                    step,
                    body,
                    repeat,
                    label,
                } = stmt
                {
                    self.interpreter.run_loop_stmt(
                        init.as_deref(),
                        cond.as_ref(),
                        step.as_ref(),
                        body,
                        *repeat,
                        label,
                    )?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunLoopStmt expects Loop"));
                }
            }
            OpCode::RunReactStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::React { body } = stmt {
                    self.interpreter.run_react_stmt(body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunReactStmt expects React"));
                }
            }
            OpCode::RunPackageStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Package { name, body } = stmt {
                    self.interpreter.run_package_stmt(name, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunPackageStmt expects Package"));
                }
            }
            OpCode::RunGivenStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Given { topic, body } = stmt {
                    self.interpreter.run_given_stmt(topic, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunGivenStmt expects Given"));
                }
            }
            OpCode::RunWhenStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::When { cond, body } = stmt {
                    self.interpreter.run_when_stmt(cond, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunWhenStmt expects When"));
                }
            }
            OpCode::RunDefaultStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Default(body) = stmt {
                    self.interpreter.run_default_stmt(body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunDefaultStmt expects Default"));
                }
            }
            OpCode::RunBlockStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Block(body) = stmt {
                    self.interpreter.run_block_stmt(body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunBlockStmt expects Block"));
                }
            }
            OpCode::RunDoBlockExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::DoBlock { body, label } = expr {
                    let val = self.interpreter.eval_do_block_expr(body, label)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunDoBlockExpr expects DoBlock"));
                }
            }
            OpCode::RunDoStmtExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::DoStmt(stmt) = expr {
                    let val = self.interpreter.eval_do_stmt_expr(stmt)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunDoStmtExpr expects DoStmt"));
                }
            }
            OpCode::RunGatherExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::Gather(body) = expr {
                    let val = self.interpreter.eval_gather_expr(body);
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunGatherExpr expects Gather"));
                }
            }
            OpCode::RunCallOnExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::CallOn { target, args } = expr {
                    let val = self.interpreter.eval_call_on_expr(target, args)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunCallOnExpr expects CallOn"));
                }
            }
            OpCode::RunAnonSubExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::AnonSub(body) = expr {
                    let val = self.interpreter.eval_anon_sub_expr(body);
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunAnonSubExpr expects AnonSub"));
                }
            }
            OpCode::RunAnonSubParamsExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::AnonSubParams { params, body } = expr {
                    let val = self.interpreter.eval_anon_sub_params_expr(params, body);
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new(
                        "RunAnonSubParamsExpr expects AnonSubParams",
                    ));
                }
            }
            OpCode::RunLambdaExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::Lambda { param, body } = expr {
                    let val = self.interpreter.eval_lambda_expr(param, body);
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunLambdaExpr expects Lambda"));
                }
            }
            OpCode::RunIndexAssignExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::IndexAssign {
                    target,
                    index,
                    value,
                } = expr
                {
                    let val = self
                        .interpreter
                        .eval_index_assign_expr(target, index, value)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunIndexAssignExpr expects IndexAssign"));
                }
            }
            OpCode::RunPostfixExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::PostfixOp { op, expr } = expr {
                    let val = self.interpreter.eval_postfix_expr(op, expr)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunPostfixExpr expects PostfixOp"));
                }
            }
            OpCode::RunTryExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::Try { body, catch } = expr {
                    let val = self.interpreter.eval_try_expr(body, catch)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunTryExpr expects Try"));
                }
            }
            OpCode::RunBlockExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::Block(body) = expr {
                    let val = self.interpreter.eval_block_expr(body)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunBlockExpr expects Block"));
                }
            }
            OpCode::RunUnaryExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                if let Expr::Unary { op, expr } = expr {
                    let val = self.interpreter.eval_unary_expr(op, expr)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunUnaryExpr expects Unary"));
                }
            }
            OpCode::RunBinaryToken(token_idx) => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let op = &code.token_pool[*token_idx as usize];
                let val = self.interpreter.eval_binary(left, op, right)?;
                self.stack.push(val);
                self.sync_locals_from_env(code);
                *ip += 1;
            }
            OpCode::RegisterSub(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::SubDecl {
                    name,
                    params,
                    param_defs,
                    body,
                    multi,
                } = stmt
                {
                    self.interpreter
                        .register_sub_decl(name, params, param_defs, body, *multi);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RegisterSub expects SubDecl"));
                }
            }
            OpCode::RegisterToken(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                match stmt {
                    Stmt::TokenDecl {
                        name,
                        params,
                        param_defs,
                        body,
                        multi,
                    }
                    | Stmt::RuleDecl {
                        name,
                        params,
                        param_defs,
                        body,
                        multi,
                    } => {
                        self.interpreter
                            .register_token_decl(name, params, param_defs, body, *multi);
                        self.sync_locals_from_env(code);
                        *ip += 1;
                    }
                    _ => {
                        return Err(RuntimeError::new(
                            "RegisterToken expects TokenDecl/RuleDecl",
                        ));
                    }
                }
            }
            OpCode::RegisterProtoSub(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::ProtoDecl { name, .. } = stmt {
                    self.interpreter.register_proto_decl(name);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RegisterProtoSub expects ProtoDecl"));
                }
            }
            OpCode::RegisterProtoToken(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::ProtoToken { name } = stmt {
                    self.interpreter.register_proto_token_decl(name);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RegisterProtoToken expects ProtoToken"));
                }
            }
            OpCode::UseModule(name_idx) => {
                let module = Self::const_str(code, *name_idx);
                self.interpreter.use_module(module)?;
                self.sync_locals_from_env(code);
                *ip += 1;
            }
            OpCode::UseLibPath => {
                let value = self.stack.pop().unwrap_or(Value::Nil);
                self.interpreter.add_lib_path(value.to_string_value());
                self.sync_locals_from_env(code);
                *ip += 1;
            }
            OpCode::RegisterEnum(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::EnumDecl { name, variants } = stmt {
                    self.interpreter.register_enum_decl(name, variants)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RegisterEnum expects EnumDecl"));
                }
            }
            OpCode::RegisterClass(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::ClassDecl {
                    name,
                    parents,
                    body,
                } = stmt
                {
                    self.interpreter.register_class_decl(name, parents, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RegisterClass expects ClassDecl"));
                }
            }
            OpCode::RegisterRole(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::RoleDecl { name, body } = stmt {
                    self.interpreter.register_role_decl(name, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RegisterRole expects RoleDecl"));
                }
            }
            OpCode::RegisterSubset(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::SubsetDecl {
                    name,
                    base,
                    predicate,
                } = stmt
                {
                    self.interpreter.register_subset_decl(name, base, predicate);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RegisterSubset expects SubsetDecl"));
                }
            }
            OpCode::RunSubtest(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Subtest { name, body } = stmt {
                    self.interpreter.run_subtest_stmt(name, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunSubtest expects Subtest"));
                }
            }
            OpCode::RunWhenever(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Whenever {
                    supply,
                    param,
                    body,
                } = stmt
                {
                    self.interpreter.run_whenever_stmt(supply, param, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("RunWhenever expects Whenever"));
                }
            }

            // -- Local variables (indexed slots) --
            OpCode::GetLocal(idx) => {
                self.stack.push(self.locals[*idx as usize].clone());
                *ip += 1;
            }
            OpCode::SetLocal(idx) => {
                let val = self.stack.pop().unwrap();
                let idx = *idx as usize;
                // Dual-write to env for interpreter bridge compatibility
                let name = &code.locals[idx];
                let val = if name.starts_with('@') {
                    if let Value::LazyList(ref list) = val {
                        Value::Array(self.interpreter.force_lazy_list_bridge(list)?)
                    } else {
                        val
                    }
                } else {
                    val
                };
                let val = if name.starts_with('%') {
                    Interpreter::coerce_to_hash(val)
                } else if name.starts_with('@') {
                    Interpreter::coerce_to_array(val)
                } else {
                    val
                };
                self.locals[idx] = val.clone();
                self.interpreter.env_mut().insert(name.clone(), val);
                *ip += 1;
            }
            // -- Assignment as expression for local variable --
            OpCode::AssignExprLocal(idx) => {
                let val = self.stack.last().unwrap().clone();
                let idx = *idx as usize;
                let name = &code.locals[idx];
                let val = if name.starts_with('%') {
                    Interpreter::coerce_to_hash(val)
                } else if name.starts_with('@') {
                    Interpreter::coerce_to_array(val)
                } else {
                    val
                };
                self.locals[idx] = val.clone();
                self.interpreter.env_mut().insert(name.clone(), val);
                *ip += 1;
            }
            OpCode::AssignReadOnly => {
                return Err(RuntimeError::new("X::Assignment::RO"));
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

    /// Junction auto-threading for binary comparison operators.
    fn eval_binary_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        f: fn(&mut Self, Value, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .into_iter()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::Junction {
                kind,
                values: results?,
            });
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .into_iter()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::Junction {
                kind,
                values: results?,
            });
        }
        f(self, left, right)
    }

    /// Sync local variable slots from the interpreter environment.
    /// Called after operations that may modify env (fallback execution, calls, etc.).
    fn sync_locals_from_env(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
    }

    /// Find the local slot index for a variable name, if it exists.
    fn find_local_slot(&self, code: &CompiledCode, name: &str) -> Option<usize> {
        code.locals.iter().position(|n| n == name)
    }

    /// Update a local slot if the variable name is a local.
    fn update_local_if_exists(&mut self, code: &CompiledCode, name: &str, val: &Value) {
        if let Some(slot) = self.find_local_slot(code, name) {
            self.locals[slot] = val.clone();
        }
    }

    /// Try to dispatch a method call natively (without interpreter bridge).
    /// Returns Some(result) on success, None if the method should fall through.
    fn try_native_method(
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.len() == 2 {
            return crate::builtins::native_method_2arg(target, method, &args[0], &args[1]);
        }
        if args.len() == 1 {
            return crate::builtins::native_method_1arg(target, method, &args[0]);
        }
        if !args.is_empty() {
            return None;
        }
        crate::builtins::native_method_0arg(target, method)
    }

    /// Try to dispatch a function call natively (pure built-in functions).
    fn try_native_function(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
        crate::builtins::native_function(name, args)
    }

    /// Look up a compiled function by name, trying multi-dispatch keys first.
    fn find_compiled_function<'a>(
        &self,
        compiled_fns: &'a HashMap<String, CompiledFunction>,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        let pkg = self.interpreter.current_package();
        let arity = args.len();
        // Try package::name/arity:types (multi-dispatch with types)
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| Interpreter::value_type_name(v).to_string())
            .collect();
        let key_typed = format!("{}::{}/{}:{}", pkg, name, arity, type_sig.join(","));
        if let Some(cf) = compiled_fns.get(&key_typed) {
            return Some(cf);
        }
        // Try package::name/arity (multi-dispatch without types)
        let key_arity = format!("{}::{}/{}", pkg, name, arity);
        if let Some(cf) = compiled_fns.get(&key_arity) {
            return Some(cf);
        }
        // Try package::name
        let key_simple = format!("{}::{}", pkg, name);
        if let Some(cf) = compiled_fns.get(&key_simple) {
            return Some(cf);
        }
        // Try GLOBAL::name
        if pkg != "GLOBAL" {
            let key_global = format!("GLOBAL::{}", name);
            if let Some(cf) = compiled_fns.get(&key_global) {
                return Some(cf);
            }
        }
        // Try fully qualified name (if name already contains ::)
        if name.contains("::") {
            compiled_fns.get(name)
        } else {
            None
        }
    }

    /// Execute a compiled function with the given arguments.
    fn call_compiled_function_named(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        // Save caller state
        let saved_env = self.interpreter.env().clone();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack_depth = self.stack.len();

        // Push to routine stack for &?ROUTINE support
        if !fn_name.is_empty() {
            self.interpreter
                .push_routine(fn_package.to_string(), fn_name.to_string());
        }

        // Bind arguments using interpreter's parameter binding
        self.interpreter
            .bind_function_args_values(&cf.param_defs, &cf.params, &args)?;

        // Initialize function locals from env
        self.locals = vec![Value::Nil; cf.code.locals.len()];
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }

        // Execute function body
        let mut ip = 0;
        let mut result = Ok(());
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    // Explicit return
                    let ret_val = e.return_value.unwrap();
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        // Extract return value from stack (implicit return)
        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        // Clean up stack
        self.stack.truncate(saved_stack_depth);

        // Pop routine stack
        if !fn_name.is_empty() {
            self.interpreter.pop_routine();
        }

        // Restore caller state
        self.locals = saved_locals;
        *self.interpreter.env_mut() = saved_env;

        match result {
            Ok(()) => Ok(ret_val),
            Err(e) => Err(e),
        }
    }
}
