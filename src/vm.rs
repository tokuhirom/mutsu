#![allow(clippy::result_large_err)]
use std::collections::HashMap;

use crate::ast::{CallArg, Expr, Stmt};
use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::value::{JunctionKind, LazyList, RuntimeError, Value, make_rat, next_instance_id};
use num_traits::{Signed, Zero};

mod vm_call_ops;
mod vm_data_ops;
mod vm_helpers;
mod vm_var_ops;

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
                        let result = self.interpreter.call_function(name, Vec::new())?;
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
            OpCode::SetTopic => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                self.interpreter.env_mut().insert("_".to_string(), val);
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
                let out = if *negate {
                    self.eval_binary_with_junctions(left, right, Self::not_smart_match_op)?
                } else {
                    self.eval_binary_with_junctions(left, right, Self::smart_match_op)?
                };
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
                let out = self
                    .interpreter
                    .eval_sequence_values(left, right, *exclude_end)?;
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
                self.exec_make_array_op(*n);
                *ip += 1;
            }
            OpCode::MakeHash(n) => {
                self.exec_make_hash_op(*n);
                *ip += 1;
            }

            // -- I/O --
            OpCode::Say(n) => {
                self.exec_say_op(*n)?;
                *ip += 1;
            }
            OpCode::Print(n) => {
                self.exec_print_op(*n)?;
                *ip += 1;
            }

            // -- Calls --
            OpCode::CallFunc { name_idx, arity } => {
                self.exec_call_func_op(code, *name_idx, *arity, compiled_fns)?;
                *ip += 1;
            }
            OpCode::CallMethod {
                name_idx,
                arity,
                modifier_idx,
            } => {
                self.exec_call_method_op(code, *name_idx, *arity, *modifier_idx)?;
                *ip += 1;
            }
            OpCode::CallMethodMut {
                name_idx,
                arity,
                target_name_idx,
                modifier_idx,
            } => {
                self.exec_call_method_mut_op(
                    code,
                    *name_idx,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                )?;
                *ip += 1;
            }
            OpCode::CallOnValue { arity } => {
                self.exec_call_on_value_op(code, *arity)?;
                *ip += 1;
            }
            OpCode::CallOnCodeVar { name_idx, arity } => {
                self.exec_call_on_code_var_op(code, *name_idx, *arity)?;
                *ip += 1;
            }
            OpCode::ExecCall { name_idx, arity } => {
                self.exec_exec_call_op(code, *name_idx, *arity, compiled_fns)?;
                *ip += 1;
            }
            OpCode::ExecCallMixed(stmt_idx) => {
                self.exec_exec_call_mixed_op(code, *stmt_idx)?;
                *ip += 1;
            }

            // -- Indexing --
            OpCode::Index => {
                self.exec_index_op()?;
                *ip += 1;
            }

            // -- String interpolation --
            OpCode::StringConcat(n) => {
                self.exec_string_concat_op(*n);
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
                self.exec_post_increment_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::PostDecrement(name_idx) => {
                self.exec_post_decrement_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::PostIncrementIndex(name_idx) => {
                self.exec_post_increment_index_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::PostDecrementIndex(name_idx) => {
                self.exec_post_decrement_index_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::IndexAssignExprNamed(name_idx) => {
                self.exec_index_assign_expr_named_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::IndexAssignExprNested(name_idx) => {
                self.exec_index_assign_expr_nested_op(code, *name_idx);
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
                        if e.is_succeed {
                            self.interpreter.set_when_matched(saved_when);
                            if let Some(v) = saved_topic.clone() {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            } else {
                                self.interpreter.env_mut().remove("_");
                            }
                            *ip = end;
                            break;
                        }
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
                            self.interpreter.set_when_matched(true);
                            return Err(e);
                        }
                        Err(e) => return Err(e),
                    }
                    if !did_proceed {
                        self.interpreter.set_when_matched(true);
                        let last = self
                            .interpreter
                            .env()
                            .get("_")
                            .cloned()
                            .unwrap_or(Value::Nil);
                        let mut sig = RuntimeError::succeed_signal();
                        sig.return_value = Some(last);
                        return Err(sig);
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
                control_start,
                body_end,
            } => {
                let saved_depth = self.stack.len();
                let body_start = *ip + 1;
                let catch_begin = *catch_start as usize;
                let control_begin = *control_start as usize;
                let end = *body_end as usize;
                match self.run_range(code, body_start, catch_begin, compiled_fns) {
                    Ok(()) => {
                        // Success — body result is on stack, skip catch
                        *ip = end;
                    }
                    Err(e) if e.return_value.is_some() => return Err(e),
                    Err(e)
                        if (e.is_last
                            || e.is_next
                            || e.is_redo
                            || e.is_proceed
                            || e.is_succeed)
                            && control_begin < end =>
                    {
                        // Control-flow signal handled by CONTROL block.
                        self.stack.truncate(saved_depth);
                        let saved_when = self.interpreter.when_matched();
                        self.interpreter.set_when_matched(false);
                        self.run_range(code, control_begin, end, compiled_fns)?;
                        self.interpreter.set_when_matched(saved_when);
                        *ip = end;
                    }
                    Err(e) => {
                        if catch_begin >= control_begin {
                            // No catch block; propagate.
                            return Err(e);
                        }
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
                        self.run_range(code, catch_begin, control_begin, compiled_fns)?;
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

            OpCode::BlockScope {
                enter_end,
                body_end,
                end,
            } => {
                let enter_start = *ip + 1;
                let body_start = *enter_end as usize;
                let leave_start = *body_end as usize;
                let end = *end as usize;

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
            }
            OpCode::DoBlockExpr { body_end, label } => {
                let body_start = *ip + 1;
                let end = *body_end as usize;
                let label = label.clone();
                loop {
                    match self.run_range(code, body_start, end, compiled_fns) {
                        Ok(()) => break,
                        Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => continue,
                        Err(e) if e.is_next && Self::label_matches(&e.label, &label) => {
                            self.stack.push(Value::Array(vec![]));
                            break;
                        }
                        Err(e) if e.is_last && Self::label_matches(&e.label, &label) => {
                            self.stack
                                .push(e.return_value.unwrap_or(Value::Array(vec![])));
                            break;
                        }
                        Err(e) => return Err(e),
                    }
                }
                *ip = end;
            }
            OpCode::DoGivenExpr(idx) => {
                let topic = self.stack.pop().unwrap_or(Value::Nil);
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Block(body) = stmt {
                    let val = self.interpreter.eval_given_with_value(topic, body)?;
                    self.stack.push(val);
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("DoGivenExpr expects Block"));
                }
            }
            OpCode::MakeGather(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Block(body) = stmt {
                    let list = LazyList {
                        body: body.clone(),
                        env: self.interpreter.env().clone(),
                        cache: std::cell::RefCell::new(None),
                    };
                    let val = Value::LazyList(std::rc::Rc::new(list));
                    self.stack.push(val);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("MakeGather expects Block"));
                }
            }
            OpCode::MakeAnonSub(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Block(body) = stmt {
                    let val = Value::Sub {
                        package: self.interpreter.current_package().to_string(),
                        name: String::new(),
                        params: vec![],
                        body: body.clone(),
                        env: self.interpreter.env().clone(),
                        id: next_instance_id(),
                    };
                    self.stack.push(val);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("MakeAnonSub expects Block"));
                }
            }
            OpCode::MakeAnonSubParams(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::SubDecl { params, body, .. } = stmt {
                    let val = Value::Sub {
                        package: self.interpreter.current_package().to_string(),
                        name: String::new(),
                        params: params.clone(),
                        body: body.clone(),
                        env: self.interpreter.env().clone(),
                        id: next_instance_id(),
                    };
                    self.stack.push(val);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("MakeAnonSubParams expects SubDecl"));
                }
            }
            OpCode::MakeLambda(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::SubDecl { params, body, .. } = stmt {
                    let val = Value::Sub {
                        package: self.interpreter.current_package().to_string(),
                        name: String::new(),
                        params: params.clone(),
                        body: body.clone(),
                        env: self.interpreter.env().clone(),
                        id: next_instance_id(),
                    };
                    self.stack.push(val);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("MakeLambda expects SubDecl"));
                }
            }
            OpCode::IndexAssignInvalid => {
                return Err(RuntimeError::new("Invalid assignment target"));
            }
            OpCode::MakeBlockClosure(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                if let Stmt::Block(body) = stmt {
                    let val = Value::Sub {
                        package: self.interpreter.current_package().to_string(),
                        name: String::new(),
                        params: vec![],
                        body: body.clone(),
                        env: self.interpreter.env().clone(),
                        id: next_instance_id(),
                    };
                    self.stack.push(val);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("MakeBlockClosure expects Block"));
                }
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
                if let Stmt::ProtoDecl {
                    name,
                    params,
                    param_defs,
                } = stmt
                {
                    let _ = (params.len(), param_defs.len());
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
            OpCode::SubtestScope { body_end } => {
                let end = *body_end as usize;
                let body_start = *ip + 1;
                let label = self.stack.pop().unwrap_or(Value::Nil).to_string_value();
                let ctx = self.interpreter.begin_subtest();
                let saved_depth = self.stack.len();
                let run_result = self.run_range(code, body_start, end, compiled_fns);
                self.stack.truncate(saved_depth);
                self.interpreter.finish_subtest(ctx, &label, run_result)?;
                self.sync_locals_from_env(code);
                *ip = end;
            }
            OpCode::WheneverScope {
                body_idx,
                param_idx,
                target_var_idx,
            } => {
                let supply_val = self.stack.pop().unwrap_or(Value::Nil);
                let param = param_idx.map(|idx| Self::const_str(code, idx).to_string());
                let target_var = target_var_idx.map(|idx| Self::const_str(code, idx));
                let stmt = &code.stmt_pool[*body_idx as usize];
                if let Stmt::Block(body) = stmt {
                    self.interpreter
                        .run_whenever_with_value(supply_val, target_var, &param, body)?;
                    self.sync_locals_from_env(code);
                    *ip += 1;
                } else {
                    return Err(RuntimeError::new("WheneverScope expects Block body"));
                }
            }

            // -- Local variables (indexed slots) --
            OpCode::GetLocal(idx) => {
                self.exec_get_local_op(*idx);
                *ip += 1;
            }
            OpCode::SetLocal(idx) => {
                self.exec_set_local_op(code, *idx)?;
                *ip += 1;
            }
            // -- Assignment as expression for local variable --
            OpCode::AssignExprLocal(idx) => {
                self.exec_assign_expr_local_op(code, *idx);
                *ip += 1;
            }
            OpCode::AssignReadOnly => {
                return Err(RuntimeError::new("X::Assignment::RO"));
            }
        }
        Ok(())
    }
}
