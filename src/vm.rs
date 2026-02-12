#![allow(clippy::result_large_err)]
use std::collections::HashMap;

use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::value::{RuntimeError, Value, make_rat};

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

    fn is_builtin_type(name: &str) -> bool {
        matches!(
            name,
            "Hash"
                | "Array"
                | "Int"
                | "Num"
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
                | "Failure"
                | "Exception"
                | "Order"
                | "Version"
                | "Nil"
        )
    }

    /// Execute one opcode at *ip, advancing ip for the next instruction.
    fn exec_one(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
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
                    // Bare function call with no args
                    self.interpreter.call_function(name, Vec::new())?
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
                // Range + Int: shift both bounds
                let range_result = match (&left, &right) {
                    (Value::Range(a, b), Value::Int(n)) => Some(Value::Range(a + n, b + n)),
                    (Value::RangeExcl(a, b), Value::Int(n)) => Some(Value::RangeExcl(a + n, b + n)),
                    (Value::RangeExclStart(a, b), Value::Int(n)) => {
                        Some(Value::RangeExclStart(a + n, b + n))
                    }
                    (Value::RangeExclBoth(a, b), Value::Int(n)) => {
                        Some(Value::RangeExclBoth(a + n, b + n))
                    }
                    (Value::Int(n), Value::Range(a, b)) => Some(Value::Range(a + n, b + n)),
                    (Value::Int(n), Value::RangeExcl(a, b)) => Some(Value::RangeExcl(a + n, b + n)),
                    (Value::Int(n), Value::RangeExclStart(a, b)) => {
                        Some(Value::RangeExclStart(a + n, b + n))
                    }
                    (Value::Int(n), Value::RangeExclBoth(a, b)) => {
                        Some(Value::RangeExclBoth(a + n, b + n))
                    }
                    _ => None,
                };
                if let Some(val) = range_result {
                    self.stack.push(val);
                    *ip += 1;
                } else {
                    let (l, r) = Interpreter::coerce_numeric(left, right);
                    let result =
                        if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                            let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                            let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                            Value::Complex(ar + br, ai + bi)
                        } else if let (Some((an, ad)), Some((bn, bd))) =
                            (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
                        {
                            if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                                make_rat(an * bd + bn * ad, ad * bd)
                            } else {
                                match (l, r) {
                                    (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b)),
                                    (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                                    (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
                                    (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
                                    _ => Value::Int(0),
                                }
                            }
                        } else {
                            match (l, r) {
                                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_add(b)),
                                (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 + b),
                                (Value::Num(a), Value::Int(b)) => Value::Num(a + b as f64),
                                _ => Value::Int(0),
                            }
                        };
                    self.stack.push(result);
                    *ip += 1;
                } // end else (non-range Add)
            }
            OpCode::Sub => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result =
                    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                        Value::Complex(ar - br, ai - bi)
                    } else if let (Some((an, ad)), Some((bn, bd))) =
                        (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
                    {
                        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                            make_rat(an * bd - bn * ad, ad * bd)
                        } else {
                            match (l, r) {
                                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b)),
                                (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
                                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
                                (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
                                _ => Value::Int(0),
                            }
                        }
                    } else {
                        match (l, r) {
                            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_sub(b)),
                            (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
                            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 - b),
                            (Value::Num(a), Value::Int(b)) => Value::Num(a - b as f64),
                            _ => Value::Int(0),
                        }
                    };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::Mul => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result =
                    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                        Value::Complex(ar * br - ai * bi, ar * bi + ai * br)
                    } else if let (Some((an, ad)), Some((bn, bd))) =
                        (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
                    {
                        if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                            make_rat(an * bn, ad * bd)
                        } else {
                            match (l, r) {
                                (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b)),
                                (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
                                (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
                                (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
                                _ => Value::Int(0),
                            }
                        }
                    } else {
                        match (l, r) {
                            (Value::Int(a), Value::Int(b)) => Value::Int(a.wrapping_mul(b)),
                            (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
                            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 * b),
                            (Value::Num(a), Value::Int(b)) => Value::Num(a * b as f64),
                            _ => Value::Int(0),
                        }
                    };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::Div => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result =
                    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                        let denom = br * br + bi * bi;
                        if denom == 0.0 {
                            return Err(RuntimeError::new("Division by zero"));
                        }
                        Value::Complex((ar * br + ai * bi) / denom, (ai * br - ar * bi) / denom)
                    } else {
                        match (&l, &r) {
                            (Value::Rat(_, _), _)
                            | (_, Value::Rat(_, _))
                            | (Value::Int(_), Value::Int(_)) => {
                                let (an, ad) = Interpreter::to_rat_parts(&l).unwrap_or((0, 1));
                                let (bn, bd) = Interpreter::to_rat_parts(&r).unwrap_or((0, 1));
                                if bn == 0 {
                                    return Err(RuntimeError::new("Division by zero"));
                                }
                                make_rat(an * bd, ad * bn)
                            }
                            (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
                            (Value::Int(a), Value::Num(b)) => Value::Num(*a as f64 / b),
                            (Value::Num(a), Value::Int(b)) => Value::Num(a / *b as f64),
                            _ => Value::Int(0),
                        }
                    };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::Mod => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result = if let (Some((an, ad)), Some((bn, bd))) =
                    (Interpreter::to_rat_parts(&l), Interpreter::to_rat_parts(&r))
                {
                    if matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)) {
                        if bn == 0 {
                            return Err(RuntimeError::new("Modulo by zero"));
                        }
                        let lf = an as f64 / ad as f64;
                        let rf = bn as f64 / bd as f64;
                        Value::Num(lf % rf)
                    } else {
                        match (l, r) {
                            (Value::Int(_), Value::Int(0)) => {
                                return Err(RuntimeError::new("Modulo by zero"));
                            }
                            (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                            (Value::Num(a), Value::Num(b)) => Value::Num(a % b),
                            (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 % b),
                            (Value::Num(a), Value::Int(b)) => Value::Num(a % b as f64),
                            _ => Value::Int(0),
                        }
                    }
                } else {
                    match (l, r) {
                        (Value::Int(_), Value::Int(0)) => {
                            return Err(RuntimeError::new("Modulo by zero"));
                        }
                        (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                        (Value::Num(a), Value::Num(b)) => Value::Num(a % b),
                        (Value::Int(a), Value::Num(b)) => Value::Num(a as f64 % b),
                        (Value::Num(a), Value::Int(b)) => Value::Num(a % b as f64),
                        _ => Value::Int(0),
                    }
                };
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::Pow => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (l, r) = Interpreter::coerce_numeric(left, right);
                let result =
                    if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                        let (ar, ai) = Interpreter::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                        let (br, bi) = Interpreter::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                        let ln_r = (ar * ar + ai * ai).sqrt().ln();
                        let ln_i = ai.atan2(ar);
                        let wr = br * ln_r - bi * ln_i;
                        let wi = br * ln_i + bi * ln_r;
                        let mag = wr.exp();
                        Value::Complex(mag * wi.cos(), mag * wi.sin())
                    } else {
                        match (l, r) {
                            (Value::Int(a), Value::Int(b)) if b >= 0 => Value::Int(a.pow(b as u32)),
                            (Value::Int(a), Value::Int(b)) => {
                                let pos = (-b) as u32;
                                let base = a.pow(pos);
                                make_rat(1, base)
                            }
                            (Value::Rat(n, d), Value::Int(b)) if b >= 0 => {
                                let p = b as u32;
                                make_rat(n.pow(p), d.pow(p))
                            }
                            (Value::Rat(n, d), Value::Int(b)) => {
                                let p = (-b) as u32;
                                make_rat(d.pow(p), n.pow(p))
                            }
                            (Value::Num(a), Value::Int(b)) => Value::Num(a.powi(b as i32)),
                            (Value::Int(a), Value::Num(b)) => Value::Num((a as f64).powf(b)),
                            (Value::Num(a), Value::Num(b)) => Value::Num(a.powf(b)),
                            _ => Value::Int(0),
                        }
                    };
                self.stack.push(result);
                *ip += 1;
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
                let result = self
                    .eval_binary_with_junctions(left, right, |_, l, r| Ok(Value::Bool(l == r)))?;
                self.stack.push(result);
                *ip += 1;
            }
            OpCode::NumNe => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let result = self
                    .eval_binary_with_junctions(left, right, |_, l, r| Ok(Value::Bool(l != r)))?;
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
                            (an * bd).cmp(&(bn * ad))
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
                            (an * bd).cmp(&(bn * ad))
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
                self.stack.push(Value::Bool(left == right));
                *ip += 1;
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
                let (mut a, mut b) = (
                    Interpreter::to_int(&left).abs(),
                    Interpreter::to_int(&right).abs(),
                );
                while b != 0 {
                    let t = b;
                    b = a % b;
                    a = t;
                }
                self.stack.push(Value::Int(a));
                *ip += 1;
            }
            OpCode::Lcm => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let (a, b) = (
                    Interpreter::to_int(&left).abs(),
                    Interpreter::to_int(&right).abs(),
                );
                let result = if a == 0 && b == 0 {
                    Value::Int(0)
                } else {
                    let mut ga = a;
                    let mut gb = b;
                    while gb != 0 {
                        let t = gb;
                        gb = ga % gb;
                        ga = t;
                    }
                    Value::Int(a / ga * b)
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

            // -- Sequence (...) --
            OpCode::Sequence => {
                let right = self.stack.pop().unwrap();
                let left = self.stack.pop().unwrap();
                let seeds = Interpreter::value_to_list(&left);
                if seeds.is_empty() {
                    self.stack.push(Value::Array(vec![]));
                } else {
                    let endpoint = match &right {
                        Value::Num(f) if f.is_infinite() => None,
                        Value::Int(n) => Some(*n),
                        _ => {
                            self.stack.push(Value::Array(seeds));
                            *ip += 1;
                            return Ok(());
                        }
                    };
                    let mut result: Vec<Value> = seeds.clone();
                    let step = if seeds.len() >= 2 {
                        match (&seeds[seeds.len() - 1], &seeds[seeds.len() - 2]) {
                            (Value::Int(b), Value::Int(a)) => b - a,
                            _ => 1,
                        }
                    } else {
                        1
                    };
                    let last = match seeds.last() {
                        Some(Value::Int(n)) => *n,
                        _ => {
                            self.stack.push(Value::Array(result));
                            *ip += 1;
                            return Ok(());
                        }
                    };
                    let mut cur = last + step;
                    let limit = endpoint.unwrap_or(last + 1000);
                    if step > 0 {
                        while cur <= limit {
                            result.push(Value::Int(cur));
                            cur += step;
                        }
                    } else if step < 0 {
                        while cur >= limit {
                            result.push(Value::Int(cur));
                            cur += step;
                        }
                    }
                    self.stack.push(Value::Array(result));
                }
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
                        self.locals[slot as usize] = item;
                    }
                    'body_redo: loop {
                        match self.run_range(code, body_start, loop_end, compiled_fns) {
                            Ok(()) => break 'body_redo,
                            Err(e) if e.is_redo && Self::label_matches(&e.label, &label) => {
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
                        let err_val = Value::Str(e.message.clone());
                        let saved_err = self.interpreter.env().get("!").cloned();
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
                        // Restore $! and $_
                        if let Some(v) = saved_err {
                            self.interpreter.env_mut().insert("!".to_string(), v);
                        } else {
                            self.interpreter.env_mut().remove("!");
                        }
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

            // -- Fallback to tree-walker --
            OpCode::InterpretStmt(idx) => {
                let stmt = &code.stmt_pool[*idx as usize];
                self.interpreter.exec_stmt(stmt)?;
                self.sync_locals_from_env(code);
                *ip += 1;
            }
            OpCode::InterpretExpr(idx) => {
                let expr = &code.expr_pool[*idx as usize];
                let val = self.interpreter.eval_expr(expr)?;
                self.stack.push(val);
                self.sync_locals_from_env(code);
                *ip += 1;
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
    /// Called after operations that may modify env (InterpretExpr, InterpretStmt, etc.).
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
            return Self::try_native_method_2arg(target, method, &args[0], &args[1]);
        }
        if args.len() == 1 {
            return Self::try_native_method_1arg(target, method, &args[0]);
        }
        if !args.is_empty() {
            return None;
        }
        match method {
            "defined" => Some(Ok(Value::Bool(!matches!(target, Value::Nil)))),
            "Bool" => Some(Ok(Value::Bool(target.truthy()))),
            "Str" => {
                // Exclude complex types with special interpreter handling
                match target {
                    Value::Package(_) | Value::Instance { .. } => None,
                    Value::Str(s) if s == "IO::Special" => Some(Ok(Value::Str(String::new()))),
                    _ => Some(Ok(Value::Str(target.to_string_value()))),
                }
            }
            "Int" => {
                let result = match target {
                    Value::Int(i) => Value::Int(*i),
                    Value::Num(f) => Value::Int(*f as i64),
                    Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                    Value::Str(s) => {
                        if let Ok(i) = s.trim().parse::<i64>() {
                            Value::Int(i)
                        } else if let Ok(f) = s.trim().parse::<f64>() {
                            Value::Int(f as i64)
                        } else {
                            return None; // Fall back to interpreter for error handling
                        }
                    }
                    Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                    _ => return None,
                };
                Some(Ok(result))
            }
            "Numeric" | "Num" => {
                let result = match target {
                    Value::Int(i) => Value::Int(*i),
                    Value::Num(f) => Value::Num(*f),
                    Value::Rat(n, d) if *d != 0 => Value::Num(*n as f64 / *d as f64),
                    Value::Str(s) => {
                        if let Ok(i) = s.trim().parse::<i64>() {
                            Value::Int(i)
                        } else if let Ok(f) = s.trim().parse::<f64>() {
                            Value::Num(f)
                        } else {
                            return None;
                        }
                    }
                    Value::Bool(b) => Value::Int(if *b { 1 } else { 0 }),
                    _ => return None,
                };
                Some(Ok(result))
            }
            "chars" => Some(Ok(Value::Int(
                target.to_string_value().chars().count() as i64
            ))),
            "elems" => {
                let result = match target {
                    Value::Array(items) => Value::Int(items.len() as i64),
                    Value::Hash(items) => Value::Int(items.len() as i64),
                    Value::Set(items) => Value::Int(items.len() as i64),
                    Value::Bag(items) => Value::Int(items.len() as i64),
                    Value::Mix(items) => Value::Int(items.len() as i64),
                    _ => return None,
                };
                Some(Ok(result))
            }
            "Complex-i" => {
                let imag = match target {
                    Value::Int(i) => *i as f64,
                    Value::Num(f) => *f,
                    _ => 0.0,
                };
                Some(Ok(Value::Complex(0.0, imag)))
            }
            "abs" => {
                let result = match target {
                    Value::Int(i) => Value::Int(i.abs()),
                    Value::Num(f) => Value::Num(f.abs()),
                    Value::Rat(n, d) => Value::Rat(n.abs(), *d),
                    _ => return None,
                };
                Some(Ok(result))
            }
            "uc" => Some(Ok(Value::Str(target.to_string_value().to_uppercase()))),
            "lc" => Some(Ok(Value::Str(target.to_string_value().to_lowercase()))),
            "sign" => {
                let result = match target {
                    Value::Int(i) => Value::Int(i.signum()),
                    Value::Num(f) => {
                        if f.is_nan() {
                            Value::Num(f64::NAN)
                        } else {
                            Value::Int(if *f > 0.0 {
                                1
                            } else if *f < 0.0 {
                                -1
                            } else {
                                0
                            })
                        }
                    }
                    _ => return None,
                };
                Some(Ok(result))
            }
            "end" => match target {
                Value::Array(items) => Some(Ok(Value::Int(items.len() as i64 - 1))),
                _ => None,
            },
            "flat" => match target {
                Value::Array(items) => {
                    let mut result = Vec::new();
                    for item in items {
                        match item {
                            Value::Array(inner) => result.extend(inner.iter().cloned()),
                            other => result.push(other.clone()),
                        }
                    }
                    Some(Ok(Value::Array(result)))
                }
                _ => None,
            },
            "sort" => match target {
                Value::Array(items) => {
                    let mut sorted = items.clone();
                    sorted.sort_by_key(|a| a.to_string_value());
                    Some(Ok(Value::Array(sorted)))
                }
                _ => None,
            },
            "reverse" => match target {
                Value::Array(items) => {
                    let mut reversed = items.clone();
                    reversed.reverse();
                    Some(Ok(Value::Array(reversed)))
                }
                Value::Str(s) => Some(Ok(Value::Str(s.chars().rev().collect()))),
                _ => None,
            },
            "unique" => match target {
                Value::Array(items) => {
                    let mut seen = Vec::new();
                    let mut result = Vec::new();
                    for item in items {
                        let key = item.to_string_value();
                        if !seen.contains(&key) {
                            seen.push(key);
                            result.push(item.clone());
                        }
                    }
                    Some(Ok(Value::Array(result)))
                }
                _ => None,
            },
            "keys" => match target {
                Value::Hash(map) => {
                    let keys: Vec<Value> = map.keys().map(|k| Value::Str(k.clone())).collect();
                    Some(Ok(Value::Array(keys)))
                }
                _ => None,
            },
            "values" => match target {
                Value::Hash(map) => {
                    let values: Vec<Value> = map.values().cloned().collect();
                    Some(Ok(Value::Array(values)))
                }
                _ => None,
            },
            "floor" => match target {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
                Value::Num(f) => Some(Ok(Value::Int(f.floor() as i64))),
                Value::Int(i) => Some(Ok(Value::Int(*i))),
                Value::Rat(n, d) if *d != 0 => {
                    let q = *n / *d;
                    let r = *n % *d;
                    if r != 0 && (*n < 0) != (*d < 0) {
                        Some(Ok(Value::Int(q - 1)))
                    } else {
                        Some(Ok(Value::Int(q)))
                    }
                }
                _ => None,
            },
            "ceiling" => match target {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
                Value::Num(f) => Some(Ok(Value::Int(f.ceil() as i64))),
                Value::Int(i) => Some(Ok(Value::Int(*i))),
                Value::Rat(n, d) if *d != 0 => {
                    let q = *n / *d;
                    let r = *n % *d;
                    if r != 0 && (*n < 0) == (*d < 0) {
                        Some(Ok(Value::Int(q + 1)))
                    } else {
                        Some(Ok(Value::Int(q)))
                    }
                }
                _ => None,
            },
            "round" => match target {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Some(Ok(Value::Num(*f))),
                Value::Num(f) => Some(Ok(Value::Int(f.round() as i64))),
                Value::Int(i) => Some(Ok(Value::Int(*i))),
                _ => None,
            },
            "sqrt" => match target {
                Value::Int(i) => Some(Ok(Value::Num((*i as f64).sqrt()))),
                Value::Num(f) => Some(Ok(Value::Num(f.sqrt()))),
                _ => None,
            },
            "words" => {
                let s = target.to_string_value();
                let words: Vec<Value> = s
                    .split_whitespace()
                    .map(|w| Value::Str(w.to_string()))
                    .collect();
                Some(Ok(Value::Array(words)))
            }
            "lines" => {
                let s = target.to_string_value();
                let lines: Vec<Value> = s.lines().map(|l| Value::Str(l.to_string())).collect();
                Some(Ok(Value::Array(lines)))
            }
            "trim" => Some(Ok(Value::Str(target.to_string_value().trim().to_string()))),
            "trim-leading" => Some(Ok(Value::Str(
                target.to_string_value().trim_start().to_string(),
            ))),
            "trim-trailing" => Some(Ok(Value::Str(
                target.to_string_value().trim_end().to_string(),
            ))),
            "so" => Some(Ok(Value::Bool(target.truthy()))),
            "not" => Some(Ok(Value::Bool(!target.truthy()))),
            "chomp" => Some(Ok(Value::Str(
                target.to_string_value().trim_end_matches('\n').to_string(),
            ))),
            "chop" => {
                let mut s = target.to_string_value();
                s.pop();
                Some(Ok(Value::Str(s)))
            }
            "comb" => {
                let s = target.to_string_value();
                let parts: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
                Some(Ok(Value::Array(parts)))
            }
            "gist" | "raku" | "perl" => match target {
                Value::Nil => Some(Ok(Value::Str("(Any)".to_string()))),
                Value::Rat(n, d) => {
                    if *d == 0 {
                        if *n == 0 {
                            Some(Ok(Value::Str("NaN".to_string())))
                        } else if *n > 0 {
                            Some(Ok(Value::Str("Inf".to_string())))
                        } else {
                            Some(Ok(Value::Str("-Inf".to_string())))
                        }
                    } else {
                        let mut dd = *d;
                        while dd % 2 == 0 {
                            dd /= 2;
                        }
                        while dd % 5 == 0 {
                            dd /= 5;
                        }
                        if dd == 1 {
                            let val = *n as f64 / *d as f64;
                            let s = format!("{}", val);
                            if s.contains('.') {
                                Some(Ok(Value::Str(s)))
                            } else {
                                Some(Ok(Value::Str(format!("{}.0", val))))
                            }
                        } else {
                            Some(Ok(Value::Str(format!("<{}/{}>", n, d))))
                        }
                    }
                }
                // Complex types fall through to interpreter
                Value::Package(_) | Value::Instance { .. } | Value::Enum { .. } => None,
                Value::Version { parts, plus, minus } => {
                    let s = Value::version_parts_to_string(parts);
                    let suffix = if *plus {
                        "+"
                    } else if *minus {
                        "-"
                    } else {
                        ""
                    };
                    Some(Ok(Value::Str(format!("v{}{}", s, suffix))))
                }
                _ => Some(Ok(Value::Str(target.to_string_value()))),
            },
            "head" => match target {
                Value::Array(items) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
                _ => None,
            },
            "tail" => match target {
                Value::Array(items) => Some(Ok(items.last().cloned().unwrap_or(Value::Nil))),
                _ => None,
            },
            "first" => match target {
                Value::Array(items) => Some(Ok(items.first().cloned().unwrap_or(Value::Nil))),
                _ => None,
            },
            "min" => match target {
                Value::Array(items) => Some(Ok(items
                    .iter()
                    .cloned()
                    .min_by(|a, b| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => x.cmp(y),
                        _ => a.to_string_value().cmp(&b.to_string_value()),
                    })
                    .unwrap_or(Value::Nil))),
                _ => Some(Ok(target.clone())),
            },
            "max" => match target {
                Value::Array(items) => Some(Ok(items
                    .iter()
                    .cloned()
                    .max_by(|a, b| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => x.cmp(y),
                        _ => a.to_string_value().cmp(&b.to_string_value()),
                    })
                    .unwrap_or(Value::Nil))),
                _ => Some(Ok(target.clone())),
            },
            "tclc" => {
                let s = target.to_string_value();
                let mut result = String::new();
                let mut first = true;
                for ch in s.chars() {
                    if first {
                        for c in ch.to_uppercase() {
                            result.push(c);
                        }
                        first = false;
                    } else {
                        for c in ch.to_lowercase() {
                            result.push(c);
                        }
                    }
                }
                Some(Ok(Value::Str(result)))
            }
            "succ" => match target {
                Value::Enum { .. } | Value::Instance { .. } => None,
                Value::Int(i) => Some(Ok(Value::Int(i + 1))),
                Value::Str(s) => {
                    if s.is_empty() {
                        Some(Ok(Value::Str(String::new())))
                    } else {
                        let mut chars: Vec<char> = s.chars().collect();
                        if let Some(last) = chars.last_mut() {
                            *last = char::from_u32(*last as u32 + 1).unwrap_or(*last);
                        }
                        Some(Ok(Value::Str(chars.into_iter().collect())))
                    }
                }
                _ => Some(Ok(target.clone())),
            },
            "pred" => match target {
                Value::Enum { .. } | Value::Instance { .. } => None,
                Value::Int(i) => Some(Ok(Value::Int(i - 1))),
                _ => Some(Ok(target.clone())),
            },
            "log" => match target {
                Value::Int(i) => Some(Ok(Value::Num((*i as f64).ln()))),
                Value::Num(f) => Some(Ok(Value::Num(f.ln()))),
                _ => Some(Ok(Value::Num(f64::NAN))),
            },
            "exp" => match target {
                Value::Int(i) => Some(Ok(Value::Num((*i as f64).exp()))),
                Value::Num(f) => Some(Ok(Value::Num(f.exp()))),
                _ => Some(Ok(Value::Num(f64::NAN))),
            },
            "Rat" => match target {
                Value::Rat(_, _) => Some(Ok(target.clone())),
                Value::Int(i) => Some(Ok(make_rat(*i, 1))),
                Value::Num(f) => {
                    let denom = 1_000_000i64;
                    let numer = (f * denom as f64).round() as i64;
                    Some(Ok(make_rat(numer, denom)))
                }
                Value::FatRat(n, d) => Some(Ok(make_rat(*n, *d))),
                _ => Some(Ok(make_rat(0, 1))),
            },
            _ => None,
        }
    }

    /// Try to dispatch a one-argument method call natively.
    fn try_native_method_1arg(
        target: &Value,
        method: &str,
        arg: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            // String methods
            "contains" => {
                let s = target.to_string_value();
                let needle = arg.to_string_value();
                Some(Ok(Value::Bool(s.contains(&needle))))
            }
            "starts-with" => {
                let s = target.to_string_value();
                let prefix = arg.to_string_value();
                Some(Ok(Value::Bool(s.starts_with(&prefix))))
            }
            "ends-with" => {
                let s = target.to_string_value();
                let suffix = arg.to_string_value();
                Some(Ok(Value::Bool(s.ends_with(&suffix))))
            }
            "index" => {
                let s = target.to_string_value();
                let needle = arg.to_string_value();
                match s.find(&needle) {
                    Some(pos) => {
                        let char_pos = s[..pos].chars().count();
                        Some(Ok(Value::Int(char_pos as i64)))
                    }
                    None => Some(Ok(Value::Nil)),
                }
            }
            "substr" => {
                let s = target.to_string_value();
                let start = match arg {
                    Value::Int(i) => (*i).max(0) as usize,
                    _ => return None,
                };
                let result: String = s.chars().skip(start).collect();
                Some(Ok(Value::Str(result)))
            }
            "split" => {
                let s = target.to_string_value();
                let sep = arg.to_string_value();
                let parts: Vec<Value> = s.split(&sep).map(|p| Value::Str(p.to_string())).collect();
                Some(Ok(Value::Array(parts)))
            }
            // Array methods
            "join" => match target {
                Value::Array(items) => {
                    let sep = arg.to_string_value();
                    let joined = items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<_>>()
                        .join(&sep);
                    Some(Ok(Value::Str(joined)))
                }
                _ => None,
            },
            "head" => match target {
                Value::Array(items) => {
                    let n = match arg {
                        Value::Int(i) => *i as usize,
                        _ => return None,
                    };
                    Some(Ok(Value::Array(items[..n.min(items.len())].to_vec())))
                }
                _ => None,
            },
            "tail" => match target {
                Value::Array(items) => {
                    let n = match arg {
                        Value::Int(i) => *i as usize,
                        _ => return None,
                    };
                    let start = items.len().saturating_sub(n);
                    Some(Ok(Value::Array(items[start..].to_vec())))
                }
                _ => None,
            },
            "rindex" => {
                let s = target.to_string_value();
                let needle = arg.to_string_value();
                match s.rfind(&needle) {
                    Some(pos) => {
                        let char_pos = s[..pos].chars().count();
                        Some(Ok(Value::Int(char_pos as i64)))
                    }
                    None => Some(Ok(Value::Nil)),
                }
            }
            "fmt" => {
                let fmt = arg.to_string_value();
                let rendered = Interpreter::format_sprintf(&fmt, Some(target));
                Some(Ok(Value::Str(rendered)))
            }
            "parse-base" => {
                let radix = match arg {
                    Value::Int(n) => *n as u32,
                    _ => return None,
                };
                let s = target.to_string_value();
                match i64::from_str_radix(&s, radix) {
                    Ok(n) => Some(Ok(Value::Int(n))),
                    Err(_) => Some(Err(RuntimeError::new(format!(
                        "Cannot parse '{}' as base {}",
                        s, radix
                    )))),
                }
            }
            // Numeric methods
            "base" => match target {
                Value::Int(i) => {
                    let radix = match arg {
                        Value::Int(r) => *r,
                        _ => return None,
                    };
                    let s = match radix {
                        2 => format!("{:b}", i),
                        8 => format!("{:o}", i),
                        16 => format!("{:X}", i),
                        _ => format!("{}", i),
                    };
                    Some(Ok(Value::Str(s)))
                }
                _ => None,
            },
            "round" => {
                let scale = match arg {
                    Value::Int(i) => *i as f64,
                    Value::Num(f) => *f,
                    _ => return None,
                };
                let x = match target {
                    Value::Int(i) => *i as f64,
                    Value::Num(f) => *f,
                    _ => return None,
                };
                if scale == 0.0 {
                    return Some(Ok(Value::Int(x.round() as i64)));
                }
                let factor = (1.0 / scale).abs();
                Some(Ok(Value::Num((x * factor).round() / factor)))
            }
            "log" => {
                let base_val = match arg {
                    Value::Int(i) => *i as f64,
                    Value::Num(f) => *f,
                    _ => return None,
                };
                let x = match target {
                    Value::Int(i) => *i as f64,
                    Value::Num(f) => *f,
                    _ => return None,
                };
                if base_val.is_finite() && base_val > 0.0 && base_val != 1.0 && x > 0.0 {
                    Some(Ok(Value::Num(x.ln() / base_val.ln())))
                } else {
                    Some(Ok(Value::Num(f64::NAN)))
                }
            }
            _ => None,
        }
    }

    /// Try to dispatch a function call natively (pure built-in functions).
    fn try_native_function(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
        match args.len() {
            1 => Self::try_native_function_1arg(name, &args[0]),
            2 => Self::try_native_function_2arg(name, &args[0], &args[1]),
            3 => Self::try_native_function_3arg(name, &args[0], &args[1], &args[2]),
            _ => Self::try_native_function_variadic(name, args),
        }
    }

    /// Native dispatch for one-arg built-in functions.
    fn try_native_function_1arg(name: &str, arg: &Value) -> Option<Result<Value, RuntimeError>> {
        match name {
            // String functions
            "uc" => Some(Ok(Value::Str(arg.to_string_value().to_uppercase()))),
            "lc" => Some(Ok(Value::Str(arg.to_string_value().to_lowercase()))),
            "tc" => {
                let s = arg.to_string_value();
                let mut result = String::new();
                let mut capitalize = true;
                for ch in s.chars() {
                    if capitalize {
                        for c in ch.to_uppercase() {
                            result.push(c);
                        }
                        capitalize = false;
                    } else {
                        result.push(ch);
                    }
                }
                Some(Ok(Value::Str(result)))
            }
            "chomp" => Some(Ok(Value::Str(
                arg.to_string_value().trim_end_matches('\n').to_string(),
            ))),
            "chop" => {
                let mut s = arg.to_string_value();
                s.pop();
                Some(Ok(Value::Str(s)))
            }
            "trim" => Some(Ok(Value::Str(arg.to_string_value().trim().to_string()))),
            "flip" => Some(Ok(Value::Str(
                arg.to_string_value().chars().rev().collect(),
            ))),
            "words" => {
                let s = arg.to_string_value();
                let parts: Vec<Value> = s
                    .split_whitespace()
                    .map(|p| Value::Str(p.to_string()))
                    .collect();
                Some(Ok(Value::Array(parts)))
            }
            "chars" => Some(Ok(Value::Int(arg.to_string_value().chars().count() as i64))),
            // Char/Ord
            "chr" => {
                if let Value::Int(i) = arg
                    && *i >= 0
                    && let Some(ch) = std::char::from_u32(*i as u32)
                {
                    return Some(Ok(Value::Str(ch.to_string())));
                }
                Some(Ok(Value::Str(String::new())))
            }
            "ord" => {
                if let Some(ch) = arg.to_string_value().chars().next() {
                    Some(Ok(Value::Int(ch as u32 as i64)))
                } else {
                    Some(Ok(Value::Nil))
                }
            }
            // Math
            "abs" => Some(Ok(match arg {
                Value::Int(i) => Value::Int(i.abs()),
                Value::Num(f) => Value::Num(f.abs()),
                _ => Value::Int(0),
            })),
            "sqrt" => Some(Ok(match arg {
                Value::Int(i) => Value::Num((*i as f64).sqrt()),
                Value::Num(f) => Value::Num(f.sqrt()),
                _ => Value::Num(f64::NAN),
            })),
            "floor" => Some(Ok(match arg {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
                Value::Num(f) => Value::Int(f.floor() as i64),
                Value::Int(i) => Value::Int(*i),
                _ => Value::Int(0),
            })),
            "ceiling" | "ceil" => Some(Ok(match arg {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
                Value::Num(f) => Value::Int(f.ceil() as i64),
                Value::Int(i) => Value::Int(*i),
                _ => Value::Int(0),
            })),
            "round" => Some(Ok(match arg {
                Value::Num(f) if f.is_nan() || f.is_infinite() => Value::Num(*f),
                Value::Num(f) => Value::Int(f.round() as i64),
                Value::Int(i) => Value::Int(*i),
                _ => Value::Int(0),
            })),
            "exp" => Some(Ok(match arg {
                Value::Int(i) => Value::Num((*i as f64).exp()),
                Value::Num(f) => Value::Num(f.exp()),
                _ => Value::Num(f64::NAN),
            })),
            "log" => {
                let x = Interpreter::to_float_value(arg).unwrap_or(f64::NAN);
                Some(Ok(Value::Num(x.ln())))
            }
            "sin" | "cos" | "tan" | "asin" | "acos" | "atan" => {
                let x = Interpreter::to_float_value(arg).unwrap_or(0.0);
                let result = match name {
                    "sin" => x.sin(),
                    "cos" => x.cos(),
                    "tan" => x.tan(),
                    "asin" => x.asin(),
                    "acos" => x.acos(),
                    "atan" => x.atan(),
                    _ => 0.0,
                };
                Some(Ok(Value::Num(result)))
            }
            "truncate" => {
                if let Some(num) = Interpreter::to_float_value(arg) {
                    if num.is_nan() || num.is_infinite() {
                        Some(Ok(Value::Num(num)))
                    } else {
                        Some(Ok(Value::Int(num.trunc() as i64)))
                    }
                } else {
                    Some(Ok(Value::Int(Interpreter::to_int(arg))))
                }
            }
            // Query
            "defined" => Some(Ok(Value::Bool(!matches!(arg, Value::Nil)))),
            "elems" => match arg {
                Value::Array(items) => Some(Ok(Value::Int(items.len() as i64))),
                Value::Hash(items) => Some(Ok(Value::Int(items.len() as i64))),
                Value::Str(s) => Some(Ok(Value::Int(s.chars().count() as i64))),
                Value::LazyList(_) => None, // fall back to interpreter which can force the list
                _ => Some(Ok(Value::Int(0))),
            },
            // Collection
            "reverse" => Some(Ok(match arg {
                Value::Array(items) => {
                    let mut reversed = items.clone();
                    reversed.reverse();
                    Value::Array(reversed)
                }
                Value::Str(s) => Value::Str(s.chars().rev().collect()),
                _ => Value::Nil,
            })),
            "sort" => Some(Ok(match arg {
                Value::Array(items) => {
                    let mut sorted = items.clone();
                    sorted.sort_by_key(|a| a.to_string_value());
                    Value::Array(sorted)
                }
                _ => Value::Nil,
            })),
            "flat" => Some(Ok(match arg {
                Value::Array(items) => {
                    let mut flat = Vec::new();
                    for item in items {
                        if let Value::Array(sub) = item {
                            flat.extend(sub.clone());
                        } else {
                            flat.push(item.clone());
                        }
                    }
                    Value::Array(flat)
                }
                other => Value::Array(vec![other.clone()]),
            })),
            "first" => Some(Ok(match arg {
                Value::Array(items) => items.first().cloned().unwrap_or(Value::Nil),
                _ => arg.clone(),
            })),
            "min" => Some(Ok(match arg {
                Value::Array(items) => items
                    .iter()
                    .cloned()
                    .min_by(|a, b| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => x.cmp(y),
                        _ => a.to_string_value().cmp(&b.to_string_value()),
                    })
                    .unwrap_or(Value::Nil),
                _ => arg.clone(),
            })),
            "max" => Some(Ok(match arg {
                Value::Array(items) => items
                    .iter()
                    .cloned()
                    .max_by(|a, b| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => x.cmp(y),
                        _ => a.to_string_value().cmp(&b.to_string_value()),
                    })
                    .unwrap_or(Value::Nil),
                _ => arg.clone(),
            })),
            "ords" => {
                let s = arg.to_string_value();
                let codes: Vec<Value> = s.chars().map(|ch| Value::Int(ch as u32 as i64)).collect();
                Some(Ok(Value::Array(codes)))
            }
            "gist" => Some(Ok(Value::Str(arg.to_string_value()))),
            _ => None,
        }
    }

    /// Native dispatch for two-arg built-in functions.
    fn try_native_function_2arg(
        name: &str,
        arg1: &Value,
        arg2: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match name {
            "join" => {
                let sep = arg1.to_string_value();
                match arg2 {
                    Value::Array(items) => {
                        let joined = items
                            .iter()
                            .map(|v| v.to_string_value())
                            .collect::<Vec<_>>()
                            .join(&sep);
                        Some(Ok(Value::Str(joined)))
                    }
                    _ => Some(Ok(Value::Str(String::new()))),
                }
            }
            "index" => {
                let s = arg1.to_string_value();
                let needle = arg2.to_string_value();
                Some(Ok(match s.find(&needle) {
                    Some(pos) => Value::Int(s[..pos].chars().count() as i64),
                    None => Value::Nil,
                }))
            }
            "substr" => {
                let s = arg1.to_string_value();
                let start = match arg2 {
                    Value::Int(i) => (*i).max(0) as usize,
                    _ => return None,
                };
                let chars: Vec<char> = s.chars().collect();
                Some(Ok(Value::Str(
                    chars[start.min(chars.len())..].iter().collect(),
                )))
            }
            "atan2" => {
                let y = Interpreter::to_float_value(arg1).unwrap_or(0.0);
                let x = Interpreter::to_float_value(arg2).unwrap_or(0.0);
                Some(Ok(Value::Num(y.atan2(x))))
            }
            "log" => {
                let x = Interpreter::to_float_value(arg1).unwrap_or(f64::NAN);
                let base_val = Interpreter::to_float_value(arg2).unwrap_or(f64::NAN);
                if base_val.is_finite() && base_val > 0.0 && base_val != 1.0 && x > 0.0 {
                    Some(Ok(Value::Num(x.ln() / base_val.ln())))
                } else {
                    Some(Ok(Value::Num(f64::NAN)))
                }
            }
            "round" => {
                let x = Interpreter::to_float_value(arg1)?;
                let scale = Interpreter::to_float_value(arg2)?;
                if scale == 0.0 {
                    Some(Ok(Value::Int(x.round() as i64)))
                } else {
                    let factor = (1.0 / scale).abs();
                    Some(Ok(Value::Num((x * factor).round() / factor)))
                }
            }
            "min" => {
                let cmp = match (arg1, arg2) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => arg1.to_string_value().cmp(&arg2.to_string_value()),
                };
                Some(Ok(
                    if cmp == std::cmp::Ordering::Less || cmp == std::cmp::Ordering::Equal {
                        arg1.clone()
                    } else {
                        arg2.clone()
                    },
                ))
            }
            "max" => {
                let cmp = match (arg1, arg2) {
                    (Value::Int(x), Value::Int(y)) => x.cmp(y),
                    _ => arg1.to_string_value().cmp(&arg2.to_string_value()),
                };
                Some(Ok(
                    if cmp == std::cmp::Ordering::Greater || cmp == std::cmp::Ordering::Equal {
                        arg1.clone()
                    } else {
                        arg2.clone()
                    },
                ))
            }
            _ => None,
        }
    }

    /// Native dispatch for three-arg built-in functions.
    fn try_native_function_3arg(
        name: &str,
        arg1: &Value,
        arg2: &Value,
        arg3: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match name {
            "substr" => {
                let s = arg1.to_string_value();
                let start = match arg2 {
                    Value::Int(i) => (*i).max(0) as usize,
                    _ => return None,
                };
                let len = match arg3 {
                    Value::Int(i) => (*i).max(0) as usize,
                    _ => return None,
                };
                let chars: Vec<char> = s.chars().collect();
                let start = start.min(chars.len());
                let end = (start + len).min(chars.len());
                Some(Ok(Value::Str(chars[start..end].iter().collect())))
            }
            _ => None,
        }
    }

    /// Native dispatch for variadic built-in functions.
    fn try_native_function_variadic(
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match name {
            "min" => {
                if args.is_empty() {
                    return Some(Ok(Value::Nil));
                }
                Some(Ok(args
                    .iter()
                    .cloned()
                    .min_by(|a, b| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => x.cmp(y),
                        _ => a.to_string_value().cmp(&b.to_string_value()),
                    })
                    .unwrap_or(Value::Nil)))
            }
            "max" => {
                if args.is_empty() {
                    return Some(Ok(Value::Nil));
                }
                Some(Ok(args
                    .iter()
                    .cloned()
                    .max_by(|a, b| match (a, b) {
                        (Value::Int(x), Value::Int(y)) => x.cmp(y),
                        _ => a.to_string_value().cmp(&b.to_string_value()),
                    })
                    .unwrap_or(Value::Nil)))
            }
            "chrs" => {
                let mut result = String::new();
                for arg in args {
                    match arg {
                        Value::Int(i) => {
                            if *i >= 0
                                && let Some(ch) = std::char::from_u32(*i as u32)
                            {
                                result.push(ch);
                                continue;
                            }
                            result.push_str(&arg.to_string_value());
                        }
                        Value::Array(items) => {
                            for item in items {
                                if let Value::Int(i) = item
                                    && *i >= 0
                                    && let Some(ch) = std::char::from_u32(*i as u32)
                                {
                                    result.push(ch);
                                    continue;
                                }
                                result.push_str(&item.to_string_value());
                            }
                        }
                        _ => result.push_str(&arg.to_string_value()),
                    }
                }
                Some(Ok(Value::Str(result)))
            }
            "flat" => {
                let mut result = Vec::new();
                for arg in args {
                    match arg {
                        Value::Array(items) => {
                            for item in items {
                                if let Value::Array(sub) = item {
                                    result.extend(sub.clone());
                                } else {
                                    result.push(item.clone());
                                }
                            }
                        }
                        other => result.push(other.clone()),
                    }
                }
                Some(Ok(Value::Array(result)))
            }
            _ => None,
        }
    }

    /// Try to dispatch a two-arg method call natively.
    fn try_native_method_2arg(
        target: &Value,
        method: &str,
        arg1: &Value,
        arg2: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "substr" => {
                let s = target.to_string_value();
                let start = match arg1 {
                    Value::Int(i) => (*i).max(0) as usize,
                    _ => return None,
                };
                let len = match arg2 {
                    Value::Int(i) => (*i).max(0) as usize,
                    _ => return None,
                };
                let chars: Vec<char> = s.chars().collect();
                let end = (start + len).min(chars.len());
                let start = start.min(chars.len());
                Some(Ok(Value::Str(chars[start..end].iter().collect())))
            }
            _ => None,
        }
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
