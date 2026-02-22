#![allow(clippy::result_large_err)]
use std::collections::HashMap;

use crate::ast::Stmt;
use crate::interpreter::Interpreter;
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::runtime;
use crate::value::{JunctionKind, LazyList, RuntimeError, Value, make_rat};
use num_traits::{Signed, Zero};

mod vm_arith_ops;
mod vm_call_ops;
mod vm_comparison_ops;
mod vm_control_ops;
mod vm_data_ops;
mod vm_helpers;
mod vm_misc_ops;
mod vm_register_ops;
mod vm_set_ops;
mod vm_string_regex_ops;
mod vm_var_ops;

fn cmp_values(left: &Value, right: &Value) -> std::cmp::Ordering {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => a.cmp(b),
        (Value::Num(a), Value::Num(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
        (Value::Int(a), Value::Num(b)) => (*a as f64)
            .partial_cmp(b)
            .unwrap_or(std::cmp::Ordering::Equal),
        (Value::Num(a), Value::Int(b)) => a
            .partial_cmp(&(*b as f64))
            .unwrap_or(std::cmp::Ordering::Equal),
        _ => left.to_string_value().cmp(&right.to_string_value()),
    }
}

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
                if e.is_warn {
                    self.interpreter.write_warn_to_stderr(&e.message);
                    ip += 1;
                    continue;
                }
                return (self.interpreter, Err(e));
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        (self.interpreter, Ok(()))
    }

    /// Run compiled bytecode without consuming self.
    /// Used by map/grep to avoid VM creation/destruction per iteration.
    pub(crate) fn run_reuse(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        self.stack.clear();
        // Initialize local variable slots
        self.locals.resize(code.locals.len(), Value::Nil);
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            } else {
                self.locals[i] = Value::Nil;
            }
        }
        let mut ip = 0;
        while ip < code.ops.len() {
            if let Err(e) = self.exec_one(code, &mut ip, compiled_fns) {
                if e.is_warn {
                    self.interpreter.write_warn_to_stderr(&e.message);
                    ip += 1;
                    continue;
                }
                return Err(e);
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        Ok(())
    }

    /// Get a reference to the interpreter (for reading env values).
    pub(crate) fn interpreter(&self) -> &Interpreter {
        &self.interpreter
    }

    /// Get a mutable reference to the interpreter (for setting env values).
    pub(crate) fn interpreter_mut(&mut self) -> &mut Interpreter {
        &mut self.interpreter
    }

    /// Consume the VM and return the interpreter.
    pub(crate) fn into_interpreter(self) -> Interpreter {
        self.interpreter
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
                let val = self.get_env_with_main_alias(name).unwrap_or_else(|| {
                    if name.starts_with('^') {
                        Value::Bool(true)
                    } else {
                        Value::Nil
                    }
                });
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetArrayVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        // Fallback: check bare name in env (for closures capturing params)
                        name.strip_prefix('@')
                            .and_then(|bare| self.interpreter.env().get(bare).cloned())
                    })
                    .unwrap_or(Value::Nil);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetHashVar(name_idx) => {
                let name = Self::const_str(code, *name_idx);
                let val = self
                    .get_env_with_main_alias(name)
                    .or_else(|| self.get_local_by_bare_name(code, name))
                    .or_else(|| {
                        name.strip_prefix('%')
                            .and_then(|bare| self.interpreter.env().get(bare).cloned())
                    })
                    .unwrap_or(Value::Nil);
                self.stack.push(val);
                *ip += 1;
            }
            OpCode::GetBareWord(name_idx) => {
                self.exec_get_bare_word_op(code, *name_idx, compiled_fns)?;
                *ip += 1;
            }
            OpCode::GetPseudoStash(name_idx) => {
                self.exec_get_pseudo_stash_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::SetGlobal(name_idx) => {
                let name = match &code.constants[*name_idx as usize] {
                    Value::Str(s) => s.clone(),
                    _ => unreachable!("SetGlobal name must be a string constant"),
                };
                let val = self.stack.pop().unwrap();
                let val = if name.starts_with('%') {
                    runtime::coerce_to_hash(val)
                } else {
                    val
                };
                self.set_env_with_main_alias(&name, val);
                *ip += 1;
            }
            OpCode::SetTopic => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                self.interpreter.env_mut().insert("_".to_string(), val);
                *ip += 1;
            }

            // -- Arithmetic --
            OpCode::Add => {
                self.exec_add_op()?;
                *ip += 1;
            }
            OpCode::Sub => {
                self.exec_sub_op()?;
                *ip += 1;
            }
            OpCode::Mul => {
                self.exec_mul_op();
                *ip += 1;
            }
            OpCode::Div => {
                self.exec_div_op()?;
                *ip += 1;
            }
            OpCode::Mod => {
                self.exec_mod_op()?;
                *ip += 1;
            }
            OpCode::Pow => {
                self.exec_pow_op();
                *ip += 1;
            }
            OpCode::Negate => {
                self.exec_negate_op()?;
                *ip += 1;
            }
            OpCode::IntBitNeg => {
                self.exec_int_bit_neg_op();
                *ip += 1;
            }
            OpCode::BoolBitNeg => {
                self.exec_bool_bit_neg_op();
                *ip += 1;
            }
            OpCode::MakeSlip => {
                self.exec_make_slip_op();
                *ip += 1;
            }

            // -- Logic / coercion --
            OpCode::Not => {
                self.exec_not_op();
                *ip += 1;
            }
            OpCode::BoolCoerce => {
                self.exec_bool_coerce_op();
                *ip += 1;
            }

            // -- String --
            OpCode::Concat => {
                self.exec_concat_op();
                *ip += 1;
            }

            // -- Numeric comparison --
            OpCode::NumEq => {
                self.exec_num_eq_op()?;
                *ip += 1;
            }
            OpCode::NumNe => {
                self.exec_num_ne_op()?;
                *ip += 1;
            }
            OpCode::NumLt => {
                self.exec_num_lt_op()?;
                *ip += 1;
            }
            OpCode::NumLe => {
                self.exec_num_le_op()?;
                *ip += 1;
            }
            OpCode::NumGt => {
                self.exec_num_gt_op()?;
                *ip += 1;
            }
            OpCode::NumGe => {
                self.exec_num_ge_op()?;
                *ip += 1;
            }
            OpCode::ApproxEq => {
                self.exec_approx_eq_op()?;
                *ip += 1;
            }
            OpCode::ContainerEq => {
                self.exec_container_eq_op();
                *ip += 1;
            }

            // -- String comparison --
            OpCode::StrEq => {
                self.exec_str_eq_op()?;
                *ip += 1;
            }
            OpCode::StrNe => {
                self.exec_str_ne_op()?;
                *ip += 1;
            }
            OpCode::StrLt => {
                self.exec_str_lt_op()?;
                *ip += 1;
            }
            OpCode::StrGt => {
                self.exec_str_gt_op()?;
                *ip += 1;
            }
            OpCode::StrLe => {
                self.exec_str_le_op()?;
                *ip += 1;
            }
            OpCode::StrGe => {
                self.exec_str_ge_op()?;
                *ip += 1;
            }

            // -- Three-way comparison --
            OpCode::Spaceship => {
                self.exec_spaceship_op();
                *ip += 1;
            }
            OpCode::Before | OpCode::After => {
                let is_before = matches!(code.ops[*ip], OpCode::Before);
                self.exec_before_after_op(is_before);
                *ip += 1;
            }
            OpCode::Cmp => {
                self.exec_cmp_op();
                *ip += 1;
            }
            OpCode::Leg => {
                self.exec_leg_op();
                *ip += 1;
            }

            // -- Identity/value equality --
            OpCode::StrictEq => {
                self.exec_strict_eq_op();
                *ip += 1;
            }
            OpCode::Eqv => {
                self.exec_eqv_op()?;
                *ip += 1;
            }
            OpCode::SmartMatchExpr {
                rhs_end,
                negate,
                lhs_var,
            } => {
                self.exec_smart_match_expr_op(code, ip, *rhs_end, *negate, lhs_var, compiled_fns)?;
            }

            // -- Divisibility --
            OpCode::DivisibleBy => {
                self.exec_divisible_by_op()?;
                *ip += 1;
            }
            OpCode::NotDivisibleBy => {
                self.exec_not_divisible_by_op()?;
                *ip += 1;
            }

            // -- Keyword math --
            OpCode::IntDiv => {
                self.exec_int_div_op()?;
                *ip += 1;
            }
            OpCode::IntMod => {
                self.exec_int_mod_op()?;
                *ip += 1;
            }
            OpCode::Gcd => {
                self.exec_gcd_op();
                *ip += 1;
            }
            OpCode::Lcm => {
                self.exec_lcm_op();
                *ip += 1;
            }
            OpCode::InfixMin => {
                self.exec_infix_min_op();
                *ip += 1;
            }
            OpCode::InfixMax => {
                self.exec_infix_max_op();
                *ip += 1;
            }

            // -- Repetition --
            OpCode::StringRepeat => {
                self.exec_string_repeat_op();
                *ip += 1;
            }
            OpCode::ListRepeat => {
                self.exec_list_repeat_op();
                *ip += 1;
            }
            OpCode::FunctionCompose => {
                self.exec_function_compose_op();
                *ip += 1;
            }

            // -- Mixin / Type check --
            OpCode::ButMixin => {
                self.exec_but_mixin_op();
                *ip += 1;
            }
            OpCode::Isa => {
                self.exec_isa_op();
                *ip += 1;
            }
            OpCode::Does => {
                self.exec_does_op();
                *ip += 1;
            }

            // -- Pair --
            OpCode::MakePair => {
                self.exec_make_pair_op();
                *ip += 1;
            }

            // -- Bitwise --
            OpCode::BitAnd => {
                self.exec_bit_and_op();
                *ip += 1;
            }
            OpCode::BitOr => {
                self.exec_bit_or_op();
                *ip += 1;
            }
            OpCode::BitXor => {
                self.exec_bit_xor_op();
                *ip += 1;
            }
            OpCode::BitShiftLeft => {
                self.exec_bit_shift_left_op();
                *ip += 1;
            }
            OpCode::BitShiftRight => {
                self.exec_bit_shift_right_op();
                *ip += 1;
            }
            OpCode::BoolBitOr => {
                self.exec_bool_bit_or_op();
                *ip += 1;
            }
            OpCode::BoolBitAnd => {
                self.exec_bool_bit_and_op();
                *ip += 1;
            }
            OpCode::BoolBitXor => {
                self.exec_bool_bit_xor_op();
                *ip += 1;
            }

            // -- Set operations --
            OpCode::SetElem => {
                self.exec_set_elem_op();
                *ip += 1;
            }
            OpCode::SetCont => {
                self.exec_set_cont_op();
                *ip += 1;
            }
            OpCode::SetUnion => {
                self.exec_set_union_op();
                *ip += 1;
            }
            OpCode::SetIntersect => {
                self.exec_set_intersect_op();
                *ip += 1;
            }
            OpCode::SetDiff => {
                self.exec_set_diff_op();
                *ip += 1;
            }
            OpCode::SetSymDiff => {
                self.exec_set_sym_diff_op();
                *ip += 1;
            }
            OpCode::SetSubset => {
                self.exec_set_subset_op();
                *ip += 1;
            }
            OpCode::SetSuperset => {
                self.exec_set_superset_op();
                *ip += 1;
            }
            OpCode::SetStrictSubset => {
                self.exec_set_strict_subset_op();
                *ip += 1;
            }
            OpCode::SetStrictSuperset => {
                self.exec_set_strict_superset_op();
                *ip += 1;
            }
            OpCode::JunctionAny => {
                self.exec_junction_any_op();
                *ip += 1;
            }
            OpCode::JunctionAll => {
                self.exec_junction_all_op();
                *ip += 1;
            }
            OpCode::JunctionOne => {
                self.exec_junction_one_op();
                *ip += 1;
            }

            // -- Sequence --
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
                if !runtime::types::value_is_defined(val) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }
            OpCode::JumpIfNotNil(target) => {
                let val = self.stack.last().unwrap();
                if runtime::types::value_is_defined(val) {
                    *ip = *target as usize;
                } else {
                    *ip += 1;
                }
            }

            OpCode::CallDefined => {
                let val = self.stack.pop().unwrap();
                // Check if the value has a user-defined .defined method
                let class_name = match &val {
                    Value::Package(name) => Some(name.clone()),
                    Value::Instance { class_name, .. } => Some(class_name.clone()),
                    _ => None,
                };
                let has_user_defined = class_name
                    .as_ref()
                    .is_some_and(|cn| self.interpreter.has_user_method(cn, "defined"));
                let defined = if has_user_defined {
                    // Call user method directly, bypassing native method dispatch
                    let cn = class_name.unwrap();
                    let attrs = match &val {
                        Value::Instance { attributes, .. } => (**attributes).clone(),
                        _ => std::collections::HashMap::new(),
                    };
                    match self
                        .interpreter
                        .run_instance_method(&cn, attrs, "defined", Vec::new())
                    {
                        Ok((result, _)) => result,
                        Err(_) => Value::Bool(runtime::types::value_is_defined(&val)),
                    }
                } else {
                    Value::Bool(runtime::types::value_is_defined(&val))
                };
                self.stack.push(defined);
                *ip += 1;
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
                self.exec_make_range_op();
                *ip += 1;
            }
            OpCode::MakeRangeExcl => {
                self.exec_make_range_excl_op();
                *ip += 1;
            }
            OpCode::MakeRangeExclStart => {
                self.exec_make_range_excl_start_op();
                *ip += 1;
            }
            OpCode::MakeRangeExclBoth => {
                self.exec_make_range_excl_both_op();
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
            OpCode::Note(n) => {
                self.exec_note_op(*n)?;
                *ip += 1;
            }

            // -- Calls --
            OpCode::CallFunc { name_idx, arity } => {
                self.exec_call_func_op(code, *name_idx, *arity, compiled_fns)?;
                *ip += 1;
            }
            OpCode::CallFuncSlip {
                name_idx,
                regular_arity,
            } => {
                self.exec_call_func_slip_op(code, *name_idx, *regular_arity, compiled_fns)?;
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
            OpCode::ExecCallPairs { name_idx, arity } => {
                self.exec_exec_call_pairs_op(code, *name_idx, *arity)?;
                *ip += 1;
            }
            OpCode::ExecCallSlip {
                name_idx,
                regular_arity,
            } => {
                self.exec_exec_call_slip_op(code, *name_idx, *regular_arity)?;
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
                self.exec_num_coerce_op();
                *ip += 1;
            }
            OpCode::StrCoerce => {
                self.exec_str_coerce_op();
                *ip += 1;
            }
            OpCode::UptoRange => {
                self.exec_upto_range_op();
                *ip += 1;
            }

            // -- Prefix increment/decrement --
            OpCode::PreIncrement(name_idx) => {
                self.exec_pre_increment_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::PreDecrement(name_idx) => {
                self.exec_pre_decrement_op(code, *name_idx);
                *ip += 1;
            }

            // -- Variable access --
            OpCode::GetCaptureVar(name_idx) => {
                self.exec_get_capture_var_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::GetCodeVar(name_idx) => {
                self.exec_get_code_var_op(code, *name_idx);
                *ip += 1;
            }

            // -- Assignment as expression --
            OpCode::AssignExpr(name_idx) => {
                self.exec_assign_expr_op(code, *name_idx);
                *ip += 1;
            }

            // -- Loops --
            OpCode::WhileLoop {
                cond_end,
                body_end,
                label,
            } => {
                self.exec_while_loop_op(code, *cond_end, *body_end, label, ip, compiled_fns)?;
            }
            OpCode::ForLoop {
                param_idx,
                param_local,
                body_end,
                label,
                arity,
                collect,
            } => {
                let spec = vm_control_ops::ForLoopSpec {
                    param_idx: *param_idx,
                    param_local: *param_local,
                    body_end: *body_end,
                    label: label.clone(),
                    arity: *arity,
                    collect: *collect,
                };
                self.exec_for_loop_op(code, &spec, ip, compiled_fns)?;
            }
            OpCode::CStyleLoop {
                cond_end,
                step_start,
                body_end,
                label,
            } => {
                let spec = vm_control_ops::CStyleLoopSpec {
                    cond_end: *cond_end,
                    step_start: *step_start,
                    body_end: *body_end,
                    label: label.clone(),
                };
                self.exec_cstyle_loop_op(code, &spec, ip, compiled_fns)?;
            }

            // -- Given/When/Default --
            OpCode::Given { body_end } => {
                self.exec_given_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::When { body_end } => {
                self.exec_when_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::Default { body_end } => {
                self.exec_default_op(code, *body_end, ip, compiled_fns)?;
            }

            // -- Repeat loop --
            OpCode::RepeatLoop {
                cond_end,
                body_end,
                label,
            } => {
                self.exec_repeat_loop_op(code, *cond_end, *body_end, label, ip, compiled_fns)?;
            }

            // -- Exception handling --
            OpCode::TryCatch {
                catch_start,
                control_start,
                body_end,
            } => {
                self.exec_try_catch_op(
                    code,
                    *catch_start,
                    *control_start,
                    *body_end,
                    ip,
                    compiled_fns,
                )?;
            }

            // -- Error handling --
            OpCode::Die => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                return Err(RuntimeError::new(val.to_string_value()));
            }
            OpCode::Fail => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                let mut err = RuntimeError::new(val.to_string_value());
                err.is_fail = true;
                return Err(err);
            }
            OpCode::Return => {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                return Err(RuntimeError {
                    return_value: Some(val),
                    ..RuntimeError::new("")
                });
            }

            // -- Environment variable access --
            OpCode::GetEnvIndex(key_idx) => {
                self.exec_get_env_index_op(code, *key_idx);
                *ip += 1;
            }
            OpCode::ExistsEnvIndex(key_idx) => {
                self.exec_exists_env_index_op(code, *key_idx);
                *ip += 1;
            }
            OpCode::ExistsExpr => {
                self.exec_exists_expr_op();
                *ip += 1;
            }

            // -- Reduction --
            OpCode::Reduction(op_idx) => {
                self.exec_reduction_op(code, *op_idx)?;
                *ip += 1;
            }

            // -- Magic variables --
            OpCode::RoutineMagic => {
                self.exec_routine_magic_op()?;
                *ip += 1;
            }
            OpCode::BlockMagic => {
                self.exec_block_magic_op()?;
                *ip += 1;
            }

            // -- Substitution --
            OpCode::Subst {
                pattern_idx,
                replacement_idx,
            } => {
                self.exec_subst_op(code, *pattern_idx, *replacement_idx)?;
                *ip += 1;
            }
            OpCode::NonDestructiveSubst {
                pattern_idx,
                replacement_idx,
            } => {
                self.exec_non_destructive_subst_op(code, *pattern_idx, *replacement_idx)?;
                *ip += 1;
            }
            OpCode::Transliterate { from_idx, to_idx } => {
                self.exec_transliterate_op(code, *from_idx, *to_idx);
                *ip += 1;
            }

            // -- Take --
            OpCode::Take => {
                self.exec_take_op();
                *ip += 1;
            }

            // -- Package scope --
            OpCode::PackageScope { name_idx, body_end } => {
                self.exec_package_scope_op(code, *name_idx, *body_end, ip, compiled_fns)?;
            }

            // -- Phaser END --
            OpCode::PhaserEnd(idx) => {
                self.exec_phaser_end_op(code, *idx);
                *ip += 1;
            }

            // -- HyperMethodCall --
            OpCode::HyperMethodCall { name_idx, arity } => {
                self.exec_hyper_method_call_op(code, *name_idx, *arity)?;
                *ip += 1;
            }

            // -- HyperOp --
            OpCode::HyperOp {
                op_idx,
                dwim_left,
                dwim_right,
            } => {
                self.exec_hyper_op(code, *op_idx, *dwim_left, *dwim_right)?;
                *ip += 1;
            }

            // -- MetaOp --
            OpCode::MetaOp { meta_idx, op_idx } => {
                self.exec_meta_op(code, *meta_idx, *op_idx)?;
                *ip += 1;
            }

            // -- InfixFunc --
            OpCode::InfixFunc {
                name_idx,
                right_arity,
                modifier_idx,
            } => {
                self.exec_infix_func_op(code, *name_idx, *right_arity, modifier_idx)?;
                *ip += 1;
            }

            // -- Type checking --
            OpCode::TypeCheck(tc_idx) => {
                self.exec_type_check_op(code, *tc_idx)?;
                *ip += 1;
            }
            OpCode::EvalAstExpr(stmt_idx) => {
                self.exec_eval_ast_expr_op(code, *stmt_idx)?;
                *ip += 1;
            }
            OpCode::IndirectTypeLookup => {
                self.exec_indirect_type_lookup_op();
                *ip += 1;
            }
            OpCode::IndirectCodeLookup(name_idx) => {
                self.exec_indirect_code_lookup_op(code, *name_idx);
                *ip += 1;
            }
            OpCode::StateVarInit(slot, key_idx) => {
                self.exec_state_var_init_op(code, *slot, *key_idx);
                *ip += 1;
            }

            // -- Block scope --
            OpCode::BlockScope {
                enter_end,
                body_end,
                end,
            } => {
                self.exec_block_scope_op(code, *enter_end, *body_end, *end, ip, compiled_fns)?;
            }
            OpCode::DoBlockExpr { body_end, label } => {
                self.exec_do_block_expr_op(code, *body_end, label, ip, compiled_fns)?;
            }
            OpCode::DoGivenExpr { body_end } => {
                self.exec_do_given_expr_op(code, *body_end, ip, compiled_fns)?;
            }

            // -- Closures and registration --
            OpCode::MakeGather(idx) => {
                self.exec_make_gather_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::MakeAnonSub(idx) => {
                self.exec_make_anon_sub_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::MakeAnonSubParams(idx) => {
                self.exec_make_anon_sub_params_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::MakeLambda(idx) => {
                self.exec_make_lambda_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::IndexAssignInvalid => {
                return Err(RuntimeError::new("Invalid assignment target"));
            }
            OpCode::MakeBlockClosure(idx) => {
                self.exec_make_block_closure_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterSub(idx) => {
                self.exec_register_sub_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterToken(idx) => {
                self.exec_register_token_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterProtoSub(idx) => {
                self.exec_register_proto_sub_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterProtoToken(idx) => {
                self.exec_register_proto_token_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::UseModule(name_idx) => {
                self.exec_use_module_op(code, *name_idx)?;
                *ip += 1;
            }
            OpCode::UseLibPath => {
                self.exec_use_lib_path_op(code);
                *ip += 1;
            }
            OpCode::RegisterEnum(idx) => {
                self.exec_register_enum_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterClass(idx) => {
                self.exec_register_class_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterRole(idx) => {
                self.exec_register_role_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::RegisterSubset(idx) => {
                self.exec_register_subset_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::SubtestScope { body_end } => {
                self.exec_subtest_scope_op(code, *body_end, ip, compiled_fns)?;
            }
            OpCode::WheneverScope {
                body_idx,
                param_idx,
                target_var_idx,
            } => {
                self.exec_whenever_scope_op(code, *body_idx, param_idx, target_var_idx)?;
                *ip += 1;
            }

            // -- Local variables --
            OpCode::GetLocal(idx) => {
                self.exec_get_local_op(*idx);
                *ip += 1;
            }
            OpCode::SetLocal(idx) => {
                self.exec_set_local_op(code, *idx)?;
                *ip += 1;
            }
            OpCode::AssignExprLocal(idx) => {
                self.exec_assign_expr_local_op(code, *idx);
                *ip += 1;
            }
            OpCode::AssignReadOnly => {
                return Err(RuntimeError::new("X::Assignment::RO"));
            }

            // -- Let scope management --
            OpCode::LetSave {
                name_idx,
                index_mode,
            } => {
                self.exec_let_save_op(code, *name_idx, *index_mode);
                *ip += 1;
            }
            OpCode::LetBlock { body_end } => {
                self.exec_let_block_op(code, *body_end, ip, compiled_fns)?;
            }
        }
        Ok(())
    }

    /// Check if a value represents a "successful" block exit for `let` purposes.
    fn is_let_success(val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Package(name) if name == "Mu" || name == "Any" => false,
            _ => true,
        }
    }
}
