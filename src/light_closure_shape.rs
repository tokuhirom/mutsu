//! Whether a closure body can run through the light closure call (#8335,
//! `vm/vm_closure_light_call.rs`), settled once per code object from its
//! bytecode alone.

use crate::opcode::{CompiledCode, OpCode};

/// A closure body the light call can run, and what the call must supply.
#[derive(Debug, Clone)]
pub(crate) struct LightClosureShape {
    /// The upvalue indices the body reads. Each must be a captured cell or
    /// value of the closure being called; a missing one is read by name on
    /// the untyped entry.
    pub(crate) upvalues: Box<[u32]>,
}

impl LightClosureShape {
    /// The shape of `cc`'s body, or `None` when the light call cannot run it.
    // Cost: O(n + l), n = ops, l = locals.
    pub(crate) fn of(cc: &CompiledCode) -> Option<Self> {
        // A WhateverCode's single `*` is its topic parameter `_`, so its body
        // reads `$_` -- through that parameter's slot, which is the read the
        // local check below admits. Any other `$_` read reaches a name.
        let topic_is_param = cc.is_whatever_code && cc.locals.iter().any(|l| l == "_");
        if !(cc.is_pointy_block || cc.is_whatever_code)
            || cc.is_routine
            || cc.is_supply_block_body
            || cc.static_name_lookup
            || cc.has_calls
            || cc.has_env_writes
            || cc.uses_capture
            || cc.reads_args_array
            || cc.reads_args_hash
            || (cc.reads_topic && !topic_is_param)
            || cc.writes_topic
            || cc.uses_routine_state
            || cc.sets_routine_state
            || !cc.state_locals.is_empty()
            || !cc.free_var_writes.is_empty()
            || !cc.free_var_container_writes.is_empty()
        {
            return None;
        }
        // Every local is a parameter (checked per call against the signature),
        // and every read of one is the plain slot read.
        for (i, name) in cc.locals.iter().enumerate() {
            if name.is_empty() || name.starts_with('!') || !cc.local_read_plain(i) {
                return None;
            }
        }
        let mut upvalues = Vec::new();
        for op in &cc.ops {
            match op {
                OpCode::GetUpvalue { index, .. } => upvalues.push(*index),
                OpCode::GetLocal(idx) if (*idx as usize) < cc.locals.len() => {}
                op if Self::op_is_pure(op) => {}
                _ => return None,
            }
        }
        upvalues.sort_unstable();
        upvalues.dedup();
        Some(LightClosureShape {
            upvalues: upvalues.into_boxed_slice(),
        })
    }

    /// The ops that take their operands from the stack and push a result,
    /// touching no variable, frame or control flow outside the body.
    fn op_is_pure(op: &OpCode) -> bool {
        matches!(
            op,
            OpCode::LoadConst(_)
                | OpCode::LoadNil
                | OpCode::LoadTrue
                | OpCode::LoadFalse
                | OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Mod
                | OpCode::Pow
                | OpCode::Negate
                | OpCode::IntDiv
                | OpCode::IntMod
                | OpCode::InfixMin
                | OpCode::InfixMax
                | OpCode::Not
                | OpCode::BoolCoerce
                | OpCode::NumCoerce
                | OpCode::StrCoerce
                | OpCode::Concat
                | OpCode::NumEq
                | OpCode::NumNe
                | OpCode::NumLt
                | OpCode::NumLe
                | OpCode::NumGt
                | OpCode::NumGe
                | OpCode::StrEq
                | OpCode::StrNe
                | OpCode::StrLt
                | OpCode::StrGt
                | OpCode::StrLe
                | OpCode::StrGe
                | OpCode::Spaceship
                | OpCode::Cmp
                | OpCode::Leg
                | OpCode::BitAnd
                | OpCode::BitOr
                | OpCode::BitXor
                | OpCode::BitShiftLeft
                | OpCode::BitShiftRight
                | OpCode::IntBitNeg
                | OpCode::Jump(_)
                | OpCode::JumpIfFalse(_)
                | OpCode::JumpIfTrue(_)
                | OpCode::Dup
                | OpCode::Pop
        )
    }
}
