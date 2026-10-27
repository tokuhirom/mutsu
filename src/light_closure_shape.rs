//! Whether a closure body can run through the light closure call (#8335,
//! `vm/vm_closure_light_call.rs`), settled once per code object from its
//! bytecode alone.

use crate::ast::ParamDef;
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

/// Whether every `ParamDef` is a `$^name` placeholder with nothing else on it
/// (`{ $^a + $^b }`), a signature the light closure *call* binds as the
/// general binder does: each argument itemized, in order (#13286). The light
/// closure bind does not serve it: the binder also publishes the twigil-less
/// alias and the call marks both names readonly, which only a body that
/// reaches names can observe.
// Cost: O(p + t), p = parameter count, t = the parameters' total trait count.
pub(crate) fn placeholder_defs_are_plain(param_defs: &[ParamDef]) -> bool {
    !param_defs.is_empty()
        && param_defs.iter().all(|pd| {
            def_is_plain_positional(pd)
                && pd.no_traits()
                && pd
                    .name
                    .strip_prefix('^')
                    .is_some_and(|bare| is_plain_scalar_param_name(bare) && !bare.starts_with('_'))
        })
}

/// Whether `pd`, whatever its name, is a positional parameter with no
/// default, type, type capture, literal, `where`, sub-signature, code
/// signature or shape, and with no trait but `is raw`.
// Cost: O(t), t = the parameter's trait count.
pub(crate) fn def_is_plain_positional(pd: &ParamDef) -> bool {
    let ParamDef {
        trait_meta: _,
        name: _,
        target: _,
        default,
        multi_invocant: _,
        required: _,
        named,
        named_alias,
        slurpy,
        double_slurpy,
        onearg,
        sigilless,
        type_constraint,
        type_capture,
        literal_value,
        sub_signature,
        where_constraint,
        traits,
        trait_args,
        optional_marker,
        outer_sub_signature,
        code_signature,
        is_invocant,
        // A `does` target is written back through its argument's container.
        does_target,
        implicit_invocant: _,
        shape_constraints,
        // Only the implicit nominal type of an *unpassed* optional depends on
        // it, and an optional parameter is refused below.
        block_param: _,
        // The compiled `where` / default / shape chunks; their source
        // expressions are refused below.
        code: _,
        spans: _,
    } = pd;
    !named
        && !named_alias
        && !slurpy
        && !double_slurpy
        && !onearg
        && !sigilless
        && !optional_marker
        && !is_invocant
        && !does_target
        && default.is_none()
        && type_constraint.is_none()
        && type_capture.is_none()
        && literal_value.is_none()
        && sub_signature.is_none()
        && where_constraint.is_none()
        && outer_sub_signature.is_none()
        && code_signature.is_none()
        && shape_constraints.is_none()
        && trait_args.is_empty()
        && traits.iter().all(|t| t == "raw")
}

/// Whether a `ParamDef::name` is a plain `$` scalar's sigil-less name.
///
/// That rejects every sigil (`@x`, `%x`, `&x`) and twigil (`!x`, `.x`, `^x`,
/// `:x`), the topic `_`, `self` (which the binder mirrors onto the reserved
/// lexical key) and the parser's synthetic `__type_only__` / `__subsig__` /
/// `__type_capture__…` names. A WhateverCode's `__wc_N` and other
/// `_`-prefixed identifiers are accepted: their itemization (or exemption from
/// it) is `Interpreter::scalar_param_bind`'s to decide.
// Cost: O(n), n = the name's length.
pub(crate) fn is_plain_scalar_param_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
        && name != "_"
        && name != "self"
        && name != "__type_only__"
        && name != "__subsig__"
        && !name.starts_with("__type_capture__")
}
