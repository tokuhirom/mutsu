//! The static half of the replay-safety proof (ADR-18012 §2.3): a `BEGIN` body
//! is recorded only if every opcode in it is one of a small set whose effect
//! stays inside the body's own values and the unit lexicals the recorder
//! compares.
//!
//! Left out on purpose, so a body using any of them runs on every load:
//! - declarations (`RegisterDecl`, routine and type declarations): they change
//!   registries the log does not hold;
//! - closures and nested phasers: they run later, or escape into values;
//! - any read of a global, `our`, package or dynamic variable: a write into
//!   what such a read returns would reach shared state;
//! - output, `use`/`require`, `EVAL` and everything else not listed.
//!
//! A call is admitted only by name: a method, `nqp::` op or routine whose
//! built-in form computes a value and writes nothing shared. If the receiver
//! turns out to be a user object, or the built-in is `augment`ed, the call
//! runs other compiled code and [`super::watch`] refuses the body.

use crate::opcode::{CompiledCode, OpCode};

/// Methods whose built-in forms only compute a value from the receiver and
/// arguments (ADR-18012's `EFFECT_FREE` set for S1).
const PURE_METHODS: &[&str] = &[
    "uniname",
    "starts-with",
    "ends-with",
    "contains",
    "comb",
    "unique",
    "keys",
    "values",
    "sort",
    "join",
    "lc",
    "uc",
    "fc",
    "chars",
    "elems",
    "Str",
    "Int",
];

/// `nqp::` ops that compute a value or edit an object the body holds. An edit
/// of an object a unit lexical holds is seen by the recorder's comparison,
/// which is why the recorder refuses a body when a unit lexical holds a value
/// it cannot compare (anything but a type object).
const PURE_NQP_OPS: &[&str] = &[
    "hash", "list", "list_i", "list_s", "atkey", "bindkey", "existskey", "atpos", "atpos_i",
    "bindpos", "bindpos_i", "push", "push_i", "push_s", "elems", "lc", "uc", "chars", "ord",
    "chr", "add_i", "sub_i", "mul_i", "iseq_i", "isne_i", "islt_i", "isgt_i", "isle_i", "isge_i",
    "isnull", "ifnull", "null", "eqat", "substr", "index", "join", "split", "create",
    "p6bindattrinvres",
];

/// Whether the body `start..end` of `code` uses only the operations above.
// Cost: O(n), n = opcodes in the body (each checked once; names by table scan
// over the fixed lists above).
pub(crate) fn body_is_recordable(code: &CompiledCode, start: usize, end: usize) -> bool {
    let Some(ops) = code.ops.get(start..end) else {
        return false;
    };
    // Array variables the body declares: `GetArrayVar` reads one by name, and
    // only the body's own may be read.
    let declared: Vec<u32> = ops
        .iter()
        .filter_map(|op| match op {
            OpCode::SetVarDynamic {
                name_idx,
                dynamic: false,
                ..
            } => Some(*name_idx),
            _ => None,
        })
        .collect();
    match ops.iter().find(|op| !op_is_recordable(code, op, &declared)) {
        None => true,
        Some(op) => {
            super::trace(&format!("a BEGIN body is not recorded: it uses {op:?}"));
            false
        }
    }
}

// Cost: O(1) (fixed-size name lists).
fn op_is_recordable(code: &CompiledCode, op: &OpCode, declared: &[u32]) -> bool {
    let named = |idx: &u32, list: &[&str]| {
        code.constants
            .get(*idx as usize)
            .and_then(|v| v.as_str())
            .is_some_and(|name| list.contains(&name))
    };
    match op {
        OpCode::LoadConst(_)
        | OpCode::GetLocal(_)
        | OpCode::SetLocal(_)
        | OpCode::SetLocalDecl { .. }
        | OpCode::CheckReadOnly(_)
        | OpCode::Pop
        | OpCode::Jump(_)
        | OpCode::JumpIfFalse(_)
        | OpCode::JumpIfNotNil(_)
        | OpCode::MakeRange
        | OpCode::Concat
        | OpCode::StrCoerce
        | OpCode::Add
        | OpCode::Sub
        | OpCode::Mul
        | OpCode::NativeIntArithmetic { .. }
        | OpCode::ContainerizePair
        | OpCode::MarkBindContext
        | OpCode::MarkVarDeclContext
        | OpCode::MarkScalarBindContext
        | OpCode::MarkAccessorRefContext
        | OpCode::TypeCheck(..)
        | OpCode::SetVarType { .. }
        | OpCode::SetVarTypeHoisted { .. }
        | OpCode::SetVarTypeScoped { .. }
        | OpCode::RestoreForParam
        | OpCode::ImportScope { .. }
        | OpCode::NqpCreateC(_)
        | OpCode::NqpAttrC(_) => true,
        OpCode::SetVarDynamic { dynamic, .. } => !dynamic,
        OpCode::GetArrayVar(name_idx) => declared.contains(name_idx),
        OpCode::LoadRegexClosure { captures, .. } => captures.is_empty(),
        OpCode::ForLoop(spec) => spec.block_callable_local.is_none() && !spec.threaded,
        OpCode::CallMethod { name_idx, .. } | OpCode::CallMethodMut { name_idx, .. } => {
            named(name_idx, PURE_METHODS)
        }
        OpCode::NqpOp { id, .. } => {
            PURE_NQP_OPS.contains(&crate::nqp_op_ids::nqp_op_name(*id))
        }
        // The call whose result `SinkPopCall` sinks is one of the ops above;
        // its operand only names it for a diagnostic.
        OpCode::SinkPopCall(_) => true,
        _ => false,
    }
}

/// The frame slots the body `start..end` declares itself: its own `my`
/// variables and loop parameters. Every other slot is a unit lexical the body
/// may change, which the recorder compares.
// Cost: O(n), n = opcodes in the body.
pub(crate) fn body_declared_slots(code: &CompiledCode, start: usize, end: usize) -> Vec<usize> {
    let mut slots = Vec::new();
    for op in code.ops.get(start..end).unwrap_or_default() {
        match op {
            OpCode::SetLocalDecl { slot, .. } => slots.push(*slot as usize),
            OpCode::SetVarDynamic {
                local_slot: Some(slot),
                ..
            } => slots.push(*slot as usize),
            OpCode::ForLoop(spec) => slots.extend(spec.param_local.map(|s| s as usize)),
            _ => {}
        }
    }
    slots
}
