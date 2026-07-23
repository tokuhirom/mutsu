//! Tier A opcode support tables (ADR-0004 §2.3): which opcodes the JIT can
//! translate, and through which shim. Split out of `vm_jit_compile.rs` purely
//! by size; the acceptance rules are unchanged — a chunk containing any
//! opcode outside these tables (and the explicit arms in `compile_chunk`)
//! bails out and stays on the interpreter forever.

use super::vm_jit_helpers as helpers;
use super::*;

/// Payload-free fallible opcodes with a dedicated `(interp) -> status` shim
/// (the hot arith / compare / string family, plus `Return`). Returns the shim
/// address for emission, `None` when the opcode is not in this family.
pub(super) fn noarg_shim(op: &OpCode) -> Option<usize> {
    let f: unsafe extern "C" fn(*mut Interpreter) -> u32 = match op {
        OpCode::Add => helpers::add,
        OpCode::Sub => helpers::sub,
        OpCode::Mul => helpers::mul,
        OpCode::Div => helpers::div,
        OpCode::Mod => helpers::modulo,
        OpCode::IntDiv => helpers::int_div,
        OpCode::IntMod => helpers::int_mod,
        OpCode::Pow => helpers::pow,
        OpCode::Negate => helpers::negate,
        OpCode::NumLt => helpers::num_lt,
        OpCode::NumLe => helpers::num_le,
        OpCode::NumGt => helpers::num_gt,
        OpCode::NumGe => helpers::num_ge,
        OpCode::NumEq => helpers::num_eq,
        OpCode::NumNe => helpers::num_ne,
        OpCode::Concat => helpers::concat,
        OpCode::StrEq => helpers::str_eq,
        OpCode::StrNe => helpers::str_ne,
        OpCode::Return => helpers::ret,
        _ => return None,
    };
    Some(f as *const () as usize)
}

/// Straight-line opcodes without a dedicated shim, executed through the
/// generic `helpers::step` (one interpreter dispatch per opcode). Every entry
/// is verified against its `exec_one` arm to unconditionally leave
/// `ip == start + 1` on Ok — no jumps, no compound-loop bodies, no arms that
/// consult or rewrite `ip` beyond the increment. Anything not provably
/// straight-line stays OFF this list and bails the chunk out.
pub(super) fn step_supported(op: &OpCode) -> bool {
    matches!(
        op,
        // Constants / stack shape
        OpCode::LoadNil
            | OpCode::LoadTrue
            | OpCode::LoadFalse
            | OpCode::Dup
            | OpCode::Pop
            // Variable reads
            | OpCode::GetGlobal(_)
            | OpCode::GetOurVar(_)
            | OpCode::GetArrayVar(_)
            | OpCode::GetHashVar(_)
            | OpCode::GetBareWord(_)
            | OpCode::GetCaptureVar(_)
            | OpCode::GetCodeVar(_)
            | OpCode::GetSelfOrNoSelf(_)
            | OpCode::GetUpvalue { .. }
            // Variable writes / declarations
            | OpCode::SetGlobal(_)
            | OpCode::SetGlobalRaw(_)
            | OpCode::SetVarDynamic { .. }
            | OpCode::SetVarType { .. }
            | OpCode::AssignExpr(_)
            | OpCode::TopicDotAssign(_)
            | OpCode::AtomicCompoundVar { .. }
            | OpCode::IndexAssignExprNamed { .. }
            | OpCode::WrapVarRef(_)
            | OpCode::LetSave { .. }
            | OpCode::CheckReadOnly(_)
            | OpCode::MarkVarReadonly(_)
            | OpCode::CheckDynamicVarDeclared(_)
            // Increment / decrement
            | OpCode::PostIncrement(..)
            | OpCode::PostDecrement(..)
            | OpCode::PreIncrement(..)
            | OpCode::PreDecrement(..)
            | OpCode::PreIncrementIndex(_)
            | OpCode::PreDecrementIndex(_)
            // Arith predicates
            | OpCode::DivisibleBy
            | OpCode::NotDivisibleBy
            // Closure construction
            | OpCode::MakeLambda(..)
            | OpCode::MakeAnonSub(..)
            | OpCode::MakeAnonSubParams(..)
            | OpCode::MakeGather(..)
            // Calls through a code variable (re-entrant, like CallMethod)
            | OpCode::CallOnCodeVar { .. }
            | OpCode::ExecCallPairs { .. }
            // In-place container mutation
            | OpCode::ArrayPush { .. }
            | OpCode::TagContainerRef(..)
            | OpCode::TagContainerRefReversed(..)
            | OpCode::MarkAccessorRefContext
            // List / hash construction, indexing and coercion
            | OpCode::MakeArray(_)
            | OpCode::MakeRealArray(_)
            | OpCode::MakeRealArrayNoFlatten(_)
            | OpCode::MakeHash(_)
            | OpCode::MakePair
            | OpCode::CoerceToList
            | OpCode::Itemize
            | OpCode::DeitemizeZen
            | OpCode::Decont
            | OpCode::DecontListElems
            | OpCode::Index { .. }
            | OpCode::IndexAutovivifyLazy
            // String / bool / numeric helpers
            | OpCode::StringConcat(_)
            | OpCode::StrCoerce
            | OpCode::BoolCoerce
            | OpCode::Not
            | OpCode::Gcd
            | OpCode::Lcm
            | OpCode::NumCoerce
            // Sink context (forces lazies / throws unhandled Failures)
            | OpCode::SinkPop(_)
            // Topic / context markers
            | OpCode::MarkBindContext
            | OpCode::MarkVarDeclContext
            | OpCode::MarkExplicitInitializerContext
            | OpCode::MarkShapedDeclContext
            | OpCode::MarkArrayShareSource(_)
            | OpCode::SetTopic
            | OpCode::SaveTopic
            | OpCode::RestoreTopic
            | OpCode::EnterPointyTopic
            | OpCode::ExitPointyTopic
            | OpCode::PushEnterResult
            | OpCode::LoadEnterResult
            // Always-throwing terminator (records its own resume point)
            | OpCode::Die
    )
}
