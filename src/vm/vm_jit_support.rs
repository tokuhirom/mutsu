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
        OpCode::BitAnd => helpers::bit_and,
        OpCode::BitOr => helpers::bit_or,
        OpCode::BitXor => helpers::bit_xor,
        OpCode::BitShiftLeft => helpers::bit_shift_left,
        OpCode::BitShiftRight => helpers::bit_shift_right,
        OpCode::IntBitNeg => helpers::int_bit_neg,
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
            | OpCode::SetVarTypeScoped { .. }
            | OpCode::AssignExpr(_)
            | OpCode::TopicDotAssign(_)
            | OpCode::AtomicCompoundVar { .. }
            | OpCode::IndexAssignExprNamed { .. }
            | OpCode::WrapVarRef { .. }
            | OpCode::LetSave { .. }
            | OpCode::CheckReadOnly(_)
            | OpCode::MarkSigillessBind(_)
            | OpCode::MarkSigillessBindSource(_)
            | OpCode::MarkVarReadonly(..)
            | OpCode::CheckDynamicVarDeclared(_)
            // Increment / decrement
            | OpCode::PostIncrement(..)
            | OpCode::PostDecrement(..)
            | OpCode::PreIncrement(..)
            | OpCode::PreDecrement(..)
            | OpCode::PreIncrementIndex(..)
            | OpCode::PreDecrementIndex(..)
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
            | OpCode::MakeNamedArg
            | OpCode::CoerceToList
            | OpCode::Itemize
            | OpCode::DeitemizeZen
            | OpCode::Decont
            | OpCode::DerefContainer
            | OpCode::DecontListElems
            | OpCode::Index { .. }
            | OpCode::IndexAutovivifyLazy { .. }
            // String / bool / numeric helpers
            | OpCode::StringConcat(_)
            | OpCode::StrCoerce
            | OpCode::BoolCoerce
            | OpCode::Not
            | OpCode::Gcd
            | OpCode::Lcm
            | OpCode::NumCoerce
            // METAOP_ASSIGN identity seed -- straight-line; throws only for the
            // `/=` / `%=` no-zero-argument case, which propagates like any other
            // fallible step shim. Must stay listed: it sits in every `$i += 1`.
            | OpCode::MetaAssignIdentity(_)
            | OpCode::GetLocalMetaAssign { .. }
            // Sink context (forces lazies / throws unhandled Failures)
            | OpCode::SinkPop(_, _)
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
            // `state` variable one-time initialization store, guarded by
            // `StateVarInitGuard` (see the explicit arm in
            // `vm_jit_compile.rs::compile_range`/`build`). Infallible,
            // straight-line -- never touches `ip` beyond the `+= 1` its own
            // `exec_one` arm does.
            | OpCode::StateVarInit(..)
            // Always-throwing terminator (records its own resume point)
            | OpCode::Die
    )
}
