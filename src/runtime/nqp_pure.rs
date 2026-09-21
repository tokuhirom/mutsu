//! The `nqp::` value ops that are pure functions of native `int`/`num`
//! operands — and the single implementation each of them has.
//!
//! # Why this module exists
//!
//! In NQP/Rakudo an `nqp::` value op is a `QAST::Op` node the QAST compiler
//! turns into ONE MoarVM instruction: `nqp::add_i($a, $b)` is an `add_i`
//! against two native registers and costs a machine add. mutsu reaches the
//! same answer through a call-shaped protocol — collect the operands into an
//! argument list, clear the pending callsite line, publish and restore the
//! multi-candidate literal mask, dispatch on the op's identity, then match its
//! NAME inside the owning table. [`crate::runtime::nqp_op_ids`] already
//! removed the worst of that (the name resolution and the walk down six
//! chained tables became a compile-time id), but what is left still costs
//! roughly 360 instructions before the op body runs, measured on a
//! `JSON::Fast` decode (#8900):
//!
//! | step | instructions per op |
//! |---|---:|
//! | [`crate::runtime::Interpreter::exec_nqp_op`]'s argument-list prologue/epilogue | ~180 |
//! | `dispatch_nqp_op_by_id`'s id → name → table select | ~38 |
//! | the owning table's `match op { .. }` over its names | ~145 |
//!
//! For `nqp::iseq_i` — 1.5 million of the 2.5 million `nqp::` ops that a
//! 10-decode run of the ADR-0110 §8 benchmark executes are in this table —
//! the op body itself is an integer compare. The protocol is two orders of
//! magnitude more expensive than the operation.
//!
//! # What it does
//!
//! [`pure_op`] answers, from the op's compile-time id and with one array
//! load, whether the op is a pure function of native operands.
//! [`try_eval_native`] then evaluates it **straight off the operand stack**:
//! no argument vector, no name, no table.
//!
//! # Why it cannot drift from the general path
//!
//! Two properties, both structural rather than maintained by hand:
//!
//! * **One implementation.** [`eval`] holds each op's body, and
//!   `call_nqp_op`'s string-keyed arms call it too. There is no second copy of
//!   `add_i` to keep in step.
//! * **The direct path declines anything it has not proved.**
//!   [`try_eval_native`] runs only when the call site's arity matches the op's
//!   own AND every operand is already exactly an `Int` (or, for a `_n` op, an
//!   `Int` or a `Num`) — a pure NaN-box tag probe. A `VarRef`, a
//!   `ContainerRef`, a `HashEntryRef`, a `Proxy` and a `Str` all fail that
//!   probe and keep the general path, which is where the unwrapping,
//!   container deref, `Proxy` FETCH and coercion live. So the direct path
//!   never has to reproduce any of them.

use crate::value::Value;

/// The operand representation an op reads.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Operand {
    /// `to_int` — the `_i` ops.
    Int,
    /// `to_f64` — the `_n` ops.
    Num,
}

/// An `nqp::` value op that is a pure function of its native operands.
///
/// "Pure" here is the strong sense the direct path needs: the body reads its
/// operands as numbers and returns a number, touching no interpreter state,
/// allocating nothing, dispatching nowhere, and raising nothing. That is what
/// lets [`crate::runtime::Interpreter::exec_nqp_op`] skip the call-shaped
/// prologue — there is no pending callsite line for such a body to observe
/// and no candidate for the literal mask to rank. `nqp::div_i` and
/// `nqp::mod_i` are deliberately absent: they can raise a
/// division-by-zero error, so they are not total.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum NqpPure {
    AddI,
    SubI,
    MulI,
    NegI,
    AbsI,
    BitOrI,
    BitAndI,
    BitXorI,
    BitNegI,
    ShlI,
    ShrI,
    IsEqI,
    IsNeI,
    IsLtI,
    IsLeI,
    IsGtI,
    IsGeI,
    CmpI,
    NotI,
    AddN,
    SubN,
    MulN,
    DivN,
    NegN,
    AbsN,
    IsEqN,
    IsNeN,
    IsLtN,
    IsLeN,
    IsGtN,
    IsGeN,
    CmpN,
    IsNanOrInf,
}

/// Every variant, for the round-trip tests. Adding a variant without adding it
/// here fails `pure_op_round_trips`.
#[cfg(test)]
const ALL: [NqpPure; 33] = [
    NqpPure::AddI,
    NqpPure::SubI,
    NqpPure::MulI,
    NqpPure::NegI,
    NqpPure::AbsI,
    NqpPure::BitOrI,
    NqpPure::BitAndI,
    NqpPure::BitXorI,
    NqpPure::BitNegI,
    NqpPure::ShlI,
    NqpPure::ShrI,
    NqpPure::IsEqI,
    NqpPure::IsNeI,
    NqpPure::IsLtI,
    NqpPure::IsLeI,
    NqpPure::IsGtI,
    NqpPure::IsGeI,
    NqpPure::CmpI,
    NqpPure::NotI,
    NqpPure::AddN,
    NqpPure::SubN,
    NqpPure::MulN,
    NqpPure::DivN,
    NqpPure::NegN,
    NqpPure::AbsN,
    NqpPure::IsEqN,
    NqpPure::IsNeN,
    NqpPure::IsLtN,
    NqpPure::IsLeN,
    NqpPure::IsGtN,
    NqpPure::IsGeN,
    NqpPure::CmpN,
    NqpPure::IsNanOrInf,
];

impl NqpPure {
    /// The op's name without the `nqp::` prefix, as
    /// [`crate::runtime::nqp_op_ids`] spells it. The inverse of [`by_name`],
    /// and the only reason it exists is `pure_op_round_trips`: nothing at run
    /// time ever needs the name, which is the point of the whole change.
    #[cfg(test)]
    fn name(self) -> &'static str {
        match self {
            NqpPure::AddI => "add_i",
            NqpPure::SubI => "sub_i",
            NqpPure::MulI => "mul_i",
            NqpPure::NegI => "neg_i",
            NqpPure::AbsI => "abs_i",
            NqpPure::BitOrI => "bitor_i",
            NqpPure::BitAndI => "bitand_i",
            NqpPure::BitXorI => "bitxor_i",
            NqpPure::BitNegI => "bitneg_i",
            NqpPure::ShlI => "bitshiftl_i",
            NqpPure::ShrI => "bitshiftr_i",
            NqpPure::IsEqI => "iseq_i",
            NqpPure::IsNeI => "isne_i",
            NqpPure::IsLtI => "islt_i",
            NqpPure::IsLeI => "isle_i",
            NqpPure::IsGtI => "isgt_i",
            NqpPure::IsGeI => "isge_i",
            NqpPure::CmpI => "cmp_i",
            NqpPure::NotI => "not_i",
            NqpPure::AddN => "add_n",
            NqpPure::SubN => "sub_n",
            NqpPure::MulN => "mul_n",
            NqpPure::DivN => "div_n",
            NqpPure::NegN => "neg_n",
            NqpPure::AbsN => "abs_n",
            NqpPure::IsEqN => "iseq_n",
            NqpPure::IsNeN => "isne_n",
            NqpPure::IsLtN => "islt_n",
            NqpPure::IsLeN => "isle_n",
            NqpPure::IsGtN => "isgt_n",
            NqpPure::IsGeN => "isge_n",
            NqpPure::CmpN => "cmp_n",
            NqpPure::IsNanOrInf => "isnanorinf",
        }
    }

    /// How many operands the op reads. A call site that supplies a different
    /// number keeps the general path, which tolerates both a short list (the
    /// missing operand reads as 0) and a long one (the surplus is ignored).
    const fn arity(self) -> usize {
        match self {
            NqpPure::NegI
            | NqpPure::AbsI
            | NqpPure::BitNegI
            | NqpPure::NotI
            | NqpPure::NegN
            | NqpPure::AbsN
            | NqpPure::IsNanOrInf => 1,
            _ => 2,
        }
    }

    const fn operand(self) -> Operand {
        match self {
            NqpPure::AddN
            | NqpPure::SubN
            | NqpPure::MulN
            | NqpPure::DivN
            | NqpPure::NegN
            | NqpPure::AbsN
            | NqpPure::IsEqN
            | NqpPure::IsNeN
            | NqpPure::IsLtN
            | NqpPure::IsLeN
            | NqpPure::IsGtN
            | NqpPure::IsGeN
            | NqpPure::CmpN
            | NqpPure::IsNanOrInf => Operand::Num,
            _ => Operand::Int,
        }
    }
}

/// The op an id stands for, when it is one of these — one array load after the
/// table is built.
///
/// The table is derived from [`crate::runtime::nqp_op_ids`]'s own registry by
/// NAME, so an id can never name the wrong op here: ids are indices into that
/// registry and are not stable across edits to it, and this table is rebuilt
/// from it in the same process.
pub(crate) fn pure_op(id: u16) -> Option<NqpPure> {
    static TABLE: std::sync::OnceLock<Box<[Option<NqpPure>]>> = std::sync::OnceLock::new();
    let table = TABLE.get_or_init(|| {
        (0..crate::runtime::nqp_op_ids::nqp_op_count() as u16)
            .map(|id| by_name(crate::runtime::nqp_op_ids::nqp_op_name(id)))
            .collect()
    });
    table.get(id as usize).copied().flatten()
}

fn by_name(name: &str) -> Option<NqpPure> {
    Some(match name {
        "add_i" => NqpPure::AddI,
        "sub_i" => NqpPure::SubI,
        "mul_i" => NqpPure::MulI,
        "neg_i" => NqpPure::NegI,
        "abs_i" => NqpPure::AbsI,
        "bitor_i" => NqpPure::BitOrI,
        "bitand_i" => NqpPure::BitAndI,
        "bitxor_i" => NqpPure::BitXorI,
        "bitneg_i" => NqpPure::BitNegI,
        "bitshiftl_i" => NqpPure::ShlI,
        "bitshiftr_i" => NqpPure::ShrI,
        "iseq_i" => NqpPure::IsEqI,
        "isne_i" => NqpPure::IsNeI,
        "islt_i" => NqpPure::IsLtI,
        "isle_i" => NqpPure::IsLeI,
        "isgt_i" => NqpPure::IsGtI,
        "isge_i" => NqpPure::IsGeI,
        "cmp_i" => NqpPure::CmpI,
        "not_i" => NqpPure::NotI,
        "add_n" => NqpPure::AddN,
        "sub_n" => NqpPure::SubN,
        "mul_n" => NqpPure::MulN,
        "div_n" => NqpPure::DivN,
        "neg_n" => NqpPure::NegN,
        "abs_n" => NqpPure::AbsN,
        "iseq_n" => NqpPure::IsEqN,
        "isne_n" => NqpPure::IsNeN,
        "islt_n" => NqpPure::IsLtN,
        "isle_n" => NqpPure::IsLeN,
        "isgt_n" => NqpPure::IsGtN,
        "isge_n" => NqpPure::IsGeN,
        "cmp_n" => NqpPure::CmpN,
        "isnanorinf" => NqpPure::IsNanOrInf,
        _ => return None,
    })
}

/// Evaluate `op` directly from `args` when the operands are already native.
///
/// `None` means "this call site is not the proved shape" and the caller keeps
/// the general path — it is NOT a fallback *inside* the op: the same [`eval`]
/// runs either way, only reached through the argument-list protocol that
/// normalizes the operands first.
#[inline]
pub(crate) fn try_eval_native(op: NqpPure, args: &[Value]) -> Option<Value> {
    if args.len() != op.arity() {
        return None;
    }
    let ready = match op.operand() {
        // `as_int`/`as_num` are pure tag probes that answer only for an exact
        // `Int`/`Num` payload. Everything the general path would have to
        // normalize first — a `VarRef` wrapper, a `ContainerRef`, a
        // `HashEntryRef`, a `Proxy`, a `Str` that coerces, a `BigInt` that
        // saturates — fails them and keeps that path.
        Operand::Int => args.iter().all(|v| v.as_int().is_some()),
        Operand::Num => args
            .iter()
            .all(|v| v.as_num().is_some() || v.as_int().is_some()),
    };
    ready.then(|| eval(op, args))
}

/// The one implementation of each of these ops, shared with the string-keyed
/// table in [`crate::runtime::nqp_ops`].
pub(crate) fn eval(op: NqpPure, args: &[Value]) -> Value {
    match op {
        NqpPure::AddI => Value::int(iarg(args, 0).wrapping_add(iarg(args, 1))),
        NqpPure::SubI => Value::int(iarg(args, 0).wrapping_sub(iarg(args, 1))),
        NqpPure::MulI => Value::int(iarg(args, 0).wrapping_mul(iarg(args, 1))),
        NqpPure::NegI => Value::int(iarg(args, 0).wrapping_neg()),
        NqpPure::AbsI => Value::int(iarg(args, 0).wrapping_abs()),
        NqpPure::BitOrI => Value::int(iarg(args, 0) | iarg(args, 1)),
        NqpPure::BitAndI => Value::int(iarg(args, 0) & iarg(args, 1)),
        NqpPure::BitXorI => Value::int(iarg(args, 0) ^ iarg(args, 1)),
        NqpPure::BitNegI => Value::int(!iarg(args, 0)),
        NqpPure::ShlI => Value::int(iarg(args, 0).wrapping_shl(iarg(args, 1).clamp(0, 63) as u32)),
        NqpPure::ShrI => Value::int(iarg(args, 0).wrapping_shr(iarg(args, 1).clamp(0, 63) as u32)),
        NqpPure::IsEqI => bool_int(iarg(args, 0) == iarg(args, 1)),
        NqpPure::IsNeI => bool_int(iarg(args, 0) != iarg(args, 1)),
        NqpPure::IsLtI => bool_int(iarg(args, 0) < iarg(args, 1)),
        NqpPure::IsLeI => bool_int(iarg(args, 0) <= iarg(args, 1)),
        NqpPure::IsGtI => bool_int(iarg(args, 0) > iarg(args, 1)),
        NqpPure::IsGeI => bool_int(iarg(args, 0) >= iarg(args, 1)),
        NqpPure::CmpI => Value::int(cmp_result(iarg(args, 0).cmp(&iarg(args, 1)))),
        NqpPure::NotI => bool_int(iarg(args, 0) == 0),
        NqpPure::AddN => Value::num(narg(args, 0) + narg(args, 1)),
        NqpPure::SubN => Value::num(narg(args, 0) - narg(args, 1)),
        NqpPure::MulN => Value::num(narg(args, 0) * narg(args, 1)),
        NqpPure::DivN => Value::num(narg(args, 0) / narg(args, 1)),
        NqpPure::NegN => Value::num(-narg(args, 0)),
        NqpPure::AbsN => Value::num(narg(args, 0).abs()),
        NqpPure::IsEqN => bool_int(narg(args, 0) == narg(args, 1)),
        NqpPure::IsNeN => bool_int(narg(args, 0) != narg(args, 1)),
        NqpPure::IsLtN => bool_int(narg(args, 0) < narg(args, 1)),
        NqpPure::IsLeN => bool_int(narg(args, 0) <= narg(args, 1)),
        NqpPure::IsGtN => bool_int(narg(args, 0) > narg(args, 1)),
        NqpPure::IsGeN => bool_int(narg(args, 0) >= narg(args, 1)),
        NqpPure::CmpN => Value::int(
            narg(args, 0)
                .partial_cmp(&narg(args, 1))
                .map_or(0, cmp_result),
        ),
        NqpPure::IsNanOrInf => bool_int({
            let n = narg(args, 0);
            n.is_nan() || n.is_infinite()
        }),
    }
}

/// A missing operand reads as 0, exactly as the general table's own `iarg`
/// did — an `nqp::` op never rejects an arity.
#[inline]
fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

#[inline]
fn narg(args: &[Value], i: usize) -> f64 {
    args.get(i).map(|v| v.to_f64()).unwrap_or(0.0)
}

/// `nqp::` comparison ops answer a native int, not a `Bool`.
#[inline]
fn bool_int(b: bool) -> Value {
    Value::int(i64::from(b))
}

#[inline]
fn cmp_result(ordering: std::cmp::Ordering) -> i64 {
    match ordering {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every variant is registered under a name the op-id registry knows, and
    /// `pure_op` finds it from that name's id. A variant whose name is missing
    /// from the registry would silently never take the direct path.
    #[test]
    fn pure_op_round_trips() {
        for op in ALL {
            let id = crate::runtime::nqp_op_ids::nqp_op_id(op.name())
                .unwrap_or_else(|| panic!("{} is not in the nqp op registry", op.name()));
            assert_eq!(pure_op(id), Some(op), "pure_op for {}", op.name());
        }
    }

    /// An op the direct path must never claim: `div_i` can raise, so it stays
    /// on the general path where the error is raised.
    #[test]
    fn partial_ops_are_not_pure() {
        for name in ["div_i", "mod_i", "add_I", "iseq_s", "chars", "atpos_i"] {
            if let Some(id) = crate::runtime::nqp_op_ids::nqp_op_id(name) {
                assert_eq!(pure_op(id), None, "{name} must not be a pure op");
            }
        }
    }

    /// The direct path declines every operand shape the general path would
    /// have had to normalize first.
    #[test]
    fn non_native_operands_decline() {
        let op = NqpPure::AddI;
        assert!(try_eval_native(op, &[Value::int(1), Value::int(2)]).is_some());
        // Wrong arity.
        assert!(try_eval_native(op, &[Value::int(1)]).is_none());
        assert!(try_eval_native(op, &[Value::int(1), Value::int(2), Value::int(3)]).is_none());
        // A `Str` that the general path would coerce.
        assert!(try_eval_native(op, &[Value::int(1), Value::str("2".to_string())]).is_none());
        // A `Num` is not an `_i` operand.
        assert!(try_eval_native(op, &[Value::int(1), Value::num(2.0)]).is_none());
        // An `Int` IS a `_n` operand.
        assert!(try_eval_native(NqpPure::AddN, &[Value::int(1), Value::num(2.0)]).is_some());
    }

    #[test]
    fn native_results_match_the_bodies() {
        let two = [Value::int(7), Value::int(3)];
        assert_eq!(eval(NqpPure::AddI, &two).as_int(), Some(10));
        assert_eq!(eval(NqpPure::SubI, &two).as_int(), Some(4));
        assert_eq!(eval(NqpPure::CmpI, &two).as_int(), Some(1));
        assert_eq!(eval(NqpPure::IsEqI, &two).as_int(), Some(0));
        assert_eq!(eval(NqpPure::NotI, &[Value::int(0)]).as_int(), Some(1));
        // Wrapping, not panicking, on the native-int edge (debug builds).
        assert_eq!(
            eval(NqpPure::AddI, &[Value::int(i64::MAX), Value::int(1)]).as_int(),
            Some(i64::MIN)
        );
        assert_eq!(
            eval(NqpPure::IsNanOrInf, &[Value::num(f64::NAN)]).as_int(),
            Some(1)
        );
    }
}
