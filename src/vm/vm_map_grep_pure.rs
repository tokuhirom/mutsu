//! A conservative proof that a full grep/map pull can run in two bulk loops.
//! Callback interleaving is observable in general, so only bytecode that reads
//! the integer topic and applies a built-in integer operator qualifies.

use super::*;
use crate::value::{MapGrepChain, MapGrepItems, MapGrepMode, SeqSource};

impl Interpreter {
    /// Both callbacks are total, side-effect-free integer expressions over a
    /// stable Array of Ints. There are no user infix candidates that could
    /// make an arithmetic opcode invoke user code. A full pull can therefore
    /// run grep to completion before map without changing observable order.
    // Cost: O(n + b), n = source elements, b = callback bytecode length.
    pub(super) fn can_batch_pure_int_chain(
        &self,
        chain: &MapGrepChain,
        downstream: Option<&Value>,
        mode: &MapGrepMode,
    ) -> bool {
        if !matches!(mode, MapGrepMode::Map)
            || !self.dispatch.user_declared_infix_ops.is_empty()
            || !pure_int_topic_expression(downstream)
        {
            return false;
        }
        let Some(SeqSource::MapGrep {
            items,
            pos: 0,
            func,
            mode: MapGrepMode::GrepArray(_),
            ..
        }) = chain.unstarted_source()
        else {
            return false;
        };
        if !pure_int_topic_expression(func.as_ref()) {
            return false;
        }
        matches!(items, MapGrepItems::Live(_))
            && items.with_items(|values| {
                values.iter().all(|value| {
                    value.with_deref(|inner| matches!(inner.view(), ValueView::Int(_)))
                })
            })
    }
}

impl Interpreter {
    /// How many elements a prefix pull of a deferred `.map` may run, given the
    /// `needed` it was asked for. A callback that is a total, side-effect-free
    /// integer expression ([`pure_int_topic_expression`]) over Int elements
    /// cannot be told from one that runs later, so a consumer that pulls one
    /// element per iteration (`for`) is served a batch that grows with the
    /// elements already consumed (the first pull runs exactly `needed`, so
    /// `.head(1)` of a long map costs one callback call). Anything else, and
    /// a pull whose elements are not all plain Ints, returns `needed` unchanged.
    // Cost: O(w), w = the batch window (at most `PURE_BATCH_MAX` elements).
    pub(super) fn pure_int_prefix_batch(
        &self,
        func: Option<&Value>,
        mode: &MapGrepMode,
        items: &MapGrepItems,
        pos: usize,
        needed: usize,
    ) -> usize {
        // The first pull of a Seq runs exactly what was asked; later pulls
        // run ahead by as many elements as were consumed so far.
        let want = pos.min(PURE_BATCH_MAX);
        // Only an immutable snapshot of plain Ints: a live Array (or a
        // container cell) can be written by the consumer between two pulls,
        // and the element it holds then must be the one mapped.
        let MapGrepItems::Snapshot(values) = items else {
            return needed;
        };
        if want <= needed
            || !matches!(mode, MapGrepMode::Map)
            || !self.dispatch.user_declared_infix_ops.is_empty()
            || !pure_int_topic_expression(func)
        {
            return needed;
        }
        let ints = values
            .get(pos..)
            .unwrap_or(&[])
            .iter()
            .take(want)
            .take_while(|value| matches!(value.view(), ValueView::Int(_)))
            .count();
        needed.max(ints)
    }
}

/// The most elements a pure Int callback runs ahead of its consumer.
const PURE_BATCH_MAX: usize = 4096;

/// A callback that is `$_ OP constant` over an Int: the admitted operator and
/// its Int literal.
#[derive(Clone, Copy)]
pub(super) enum PureIntOp {
    Add(i64),
    Sub(i64),
    Mul(i64),
    /// `$_ %% constant`, constant != 0.
    DivisibleBy(i64),
}

impl PureIntOp {
    /// The callback's value for the Int `elem`, as the built-in operator
    /// computes it. `None` when `elem` is not a plain Int or the result leaves
    /// the i64 range (the caller then runs the real callback, which promotes
    /// to a BigInt).
    // Cost: O(1).
    pub(super) fn apply(self, elem: &Value) -> Option<Value> {
        let x = elem.as_int()?;
        match self {
            Self::Add(c) => x.checked_add(c).map(Value::int),
            Self::Sub(c) => x.checked_sub(c).map(Value::int),
            Self::Mul(c) => x.checked_mul(c).map(Value::int),
            // c != 0 by construction; `i64::MIN %% -1` is the one overflow.
            Self::DivisibleBy(c) => x.checked_rem(c).map(|r| Value::truth(r == 0)),
        }
    }
}

/// Admit a simple binary expression of the current topic and an Int literal.
/// Each admitted opcode has no user callback when both operands are native
/// integers; `%%` also requires a nonzero divisor to avoid a Failure value.
// Cost: O(b), b = callback bytecode length.
fn pure_int_topic_expression(func: Option<&Value>) -> bool {
    pure_int_topic_op(func).is_some()
}

/// [`pure_int_topic_expression`], keeping the operator and its constant.
// Cost: O(b), b = callback bytecode length.
pub(super) fn pure_int_topic_op(func: Option<&Value>) -> Option<PureIntOp> {
    let ValueView::Sub(data) = func.map(Value::view)? else {
        return None;
    };
    if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
        return None;
    }
    let code = data.compiled_code.as_ref()?;
    let [first, OpCode::LoadConst(value_idx), operator] = code.ops.as_slice() else {
        return None;
    };
    // The topic is read either as the dynamic `$_` of a bare block, or as the
    // one parameter of a WhateverCode (`*+1`), which keeps `_` as its param.
    let reads_topic = if data.is_bare_block
        && data.params.is_empty()
        && data.param_defs.is_empty()
        // Line markers (`Stmt::SetLine`, which a closure body carries since
        // #14164) emit no op; only the statements count.
        && data.body.iter().filter(|s| !s.is_marker()).count() == 1
    {
        matches!(first, OpCode::GetGlobal(idx)
            if matches!(code.constants.get(*idx as usize).map(Value::view),
                Some(ValueView::Str(name)) if name.as_str() == "_"))
    } else if crate::runtime::resolution_map_grep::sub_is_whatever_code(&data)
        && data.params.len() == 1
        && data.params[0] == "_"
    {
        matches!(first, OpCode::GetLocal(0)) && code.locals.len() == 1 && code.locals[0] == "_"
    } else {
        false
    };
    if !reads_topic {
        return None;
    }
    let ValueView::Int(constant) = code.constants.get(*value_idx as usize).map(Value::view)? else {
        return None;
    };
    match operator {
        OpCode::Add => Some(PureIntOp::Add(constant)),
        OpCode::Sub => Some(PureIntOp::Sub(constant)),
        OpCode::Mul => Some(PureIntOp::Mul(constant)),
        OpCode::DivisibleBy if constant != 0 => Some(PureIntOp::DivisibleBy(constant)),
        _ => None,
    }
}
