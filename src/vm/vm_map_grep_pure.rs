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
    /// a pull whose elements are not all Ints, returns `needed` unchanged.
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
        if want <= needed
            || !matches!(mode, MapGrepMode::Map)
            || !self.dispatch.user_declared_infix_ops.is_empty()
            || !pure_int_topic_expression(func)
        {
            return needed;
        }
        let ints = items.with_items(|values| {
            values
                .get(pos..)
                .unwrap_or(&[])
                .iter()
                .take(want)
                .take_while(|value| {
                    value.with_deref(|inner| matches!(inner.view(), ValueView::Int(_)))
                })
                .count()
        });
        needed.max(ints)
    }
}

/// The most elements a pure Int callback runs ahead of its consumer.
const PURE_BATCH_MAX: usize = 4096;

/// Admit a simple binary expression of the current topic and an Int literal.
/// Each admitted opcode has no user callback when both operands are native
/// integers; `%%` also requires a nonzero divisor to avoid a Failure value.
// Cost: O(b), b = callback bytecode length.
fn pure_int_topic_expression(func: Option<&Value>) -> bool {
    let Some(ValueView::Sub(data)) = func.map(Value::view) else {
        return false;
    };
    if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
        return false;
    }
    let Some(code) = &data.compiled_code else {
        return false;
    };
    let [first, OpCode::LoadConst(value_idx), operator] = code.ops.as_slice() else {
        return false;
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
        return false;
    }
    let Some(ValueView::Int(constant)) = code.constants.get(*value_idx as usize).map(Value::view)
    else {
        return false;
    };
    matches!(operator, OpCode::Add | OpCode::Sub | OpCode::Mul)
        || matches!(operator, OpCode::DivisibleBy) && constant != 0
}
