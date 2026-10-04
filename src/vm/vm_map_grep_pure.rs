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

/// Admit a simple binary expression of the current topic and an Int literal.
/// Each admitted opcode has no user callback when both operands are native
/// integers; `%%` also requires a nonzero divisor to avoid a Failure value.
// Cost: O(b), b = callback bytecode length.
fn pure_int_topic_expression(func: Option<&Value>) -> bool {
    let Some(ValueView::Sub(data)) = func.map(Value::view) else {
        return false;
    };
    if !data.is_bare_block
        || !data.params.is_empty()
        || !data.param_defs.is_empty()
        || !data.assumed_positional.is_empty()
        || !data.assumed_named.is_empty()
        || data.body.len() != 1
    {
        return false;
    }
    let Some(code) = &data.compiled_code else {
        return false;
    };
    let [
        OpCode::GetGlobal(topic_idx),
        OpCode::LoadConst(value_idx),
        operator,
    ] = code.ops.as_slice()
    else {
        return false;
    };
    if !matches!(code.constants.get(*topic_idx as usize).map(Value::view), Some(ValueView::Str(name)) if name.as_str() == "_")
    {
        return false;
    }
    let Some(ValueView::Int(constant)) = code.constants.get(*value_idx as usize).map(Value::view)
    else {
        return false;
    };
    matches!(operator, OpCode::Add | OpCode::Sub | OpCode::Mul)
        || matches!(operator, OpCode::DivisibleBy) && constant != 0
}
