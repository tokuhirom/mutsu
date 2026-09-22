//! The structural half of a runtime infix operator: the meta-operator layers
//! wrapped around its leaf.
//!
//! [`Interpreter::eval_reduction_operator_values`] used to re-derive that
//! structure from the operator's spelling on every call — and the call is *per
//! element*: once per pair of a `Z`/`X`/hyper, once per fold step of a
//! reduction. [`crate::compiled_operator::InfixShape::lower`] now decodes it
//! once and [`Interpreter::eval_infix_shape`] walks the decoded layers, so
//! descending into a `Z+`'s inner `+` for the next element is a slice bump
//! rather than another parse.
//!
//! The leaf itself (the operator tables, the coercion bridges, the user-infix
//! fallback) stays in `vm_dispatch_helpers.rs`.

use super::*;
use crate::compiled_operator::{InfixRef, InfixShape, MetaLayer};

impl Interpreter {
    /// Apply an infix operator, given as its source spelling, to two values.
    ///
    /// The spelling is decoded once here; a caller with an element loop should
    /// lower it itself and call [`Interpreter::eval_infix_shape`] instead, so
    /// the decode does not run per element.
    pub(super) fn eval_reduction_operator_values(
        &mut self,
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        let shape = InfixShape::lower(op);
        self.eval_infix_shape(shape.as_ref(), left, right)
    }

    /// Apply an already-decoded infix operator to two values, peeling one
    /// meta-operator layer per recursion.
    pub(super) fn eval_infix_shape(
        &mut self,
        op: InfixRef<'_>,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        let Some((layer, inner)) = op.split_first() else {
            return self.eval_infix_leaf(op, left, right);
        };
        match layer {
            // `R op`: apply the inner operator with the operands swapped.
            MetaLayer::Reverse => self.eval_infix_shape(inner, right, left),
            // A bare `Z` zips two lists into tuples (used by `[Z]`). When the
            // left elements are already lists (from a prior `Z` fold), flatten
            // them so `[Z] (a,b,c),(d,e,f),(g,h,i)` produces `(a d g)`,
            // `(b e h)`, `(c f i)`.
            MetaLayer::ZipTuple => {
                let left_list = runtime::value_to_list(left);
                let right_list = runtime::value_to_list(right);
                let len = left_list.len().min(right_list.len());
                let mut results = Vec::with_capacity(len);
                for i in 0..len {
                    let mut tuple = match left_list[i].view() {
                        ValueView::Array(items, kind) if !kind.is_itemized() => items.to_vec(),
                        _ => vec![left_list[i].clone()],
                    };
                    tuple.push(right_list[i].clone());
                    results.push(Value::array(tuple));
                }
                // Like the plain `Z` infix, the reduction yields a Seq (raku:
                // `([Z] ...).WHAT` is `(Seq)`, `.raku` shows the `.Seq` suffix).
                Ok(Value::seq(results))
            }
            // `Z op`: zip two lists element-wise with the inner operator.
            MetaLayer::Zip => {
                let left_list = runtime::value_to_list(left);
                let right_list = runtime::value_to_list(right);
                let len = left_list.len().min(right_list.len());
                let mut results = Vec::with_capacity(len);
                for i in 0..len {
                    results.push(self.eval_infix_shape(inner, &left_list[i], &right_list[i])?);
                }
                Ok(Value::seq(results))
            }
            // `>>op<<`, `>>op>>`, `<<op<<`, `<<op>>`: apply the inner operator
            // element-wise to two lists.
            MetaLayer::Hyper {
                dwim_left,
                dwim_right,
            } => {
                let left_list = runtime::value_to_list(left);
                let right_list = runtime::value_to_list(right);
                let len = if dwim_left && !dwim_right {
                    right_list.len()
                } else if dwim_right && !dwim_left {
                    left_list.len()
                } else {
                    left_list.len().max(right_list.len())
                };
                let mut results = Vec::with_capacity(len);
                for i in 0..len {
                    let l = if left_list.is_empty() {
                        &Value::int(0.into())
                    } else {
                        &left_list[i % left_list.len()]
                    };
                    let r = if right_list.is_empty() {
                        &Value::int(0.into())
                    } else {
                        &right_list[i % right_list.len()]
                    };
                    results.push(self.eval_infix_shape(inner, l, r)?);
                }
                Ok(Value::array(results))
            }
        }
    }
}
