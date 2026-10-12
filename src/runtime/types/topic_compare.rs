//! Answering a [`TopicIntCompare`] predicate without running its bytecode
//! (see `crate::predicate_shape`).
use super::*;
use crate::predicate_shape::{IntCompareOp, TopicIntCompare};

impl Interpreter {
    /// Whether `value` satisfies `cmp`, or `None` when the predicate's
    /// bytecode has to run instead: `value` is not a plain Int, or a user
    /// candidate of the operator exists anywhere (the comparison routine's
    /// own scope check depends on the frame the chunk would run in).
    ///
    /// The answer comes from the routine the comparison opcodes run
    /// (`num_lt_values` and kin), so it is the one the chunk would give.
    // Cost: O(1).
    pub(crate) fn topic_int_compare_holds(
        &mut self,
        cmp: TopicIntCompare,
        value: &Value,
    ) -> Option<bool> {
        if !matches!(value.view(), ValueView::Int(_))
            || self
                .dispatch
                .user_declared_infix_ops
                .contains_key(cmp.op.infix_name())
        {
            return None;
        }
        let lhs = value.clone();
        let rhs = Value::int(cmp.rhs);
        let verdict = match cmp.op {
            IntCompareOp::Lt => self.num_lt_values(lhs, rhs),
            IntCompareOp::Le => self.num_le_values(lhs, rhs),
            IntCompareOp::Gt => self.num_gt_values(lhs, rhs),
            IntCompareOp::Ge => self.num_ge_values(lhs, rhs),
            IntCompareOp::Eq => self.num_eq_values(lhs, rhs),
        };
        verdict.ok().map(|v| v.truthy())
    }
}
