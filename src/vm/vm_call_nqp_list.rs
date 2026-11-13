//! The list `nqp::` ops on an `IterationBuffer`, answered without the op-table
//! walk (#17337).
//!
//! `nqp::atpos`, `nqp::elems`, `nqp::push` and `nqp::shift` on an
//! `IterationBuffer` are the inner loop of nqp-style Raku code (ParaSeq's batch
//! processors run several per element). Through [`Interpreter::exec_nqp_op`]
//! each paid ~2,500 instructions in front of a few hundred of work: the operand
//! vector, the `Proxy` fetch, the `ConcBlockingQueue` probe, the REPR check and
//! a string `match` over the op names of its table. For a plain
//! `IterationBuffer` every one of those answers "no" (it is a `VMArray`, never a
//! queue, and its operands here are neither containers nor proxies), so this
//! entry calls the op's own implementation -- the very function its table arm
//! calls -- directly. Anything else (a subclass, a view, a typed list, a
//! `VarRef` or `Proxy` operand, a non-`Int` index) takes the general path.

use super::*;

/// The ops this entry answers.
#[derive(Clone, Copy)]
enum ListOp {
    AtPos,
    Elems,
    Push,
    Shift,
}

/// The op `id` names, when it is one of [`ListOp`]'s with that many operands.
// Cost: O(1) after the first call (four ids resolved once).
fn list_op(id: u16, arity: usize) -> Option<ListOp> {
    static IDS: std::sync::OnceLock<[Option<u16>; 4]> = std::sync::OnceLock::new();
    let ids = IDS.get_or_init(|| {
        ["atpos", "elems", "push", "shift"].map(crate::runtime::nqp_op_ids::nqp_op_id)
    });
    let op = match ids.iter().position(|&known| known == Some(id))? {
        0 => ListOp::AtPos,
        1 => ListOp::Elems,
        2 => ListOp::Push,
        _ => ListOp::Shift,
    };
    let operands = match op {
        ListOp::AtPos | ListOp::Push => 2,
        ListOp::Elems | ListOp::Shift => 1,
    };
    (arity == operands).then_some(op)
}

/// Whether `v` is an instance of `IterationBuffer` itself.
// Cost: O(1), a tag probe and a symbol compare.
fn is_plain_iteration_buffer(v: &Value) -> bool {
    static CLASS: std::sync::OnceLock<crate::symbol::Symbol> = std::sync::OnceLock::new();
    let class = *CLASS.get_or_init(|| crate::symbol::Symbol::intern("IterationBuffer"));
    matches!(v.view(), ValueView::Instance { class_name, .. } if class_name == class)
}

impl Interpreter {
    /// The direct form of [`Self::exec_nqp_op`] for `atpos`, `elems`, `push`
    /// and `shift` on an `IterationBuffer` (see the module docs). `true` means
    /// the op ran and its result is on the stack; `false` leaves the stack to
    /// the general path. Like `exec_nqp_pure_op`, the one-shot callsite-line
    /// clear and the dispatch tally are kept.
    // Cost: O(1) plus the op: `atpos`/`elems`/`push` O(1), `shift` O(1)
    // amortized.
    pub(super) fn exec_nqp_list_op(&mut self, id: u16, arity: usize) -> bool {
        let Some(op) = list_op(id, arity) else {
            return false;
        };
        let start = self.stack.len() - arity;
        let operands = &self.stack[start..];
        if !is_plain_iteration_buffer(&operands[0])
            || operands
                .iter()
                .any(|v| v.as_varref().is_some() || v.is_proxy_value())
        {
            return false;
        }
        let result = match op {
            ListOp::AtPos => {
                let ValueView::Int(idx) = operands[1].view() else {
                    return false;
                };
                crate::runtime::nqp_backing::elem_at(&operands[0], idx)
                    .map(|e| e.unwrap_or(Value::NIL))
            }
            ListOp::Elems => Ok(Value::int(
                Self::nqp_elems_len_of(&operands[0]).unwrap_or(0) as i64,
            )),
            ListOp::Push => {
                crate::runtime::nqp_backing::push_elem("push", &operands[0], operands[1].clone())
            }
            ListOp::Shift => Self::nqp_shift("shift", &operands[0]),
        };
        let Ok(result) = result else {
            // An error (`atpos` before the start) is raised by the general
            // path, with its own wording and context.
            return false;
        };
        loan_env!(self, set_pending_callsite_line(None));
        crate::vm::vm_stats::record_function_dispatch();
        self.stack.truncate(start);
        self.stack.push(result);
        true
    }
}
