//! The list `nqp::` ops on an `IterationBuffer`, answered without the op-table
//! walk (#17337).
//!
//! `nqp::atpos`, `nqp::elems`, `nqp::push`, `nqp::shift` and `nqp::setelems` on an
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
    SetElems,
}

/// The op `id` names, when it is one of [`ListOp`]'s with that many operands.
// Cost: O(1) after the first call (four ids resolved once).
fn list_op(id: u16, arity: usize) -> Option<ListOp> {
    static IDS: std::sync::OnceLock<[Option<u16>; 5]> = std::sync::OnceLock::new();
    let ids = IDS.get_or_init(|| {
        ["atpos", "elems", "push", "shift", "setelems"].map(crate::runtime::nqp_op_ids::nqp_op_id)
    });
    let op = match ids.iter().position(|&known| known == Some(id))? {
        0 => ListOp::AtPos,
        1 => ListOp::Elems,
        2 => ListOp::Push,
        3 => ListOp::Shift,
        _ => ListOp::SetElems,
    };
    let operands = match op {
        ListOp::AtPos | ListOp::Push | ListOp::SetElems => 2,
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

/// The element storage of a plain `IterationBuffer`, handed to `f` while the
/// instance's attribute map is read-locked. `None` when the buffer has not
/// vivified its storage yet (the general path does that). The `f`s below are
/// pure element reads and edits that never re-enter the interpreter, so
/// nothing can wait on the read lock while it is held.
// Cost: O(1) plus f.
fn with_buffer_items<R>(
    buffer: &Value,
    f: impl FnOnce(&crate::value::GcRef<'_, crate::value::ArrayData>) -> R,
) -> Option<R> {
    let ValueView::Instance { attributes, .. } = buffer.view() else {
        return None;
    };
    let map = attributes.as_map();
    match map
        .get(crate::runtime::nqp_ops_list::iteration_buffer_items_key())?
        .view()
    {
        ValueView::Array(items, _) => Some(f(&items)),
        _ => None,
    }
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
        if operands
            .iter()
            .any(|v| v.as_varref().is_some() || v.is_proxy_value())
        {
            return false;
        }
        // A buffer held in a captured or bound variable arrives as the
        // variable's cell; every op reads through it (`with_nqp_backing_array`
        // derefs a cell), so the buffer itself is the operand.
        let buffer = if operands[0].is_container_ref() {
            operands[0].deref_container()
        } else {
            operands[0].clone()
        };
        if !is_plain_iteration_buffer(&buffer) {
            return false;
        }
        // `atpos`, `push` and `setelems` reach the buffer's storage directly
        // (the same element read, `Vec` push and resize their general forms
        // end in), without the high-level-Array refusal, the Buf probe and
        // the storage-handle clone those forms need for other operands.
        let view = buffer.is_iteration_buffer_view();
        let result = match op {
            ListOp::AtPos => {
                let ValueView::Int(idx) = operands[1].deref_container().view() else {
                    return false;
                };
                match with_buffer_items(&buffer, |items| {
                    crate::runtime::nqp_backing::resolve_index(idx, items.len())
                        .map(|i| Self::array_elem_at(items, i, view).unwrap_or(Value::NIL))
                }) {
                    Some(r) => r,
                    None => crate::runtime::nqp_backing::elem_at(&buffer, idx)
                        .map(|e| e.unwrap_or(Value::NIL)),
                }
            }
            ListOp::Elems => Ok(Value::int(
                Self::nqp_elems_len_of(&buffer).unwrap_or(0) as i64
            )),
            ListOp::Push => {
                let value = operands[1].clone();
                let pushed = with_buffer_items(&buffer, |items| {
                    // SAFETY: audited aliased in-place container write (see
                    // `Interpreter::nqp_with_elems_mut`): a pure element push
                    // that never re-enters the interpreter.
                    let data = unsafe { crate::value::gc_contents_mut(items) };
                    data.items_mut().push(value.clone());
                });
                match pushed {
                    Some(()) => Ok(value),
                    None => crate::runtime::nqp_backing::push_elem("push", &buffer, value),
                }
            }
            ListOp::Shift => Self::nqp_shift("shift", &buffer),
            ListOp::SetElems => {
                let ValueView::Int(n) = operands[1].deref_container().view() else {
                    return false;
                };
                if with_buffer_items(&buffer, |items| {
                    crate::runtime::nqp_backing::setelems_items(&buffer, items, n.max(0) as usize)
                })
                .is_none()
                {
                    return false;
                }
                Ok(buffer.clone())
            }
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
