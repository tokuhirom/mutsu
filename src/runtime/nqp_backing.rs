//! The array storage behind a list-ish `nqp::` value, shared by the list
//! ops of `nqp_ops_list.rs`, `nqp_ops_builtin.rs` and TRIR.

use super::nqp_ops_list::ITERATION_BUFFER_ITEMS;
use crate::runtime::{Interpreter, RuntimeError};
use crate::value::{Value, ValueView};

/// The array a list-ish nqp value is backed by, as a `Value` that shares the
/// target's `Gc` node — so an in-place write through it is visible to every
/// other holder of the original.
///
/// A plain array answers for itself. A `Uni` answers with its codepoint array
/// — rakudo's `Uni` is a `uint32` VMArray and nqp code indexes, consumes and
/// splices one as exactly that. An `IterationBuffer` answers with its element
/// array, vivified when absent: `nqp::create(IterationBuffer)` runs `CREATE`,
/// which by definition installs no attributes, so the buffer nqp code then
/// pushes onto has no storage yet. A Buf/Blob answers `None` — its elements
/// live behind `value_buf` and [`Interpreter::nqp_with_elems_mut`] routes those
/// separately.
pub(crate) fn nqp_backing_array(v: &Value) -> Option<Value> {
    match v.view() {
        ValueView::Array(..) => Some(v.clone()),
        ValueView::Uni(uni) => Some(uni.codes.clone()),
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "IterationBuffer" => {
            if let Some(items) = attributes.as_map().get(ITERATION_BUFFER_ITEMS)
                && matches!(items.view(), ValueView::Array(..))
            {
                return Some(items.clone());
            }
            // One key into the shared cell, not a copy of the whole map
            // committed back (`InstanceAttrs::insert`, as `value_buf` writes).
            let fresh = Value::real_array(Vec::new());
            attributes.insert(ITERATION_BUFFER_ITEMS, fresh.clone());
            Some(fresh)
        }
        _ => None,
    }
}

/// [`nqp_backing_array`] handed to `f` by reference: a plain array and a
/// `Uni` lend their own node, so the common case pays no refcount pair for a
/// read or an in-place edit (ADR-0116 D2.2: that clone and its drop were
/// ~1.9 M instructions per 100 JSON::Fast records). An `IterationBuffer`
/// still goes through the owned form, which may vivify its storage.
// Cost: O(1) plus f.
pub(crate) fn with_nqp_backing_array<R>(v: &Value, f: impl FnOnce(&Value) -> R) -> Option<R> {
    match v.view() {
        ValueView::Array(..) => Some(f(v)),
        ValueView::Uni(uni) => Some(f(&uni.codes)),
        _ => nqp_backing_array(v).map(|array| f(&array)),
    }
}

/// Push a value onto an nqp list / native array in place. Returns the
/// pushed value itself (not the array) -- nqp's own `push`/`push_i`/
/// `push_s`/`push_n` all hand back the element just appended, which is what
/// lets idioms like `has-word`'s
/// `nqp::add_i(nqp::push_i(@positions,$pos),$move)` chain off it directly.
pub(crate) fn push_elem(op: &str, target: &Value, val: Value) -> Result<Value, RuntimeError> {
    // A Buf encodes just the new element onto its storage (#7680, #9132);
    // the generic element editor below would decode and re-encode it whole.
    if let Some((class_name, attrs)) = crate::value::value_buf::buf_target(target) {
        let end = crate::value::value_buf::BufEnd::Back;
        crate::value::value_buf::extend_buf_elems(
            &attrs,
            class_name,
            std::slice::from_ref(&val),
            end,
        );
        return Ok(val);
    }
    Interpreter::nqp_with_elems_mut(op, target, |elems| elems.push(val.clone()))?;
    Ok(val)
}
