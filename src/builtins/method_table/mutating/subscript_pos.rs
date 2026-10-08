//! `ASSIGN-POS` and `DELETE-POS` on an `Array` (ADR-11276 slice 4, §9.37).
//!
//! The positional half of the subscript protocol's mutators, with the keyed
//! half in [`super::subscript`]. One handler answers a named `@a`, a scalar
//! holding an array, a by-value receiver, the backing storage of an `is Array`
//! instance and an array inside a `Mixin` (the last two reach the row through a
//! detached place). The write goes **in place** through the array's shared
//! `Gc` node, so every holder sees it.
//!
//! A one-index call is the row; the multi-dimension forms (`@a.ASSIGN-POS(1, 2,
//! $v)`, `@a.DELETE-POS(1, 2)`) walk nested arrays and stay with the cascade,
//! which also keeps `BIND-POS`: binding a scalar *variable* needs the caller's
//! argument sources, which a row does not receive.
//!
//! A `List` declares neither method in Rakudo, but the cascade answered both for
//! every array value, so the `List` rows share the handlers.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::methods::make_not_enough_dimensions_error;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $arity:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Mut($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Array", "ASSIGN-POS", 2, assign_pos),
    row!("Array", "DELETE-POS", 1, delete_pos),
    row!("List", "ASSIGN-POS", 2, assign_pos),
    row!("List", "DELETE-POS", 1, delete_pos),
];

/// A non-negative index, or `None` for anything else.
// Cost: O(1).
fn index_of(idx: &Value) -> Option<usize> {
    match idx.view() {
        ValueView::Int(i) if i >= 0 => Some(i as usize),
        ValueView::Num(f) if f >= 0.0 => Some(f as usize),
        _ => None,
    }
}

/// `Array.ASSIGN-POS($index, $value)`: store `$value` at `$index`, growing the
/// array (the gap stays a hole). Answers `$value`.
// Cost: O(1) amortized, in place (the element type check reads the container's
// `value_type`; only a failing check scans the env, for the variable name its
// message reports).
fn assign_pos(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let target = place.value().deref_container().descalarize().clone();
    let ValueView::Array(items, _) = target.view() else {
        return None;
    };
    let (idx, value) = (&args[0], &args[1]);
    let shape = crate::runtime::utils::shaped_array_shape(&target);
    // A shaped array needs one index per dimension.
    if let Some(shape) = &shape
        && shape.len() > 1
    {
        return Some(Err(make_not_enough_dimensions_error(
            "assign to",
            1,
            shape.len(),
        )));
    }
    let Some(index) = index_of(idx) else {
        // `@a[-1] = $v` refuses the same way.
        return Some(Err(RuntimeError::new(format!(
            "Index out of range. Is: {}, should be in 0..^Inf",
            idx.to_string_value()
        ))));
    };
    if !value.is_nil()
        && let Some(constraint) = items.value_type.as_deref()
        && !interp.type_matches_value(constraint, value)
    {
        let var_name = interp.array_binding_name(&items);
        return Some(Err(
            interp.type_check_element_failure(&var_name, constraint, value)
        ));
    }
    if let Some(shape) = &shape
        && !shape.is_empty()
        && index >= shape[0]
    {
        return Some(Err(RuntimeError::new(format!(
            "Index {} for dimension 1 out of range 0..{}",
            index, shape[0]
        ))));
    }
    match items.get(index).map(Value::view) {
        Some(ValueView::Scalar(_)) => return Some(Err(RuntimeError::assignment_ro(None))),
        // An element `BIND-POS`-bound to a bare value (#10924).
        Some(ValueView::ContainerRef(cell)) if cell.is_readonly() => {
            return Some(Err(RuntimeError::immutable_value()));
        }
        _ => {}
    }
    // In place through the shared node, with the same store the `[]=` opcode
    // uses: every holder of the array sees the write (container identity), and
    // a grown gap stays a hole.
    // SAFETY: audited aliased in-place container write (see value::aliased_mut);
    // no borrow into the node is live.
    let data = unsafe { crate::value::gc_contents_mut(&items) };
    data.store_element(
        index,
        Interpreter::itemize_value_for_element_store(value.clone()),
    );
    place.reseat(interp);
    Some(Ok(value.clone()))
}

/// `Array.DELETE-POS($index)`: leave a hole at `$index`, trim trailing holes
/// and answer what the slot held.
// Cost: O(t), t = trailing holes trimmed (in place through the shared node).
fn delete_pos(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let target = place.value().deref_container().descalarize().clone();
    let ValueView::Array(items, _) = target.view() else {
        return None;
    };
    if let Some(shape) = crate::runtime::utils::shaped_array_shape(&target)
        && shape.len() > 1
    {
        return Some(Err(make_not_enough_dimensions_error(
            "delete from",
            1,
            shape.len(),
        )));
    }
    if items
        .value_type
        .as_deref()
        .is_some_and(crate::runtime::native_types::is_native_array_element_type)
    {
        return Some(Err(RuntimeError::new(
            "Cannot delete from a natively typed array",
        )));
    }
    let Some(index) = index_of(&args[0]) else {
        return Some(Err(RuntimeError::new(
            "Cannot DELETE-POS with a negative index",
        )));
    };
    // Through the shared backing node rather than a rebuild plus env rewrite: an
    // array held inside a `Mixin`'s `Arc<Value>` is not an env binding.
    let old = interp.array_delete_pos_value(&target, index);
    place.reseat(interp);
    Some(Ok(old))
}
