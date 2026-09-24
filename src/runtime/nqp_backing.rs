//! The array storage behind a list-ish `nqp::` value, shared by the list
//! ops of `nqp_ops_list.rs`, `nqp_ops_builtin.rs` and TRIR.

use super::nqp_ops_list::ITERATION_BUFFER_ITEMS;
use crate::runtime::{Interpreter, RuntimeError};
use crate::value::{NqpElemKind, Value, ValueView};

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

/// The storage of a plain list, or of a `Uni`'s codepoint list, handed to `f`
/// for an in-place edit. `None` for anything else (an `IterationBuffer`, a
/// Buf/Blob, a non-list), which the caller sends down the general path.
///
/// These two are the only shapes TRIR's per-character list ops meet in
/// practice (JSON::Fast's `unjsonify-string` shifts every codepoint off one
/// `Uni` and pushes it onto another), and neither can be a Buf, so this skips
/// [`push_elem`]'s Buf probe and [`Interpreter::nqp_with_elems_mut`]'s closure
/// layers. The edit is the same one those make: same node, same
/// `ArrayData` method.
// Cost: O(1) plus f.
#[inline]
pub(crate) fn with_list_data_mut<R>(
    v: &Value,
    f: impl FnOnce(&mut crate::value::ArrayData) -> R,
) -> Option<R> {
    match v.view() {
        // SAFETY: audited aliased in-place container write (see
        // value::aliased_mut and docs/gc-contents-mut-inventory.md) -- every
        // caller's `f` is a pure element edit (push/shift) that never
        // re-enters the interpreter, so no other borrow into the node is
        // live across it.
        ValueView::Array(items, _) => Some(f(unsafe { crate::value::gc_contents_mut(&items) })),
        ValueView::Uni(uni) => match uni.codes.view() {
            // SAFETY: as above.
            ValueView::Array(items, _) => Some(f(unsafe { crate::value::gc_contents_mut(&items) })),
            _ => None,
        },
        _ => None,
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

/// MoarVM's VMArray index rule, the one every positional `nqp::` op uses: a
/// negative index counts from the end, and one that still lands before the
/// start is an error. An index past the end is returned as is -- a read then
/// finds nothing, a write grows the list.
///
/// `nqp::atpos`, `atpos_i`/`_n`/`_s`, every `bindpos*`, `splice` and TRIR's
/// `atpos_i` resolve through here; each used to clamp a negative index to 0
/// (so `nqp::bindpos($l, -1, $v)` overwrote the FIRST element) or read it as
/// "absent".
// Cost: O(1).
pub(crate) fn resolve_index(idx: i64, len: usize) -> Result<usize, RuntimeError> {
    if idx >= 0 {
        return Ok(idx as usize);
    }
    let from_end = len as i64 + idx;
    if from_end < 0 {
        return Err(RuntimeError::new("MVMArray: Index out of bounds"));
    }
    Ok(from_end as usize)
}

/// The element at `idx` of a list-ish nqp value (see [`resolve_index`]).
/// `Ok(None)` past the end, or when the value has no elements at all.
// Cost: O(1).
pub(crate) fn elem_at(target: &Value, idx: i64) -> Result<Option<Value>, RuntimeError> {
    let Some(len) = Interpreter::nqp_elems_len_of(target) else {
        return Ok(None);
    };
    let i = resolve_index(idx, len)?;
    Ok(Interpreter::nqp_elem_at(target, i))
}

/// What a growth slot of a typed nqp list holds (#9235): `nqp::list_i`,
/// `list_n` and `list_s` are native arrays, so a slot `setelems` or a
/// past-the-end `bindpos*` opens reads back as that type's zero -- the null
/// string for `list_s`. `None` for an object list, whose slots are null.
// Cost: O(1).
pub(crate) fn typed_list_fill(kind: NqpElemKind) -> Option<Value> {
    match kind {
        NqpElemKind::Object => None,
        NqpElemKind::Int => Some(Value::int(0)),
        NqpElemKind::Num => Some(Value::num(0.0)),
        NqpElemKind::Str => Some(Value::str(String::new())),
    }
}

/// The element kind of `target` when it is an nqp typed list (see
/// [`typed_list_fill`]); [`NqpElemKind::Object`] for anything else.
// Cost: O(1).
fn nqp_elem_kind(target: &Value) -> NqpElemKind {
    match target.view() {
        ValueView::Array(items, _) => items.nqp_elem,
        _ => NqpElemKind::Object,
    }
}

/// Store `val` at `idx` of a list-ish nqp value (see [`resolve_index`]),
/// growing it when `idx` is past the end: with the typed list's zero on an
/// `nqp::list_i`/`_n`/`_s` (see [`typed_list_fill`]), otherwise with the op's
/// own `fill`. A Buf/Blob encodes the one element in place. The shared body of
/// `nqp::bindpos` and its typed twins, which differ only in how they convert
/// `val` and what `fill` is.
// Cost: O(1) amortized; O(i - e) when growing, i = index, e = elements.
pub(crate) fn bind_elem(
    op: &str,
    target: &Value,
    idx: i64,
    val: Value,
    fill: Value,
) -> Result<Value, RuntimeError> {
    let len = Interpreter::nqp_elems_len_of(target).unwrap_or(0);
    let i = resolve_index(idx, len)?;
    if let Some((_, attrs)) = crate::value::value_buf::buf_target(target)
        && crate::value::value_buf::set_buf_elem(&attrs, i, &val).is_some()
    {
        return Ok(val);
    }
    let fill = typed_list_fill(nqp_elem_kind(target)).unwrap_or(fill);
    let stored = val.clone();
    Interpreter::nqp_with_elems_mut(op, target, |elems| {
        if elems.len() <= i {
            elems.resize(i + 1, fill);
        }
        elems[i] = stored;
    })?;
    Ok(val)
}

/// `nqp::atpos_i`'s answer as a native int: the element at `idx` (see
/// [`resolve_index`]) coerced to an int, 0 past the end. The one body the
/// op table and TRIR's typed `AtPosI` share.
// Cost: O(1).
#[inline]
pub(crate) fn atpos_i(target: &Value, idx: i64) -> Result<i64, RuntimeError> {
    // A plain `nqp::list_i` IS an array, and that is what every scanner's
    // lookup table is: read it directly rather than through the backing
    // walk, which costs more than the read.
    if idx >= 0
        && let ValueView::Array(items, _) = target.view()
    {
        return Ok(items.get(idx as usize).map_or(0, |e| {
            e.as_int().unwrap_or_else(|| crate::runtime::to_int(e))
        }));
    }
    Ok(elem_at(target, idx)?.map_or(0, |e| crate::runtime::to_int(&e)))
}
