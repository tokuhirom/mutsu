//! `BIND-KEY` on a `Hash` and the six quant hashes, `BIND-POS` on an `Array`
//! (ADR-11276 slice 4, §9.41).
//!
//! Binding an element to a caller's *variable* (`%h.BIND-KEY('k', $x)`,
//! `@a.BIND-POS(0, $x)`, which `%h<k> := $x` and `@a[0] := $x` compile to)
//! stores a shared `ContainerRef` cell in the element and promotes `$x` into
//! the same cell, so a later `$x = ...` writes through to the element and vice
//! versa. That needs the call's argument sources, which the VM hands the row
//! through [`ReceiverPlace::arg_source`]; a call that did not come through the
//! VM carries none and stays with the cascade, which stores the immutable
//! bind marker for a literal source.
//!
//! Like the other subscript mutators the write goes **in place** through the
//! container's shared `Gc` node, so every holder of the hash or array sees the
//! bound element and the container's type metadata stays attached. A `Set`,
//! `Bag` or `Mix` (mutable or not) refuses with `X::Bind`.
//!
//! `BIND-POS` is the one-index form on a plain array whose source is a scalar
//! variable; a natively typed array (it cannot hold a boxed cell), a negative
//! index and a non-variable source decline to the cascade, which raises the
//! right error or stores the bind marker.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueMap, ValueView};

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 2,
            handler: Handler::Mut($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Hash", "BIND-KEY", hash_bind_key),
    row!("Array", "BIND-POS", array_bind_pos),
    row!("SetHash", "BIND-KEY", quanthash_bind_key),
    row!("BagHash", "BIND-KEY", quanthash_bind_key),
    row!("MixHash", "BIND-KEY", quanthash_bind_key),
    row!("Set", "BIND-KEY", quanthash_bind_key),
    row!("Bag", "BIND-KEY", quanthash_bind_key),
    row!("Mix", "BIND-KEY", quanthash_bind_key),
];

/// `Hash.BIND-KEY($key, $source)`: bind the pair to the caller's variable (or,
/// for a source that is not a variable, to a read-only cell holding the
/// value). An object hash keys by `.WHICH` and remembers the key object.
/// Answers the bound value.
// Cost: O(1) expected (one hash insert; an object hash also records its key
// object).
fn hash_bind_key(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if !place.from_vm() {
        return None;
    }
    let mut hash = place.value().deref_container().descalarize().clone();
    if !matches!(hash.view(), ValueView::Hash(..)) {
        return None;
    }
    let value = args[1].clone();
    let mut install: Option<(String, Value)> = None;
    let source_var = place.arg_source(1).map(str::to_string);
    let cell = interp.bind_key_source_cell(source_var.as_deref(), &value, &mut install);
    hash.with_hash_mut(|gc| {
        let data = crate::value::gc_data_mut(gc);
        let key = if data.key_type.is_some() {
            let key = crate::runtime::utils::value_which_key(&args[0]);
            data.original_keys
                .get_or_insert_with(ValueMap::default)
                .insert(key.clone(), args[0].clone());
            key
        } else {
            args[0].to_string_value()
        };
        data.map.insert(key, Value::container_ref(cell));
    });
    if let Some((name, cell)) = install {
        place.install_source_cell(interp, &name, cell);
    }
    place.reseat(interp);
    Some(Ok(value))
}

/// `BIND-KEY` on a `Set`, `Bag`, `Mix` or their mutable forms: refused.
// Cost: O(1).
fn quanthash_bind_key(
    _interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let target = place.value().deref_container().descalarize().clone();
    let name = match target.view() {
        ValueView::Set(_, mutable) => ["Set", "SetHash"][mutable as usize],
        ValueView::Bag(_, mutable) => ["Bag", "BagHash"][mutable as usize],
        ValueView::Mix(_, mutable) => ["Mix", "MixHash"][mutable as usize],
        _ => return None,
    };
    Some(Err(RuntimeError::bind(name)))
}

/// `Array.BIND-POS($index, $source)`: bind element `$index` to the caller's
/// scalar variable, growing the array (the gap stays a hole). Answers the
/// bound value.
// Cost: O(1) amortized, in place (the element type is read off the node).
fn array_bind_pos(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if !place.from_vm() {
        return None;
    }
    let source_var = place
        .arg_source(1)
        .filter(|n| !n.contains('\0'))?
        .to_string();
    let target = place.value().deref_container().descalarize().clone();
    let ValueView::Array(items, _) = target.view() else {
        return None;
    };
    // A natively typed array cannot hold a boxed cell: the cascade raises
    // "Cannot bind to a natively typed array".
    if items
        .value_type
        .as_deref()
        .is_some_and(crate::runtime::native_types::is_native_array_element_type)
    {
        return None;
    }
    let index = match args[0].view() {
        ValueView::Int(n) if n >= 0 => n as usize,
        ValueView::Num(f) if f >= 0.0 => f as usize,
        _ => return None,
    };
    let value = args[1].clone();
    // Reuse the source variable's cell when it is already cell-bound (so all
    // aliases stay shared); otherwise promote it into a fresh one.
    let mut install: Option<(String, Value)> = None;
    let cell = interp.bind_key_source_cell(Some(&source_var), &value, &mut install);
    // SAFETY: audited aliased in-place container write (see value::aliased_mut);
    // no borrow into the node is live.
    let data = unsafe { crate::value::gc_contents_mut(&items) };
    data.store_element(index, Value::container_ref(cell));
    if let Some((name, cell)) = install {
        place.install_source_cell(interp, &name, cell);
    }
    place.reseat(interp);
    Some(Ok(value))
}
