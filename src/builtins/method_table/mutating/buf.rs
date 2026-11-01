//! The `Buf` mutators (ADR-11276 §8.3, slice 3F remainder): `push`, `append`,
//! `unshift`, `prepend`, `pop`, `shift`, `splice` and `reallocate`.
//!
//! Rakudo declares them on `Buf`. `Blob` declares only the protos of `push`,
//! `append`, `unshift` and `prepend`, whose candidates all want a `Buf:D`, so
//! a `Blob` or encoding-buffer receiver gets `X::Multi::NoMatch` from `Blob`'s
//! rows here and no row at all for `pop`, `shift` and `reallocate`
//! (`X::Method::NotFound`). The recognition table folds `Buf`/`Blob`/`utf8`
//! into one owner; what the rows add is the receiver's mutability.
//!
//! A named binding is written through the interpreter's by-name routines
//! (`buf_mutate_method`, `buf_pop_shift_splice`, `buf_reallocate`), which
//! re-seat the binding through its shared cell. A receiver with no name (a
//! chained call such as `Buf.new($s.encode).append($body)`) has no container
//! to write back to, so the mutation is done on a copy and the copy is the
//! answer, as the by-value cascade arms always did.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::value::value_buf::{BufEnd, buf_attrs_extended, buf_elems_or_empty, make_buf};
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Buf",
            name: $name,
            arity: 0,
            handler: Handler::Mut($handler),
            flags: RowFlags::SLURPY,
            named: &[],
        }
    };
}

/// A `Blob` proto with no candidate for a `Blob:D` invocant.
macro_rules! blob_row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Blob",
            name: $name,
            arity: 0,
            handler: Handler::Mut($handler),
            flags: RowFlags::SLURPY,
            named: &[],
        }
    };
}

/// Every row is slurpy from zero arguments, as the `Array` mutators are.
pub(super) static ROWS: &[MethodRow] = &[
    row!("push", push_row),
    row!("append", append_row),
    row!("unshift", unshift_row),
    row!("prepend", prepend_row),
    row!("pop", pop_row),
    row!("shift", shift_row),
    row!("splice", splice_row),
    row!("reallocate", reallocate_row),
    blob_row!("push", blob_push_row),
    blob_row!("append", blob_append_row),
    blob_row!("unshift", blob_unshift_row),
    blob_row!("prepend", blob_prepend_row),
];

macro_rules! blob_handler {
    ($fn:ident, $method:literal) => {
        // Cost: O(a), a = arguments (their type names, for the message).
        fn $fn(
            interp: &mut Interpreter,
            place: &mut ReceiverPlace<'_>,
            args: &[Value],
            _named: Named<'_>,
        ) -> Option<Result<Value, RuntimeError>> {
            let ValueView::Instance { class_name, .. } = place.value().descalarize().view() else {
                return None;
            };
            let profile = interp.format_call_arg_profile(args);
            Some(Err(
                crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
                    $method,
                    &class_name.resolve(),
                    true,
                    &profile,
                    &[],
                ),
            ))
        }
    };
}

blob_handler!(blob_push_row, "push");
blob_handler!(blob_append_row, "append");
blob_handler!(blob_unshift_row, "unshift");
blob_handler!(blob_prepend_row, "prepend");

macro_rules! handler {
    ($fn:ident, $method:literal) => {
        // Cost: see `run`.
        fn $fn(
            interp: &mut Interpreter,
            place: &mut ReceiverPlace<'_>,
            args: &[Value],
            _named: Named<'_>,
        ) -> Option<Result<Value, RuntimeError>> {
            run(interp, place, $method, args)
        }
    };
}

handler!(push_row, "push");
handler!(append_row, "append");
handler!(unshift_row, "unshift");
handler!(prepend_row, "prepend");
handler!(pop_row, "pop");
handler!(shift_row, "shift");
handler!(splice_row, "splice");
handler!(reallocate_row, "reallocate");

/// Apply `method` to the `Buf` the place holds.
// Cost: O(k) for a push, append, unshift or prepend of k elements, O(e) for a
// pop, shift, splice or reallocate of an e-element buffer (the elements are
// decoded once).
fn run(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let target = place.value().descalarize().clone();
    if !matches!(target.view(), ValueView::Instance { .. }) {
        return None;
    }
    // An element that is an object rather than an Int (a `CArray[uint8]` slot
    // reference, `UIntPosRef`) contributes its integer value, as rakudo's
    // `append` coerces each element.
    let coerced;
    let args = if matches!(method, "push" | "append" | "unshift" | "prepend") {
        match interp.coerce_buf_element_objects(args) {
            Ok(items) => {
                coerced = items;
                &coerced[..]
            }
            Err(e) => return Some(Err(e)),
        }
    } else {
        args
    };
    let Some(name) = place.name().map(str::to_string) else {
        return by_value(&target, method, args);
    };
    Some(match method {
        "reallocate" => interp.buf_reallocate(&name, target, args),
        "pop" | "shift" | "splice" => {
            interp.buf_pop_shift_splice(&name, target, method, args.to_vec())
        }
        _ => interp.buf_mutate_method(&name, target, method, args.to_vec()),
    })
}

/// The mutation of a receiver with no binding: done on a copy, which is the
/// answer. `pop` and `shift` answer the element they would remove and leave the
/// buffer alone.
// Cost: O(e + k), e = elements, k = arguments' elements.
fn by_value(target: &Value, method: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
        id,
        ..
    } = target.view()
    else {
        return None;
    };
    Some(match method {
        "push" | "append" | "unshift" | "prepend" => {
            // A `Str` element is a type error, as it is for a named buffer.
            if let Some(bad) = args.iter().find(|a| matches!(a.view(), ValueView::Str(_))) {
                return Some(Err(Interpreter::buf_element_type_error(bad)));
            }
            let new_items = Interpreter::flatten_buf_args(args.to_vec());
            let end = if matches!(method, "append" | "push") {
                BufEnd::Back
            } else {
                BufEnd::Front
            };
            // Only the new elements are encoded; the existing ones are carried
            // across without being decoded to boxed `Value`s (#7680).
            //
            // A receiver that is a stored object -- an accessor's result
            // (`$obj.buffer.append(..)`), a hash or array element -- is the
            // very `Buf` its container holds, and rakudo mutates that object.
            // The buffer's storage is shared by every alias of it (the named
            // path above extends the same cell), so grow it in place. A
            // `Blob` is immutable and keeps the copy.
            if !crate::runtime::utils::is_blob_like_class(&class_name.resolve()) {
                crate::value::value_buf::extend_buf_elems(&attributes, class_name, &new_items, end);
                return Some(Ok(Value::instance_sharing_cell(
                    &attributes,
                    class_name,
                    id,
                )));
            }
            let attrs = buf_attrs_extended(&attributes, class_name, &new_items, end);
            Ok(Value::make_instance(class_name, attrs))
        }
        "pop" | "shift" => {
            let bytes = buf_elems_or_empty(&attributes);
            let element = if method == "pop" {
                bytes.last()
            } else {
                bytes.first()
            };
            match element {
                Some(element) => Ok(element.clone()),
                None => Err(Interpreter::buf_empty_error(method)),
            }
        }
        "reallocate" => {
            let new_size = args.first().map_or(0, crate::runtime::to_int) as usize;
            let mut bytes = buf_elems_or_empty(&attributes);
            if let Err(e) = Interpreter::autoviv_resize(&mut bytes, new_size, Value::int(0)) {
                return Some(Err(e));
            }
            bytes.truncate(new_size);
            Ok(make_buf(class_name, bytes))
        }
        // A `splice` of a temporary has nothing to remove from; the cascade
        // answers it.
        _ => return None,
    })
}
