//! The `Buf` mutators (ADR-11276 §8.3, slice 3F remainder): `push`, `append`,
//! `unshift`, `prepend`, `pop`, `shift`, `splice` and `reallocate`.
//!
//! Rakudo declares them on `Buf`, not on `Blob`, so the rows are `Buf`'s
//! alone: a `Blob` or `utf8` receiver has no row and takes the cascade, which
//! refuses the call. The recognition table folds `Buf`/`Blob`/`utf8` into one
//! owner; what the rows add is the receiver's mutability.
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
];

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
pub(crate) fn by_value(
    target: &Value,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Instance {
        class_name,
        attributes,
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
