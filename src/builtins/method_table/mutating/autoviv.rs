//! `Any`'s `push`, `append`, `unshift` and `prepend`, and `Range`'s six
//! mutators (ADR-11276 §9.164).
//!
//! Rakudo declares the four growing methods on `Any` for an undefined
//! invocant only (`multi method push(Any:U \SELF: |values)`): the container
//! autovivifies to an `Array`, which then grows as `Array`'s row does. A
//! defined invocant that declares no mutator of its own (`5`, `"s"`) matches
//! no candidate (`X::Multi::NoMatch`); `pop` and `shift` are not declared on
//! `Any` at all, so they are `X::Method::NotFound` without a row.
//!
//! `Range` declares the six mutators to refuse them (`X::Immutable`); it has no
//! `splice`, which `Any`'s proto answers with no candidate.
//!
//! A named binding is autovivified here (`Interpreter::vivify_array_binding`,
//! §9.171) and grows through `Array`'s row as the binding it now is. An
//! undefined receiver with no binding (the value of an expression, as an
//! out-of-range element read through the interpreter) answers the fresh
//! `Array`, which the caller stores where the value came from.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::methods_signature_errors::{make_multi_no_match_error, make_x_immutable_error};
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;
type Grow = fn(&mut Interpreter, &mut ReceiverPlace<'_>, &[Value], Named<'_>) -> Answer;

macro_rules! row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Mut($handler),
            flags: RowFlags::SLURPY,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Any", "push", any_push),
    row!("Any", "append", any_append),
    row!("Any", "unshift", any_unshift),
    row!("Any", "prepend", any_prepend),
    row!("Range", "push", range_push),
    row!("Range", "append", range_append),
    row!("Range", "unshift", range_unshift),
    row!("Range", "prepend", range_prepend),
    row!("Range", "pop", range_pop),
    row!("Range", "shift", range_shift),
];

macro_rules! any_grow {
    ($fn:ident, $name:literal, $array_row:path) => {
        // Cost: as `Array`'s row, O(k) for k elements.
        fn $fn(
            interp: &mut Interpreter,
            place: &mut ReceiverPlace<'_>,
            args: &[Value],
            named: Named<'_>,
        ) -> Answer {
            grow(interp, place, $name, args, named, $array_row)
        }
    };
}

any_grow!(any_push, "push", super::array::push_row);
any_grow!(any_append, "append", super::array::append_row);
any_grow!(any_unshift, "unshift", super::array::unshift_row);
any_grow!(any_prepend, "prepend", super::array::prepend_row);

/// `Any.push` and its kin: autovivify an undefined receiver to an `Array` and
/// grow it through `Array`'s row; refuse a defined scalar.
// Cost: as `Array`'s row, O(k) for k elements, plus one env write to vivify a
// binding.
fn grow(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    method: &str,
    args: &[Value],
    named: Named<'_>,
    array_row: Grow,
) -> Answer {
    let value = place.value().descalarize();
    // An `Array` type object in a named binding (`my Array $a; $a.push(1)`)
    // vivifies as an undefined `Any` does.
    let array_type = matches!(value.view(), ValueView::Package(name) if name == "Array");
    match value.view() {
        ValueView::Nil => {}
        ValueView::Package(name) if name == "Any" || name == "Mu" => {}
        ValueView::Package(_) if array_type && place.name().is_some() => {}
        ValueView::Int(_)
        | ValueView::Num(_)
        | ValueView::Str(_)
        | ValueView::Bool(_)
        | ValueView::Rat(..)
        | ValueView::Complex(..) => return Some(Err(make_multi_no_match_error(method))),
        _ => return None,
    }
    if let Some(name) = place.name() {
        let name = name.to_string();
        let vivified = interp.vivify_array_binding(&name);
        let mut array = place.rebound(&vivified)?;
        return array_row(interp, &mut array, args, named);
    }
    let mut fresh = Value::real_array(Vec::new());
    let mut array = ReceiverPlace::detached(&mut fresh);
    match array_row(interp, &mut array, args, named)? {
        Ok(_) => Some(Ok(fresh)),
        Err(err) => Some(Err(err)),
    }
}

macro_rules! range_refusal {
    ($fn:ident, $name:literal) => {
        // Cost: O(1).
        fn $fn(
            _interp: &mut Interpreter,
            _place: &mut ReceiverPlace<'_>,
            _args: &[Value],
            _named: Named<'_>,
        ) -> Answer {
            Some(Err(make_x_immutable_error($name, "Range")))
        }
    };
}

range_refusal!(range_push, "push");
range_refusal!(range_append, "append");
range_refusal!(range_unshift, "unshift");
range_refusal!(range_prepend, "prepend");
range_refusal!(range_pop, "pop");
range_refusal!(range_shift, "shift");
