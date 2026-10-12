//! The list terminals on the owners Rakudo declares them on (ADR-11276 slice 3C
//! remainder): the handlers are the ones `Any`, `List` and `Seq` already use,
//! registered once more on the declaring owner so the table, `.^can` and the
//! resolver see them where Rakudo does. Every handler declines what it does not
//! cover, so a receiver it declines takes the cascade as before.

use super::{Handler, MethodRow, RowFlags, list, list_aggregate, list_transform};
use crate::runtime;
use crate::value::{RuntimeError, Value, ValueView};

const fn narrow(
    owner: &'static str,
    name: &'static str,
    arity: u8,
    handler: crate::builtins::method_table::NarrowFn,
) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity,
        handler: Handler::Narrow(handler),
        flags: RowFlags::NONE,
        named: &[],
    }
}

/// `Any.head` and `Any.tail` are `self.list.head` / `self.list.tail`, which a
/// type object answers too (`Int.head` is `Int`, `Int.tail(2)` is `(Int,)`).
const fn any_type_object_ok(
    name: &'static str,
    arity: u8,
    handler: crate::builtins::method_table::NarrowFn,
) -> MethodRow {
    MethodRow {
        owner: "Any",
        name,
        arity,
        handler: Handler::Narrow(handler),
        flags: RowFlags::TYPE_OBJECT_OK,
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    any_type_object_ok("head", 0, head_one),
    narrow("List", "head", 0, head_one),
    narrow("Map", "head", 0, head_one),
    any_type_object_ok("tail", 0, tail_one),
    any_type_object_ok("head", 1, list::head),
    any_type_object_ok("tail", 1, list::tail),
    narrow("List", "tail", 0, tail_one),
    narrow("Array", "tail", 0, tail_one),
    narrow("List", "head", 1, list::head),
    narrow("Map", "head", 1, list::head),
    narrow("List", "tail", 1, list::tail),
    narrow("Array", "tail", 1, list::tail),
    narrow("List", "sum", 0, list_aggregate::sum),
    list_transform::flat_row("Map"),
    list_transform::flat_row("Range"),
];

/// `.head` with no count: the first element, `Nil` when there is none. Every
/// owner's row (`Any`, `List`, `Map`, `Seq`) and the receivers with no shape
/// (through `method_table::answer_shapeless`) share it. A user-class instance
/// declines: its own `head` attribute or method, or its list, is the
/// interpreter's to find. A built-in class with a shape (`Date`) reads as one
/// element, itself.
// Cost: O(1) on an Array, a List, a reified Seq or a Range; O(e) on any other
// list-like, e = elements of the invocant (decomposed into a Vec to read one
// slot).
pub(crate) fn head_one(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let first = |items: &[Value]| items.first().cloned().unwrap_or(Value::NIL);
    Some(Ok(match target.view() {
        ValueView::Instance { class_name, .. } if user_instance(class_name) => return None,
        // Argless `.head` reads the backing store raw (Rakudo's Array.head
        // candidate): a hole at index 0 yields `Nil`, not the vivified `Any`
        // that iteration (`for @a`, `.head(n)`) would produce. An `Array`'s
        // element comes back as its container, as rakudo's `Array.head` hands
        // out the element itself: `$_ = 9 with @a.head` and `my $x := @a.head`
        // write into `@a`.
        ValueView::Array(items, kind) => {
            if items.hole_at(0) || items.is_empty() {
                Value::NIL
            } else if !kind.is_immutable_list() {
                target
                    .array_slot_ref(0, true)
                    .unwrap_or_else(|| items.first().cloned().unwrap_or(Value::NIL))
            } else {
                items.first().cloned().unwrap_or(Value::NIL)
            }
        }
        ValueView::Range(start, end) if start > end => Value::NIL,
        ValueView::Range(start, _) => Value::int(start),
        ValueView::RangeExcl(start, end) if start >= end => Value::NIL,
        ValueView::RangeExcl(start, _) => Value::int(start),
        ValueView::RangeExclBoth(start, end) if start + 1 >= end => Value::NIL,
        ValueView::RangeExclBoth(start, _) => Value::int(start + 1),
        ValueView::RangeExclStart(start, end) if start >= end => Value::NIL,
        ValueView::RangeExclStart(start, _) => Value::int(start + 1),
        ValueView::GenericRange { .. }
            if let Some(first) = runtime::unbounded_range::first(target) =>
        {
            first
        }
        ValueView::GenericRange {
            start, excl_start, ..
        } => {
            let items = runtime::value_to_list(target);
            if items.is_empty() {
                Value::NIL
            } else if excl_start {
                match start.view() {
                    ValueView::Int(n) => Value::int(n + 1),
                    _ => first(&items),
                }
            } else {
                (**start).clone()
            }
        }
        // ADR-0040: decompose the RECEIVER into its own elements, ignoring its
        // own itemization (see `value_to_list_for_receiver`'s doc comment).
        _ => runtime::with_receiver_items(target, first),
    }))
}

/// `.tail` with no count: the last element, `Nil` when there is none (the
/// `Any`, `List` and `Array` rows). A user-class instance declines, as for
/// [`head_one`].
// Cost: O(1) on an Array, a List or a reified Seq; O(e) otherwise, e =
// elements of the invocant (decomposed into a Vec to read the last slot).
pub(crate) fn tail_one(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let last = |items: &[Value]| items.last().cloned().unwrap_or(Value::NIL);
    Some(Ok(match target.view() {
        ValueView::Instance { class_name, .. } if user_instance(class_name) => return None,
        // As for `.head`: an `Array`'s last element comes back as its container.
        ValueView::Array(items, kind) => match items.len().checked_sub(1) {
            Some(i) if !kind.is_immutable_list() && !items.hole_at(i) => target
                .array_slot_ref(i, true)
                .unwrap_or_else(|| items.last().cloned().unwrap_or(Value::NIL)),
            _ => items.last().cloned().unwrap_or(Value::NIL),
        },
        _ => runtime::with_receiver_items(target, last),
    }))
}

/// Whether an instance of `class` is one of a class with no dispatch shape (a
/// user class, `Supply`), whose list or own method only the interpreter finds.
// Cost: O(s), s = instance-backed shapes.
fn user_instance(class: crate::symbol::Symbol) -> bool {
    crate::value::DispatchShape::from_instance_class(class).is_none()
}
