//! `Seq`'s own rows (ADR-11276 remainder: the `Seq` shape).
//!
//! A row is reached only by a *settled* `Seq` (`DispatchShape::Seq`): reified,
//! not lazy, not a `List` view, and only through the entries that run after the
//! consumption step (`reify_or_consume_seq_target`) has decided what the call
//! does to the body. The rows answer from the elements the step left in it, so a
//! method that consumes a `Seq` still consumes it.
//!
//! Most rows share the handler of the `List` or `Range` row for the same name:
//! a settled `Seq` reads as its elements.

use super::{Handler, MethodRow, RowFlags, lazy, list, positional};
use crate::builtins::methods_0arg::coercion::value_to_capture;
use crate::builtins::methods_0arg::is_value_lazy;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! pure {
    ($name:literal, $handler:path) => {
        MethodRow {
            owner: "Seq",
            name: $name,
            arity: 0,
            handler: Handler::Pure($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

macro_rules! narrow {
    ($name:literal, $handler:path) => {
        MethodRow {
            owner: "Seq",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

macro_rules! narrow_at {
    ($name:literal, $arity:literal, $handler:path) => {
        MethodRow {
            owner: "Seq",
            name: $name,
            arity: $arity,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    pure!("elems", list::elems),
    pure!("end", list::end),
    pure!("Bool", list::bool),
    narrow!("item", lazy::item),
    narrow!("hyper", lazy::hyper),
    narrow!("race", lazy::race),
    narrow!("lazy", lazy::lazy),
    narrow!("is-lazy", is_lazy),
    narrow!("Slip", slip),
    narrow!("List", list_type),
    narrow!("list", list_value),
    narrow!("Array", array_value),
    narrow!("cache", cache),
    narrow!("Capture", capture),
    narrow!("reverse", list::reverse),
    narrow!("sink", sink),
    narrow!("head", head_one),
    narrow_at!("head", 1, list::head),
    narrow_at!("join", 0, list::join),
    narrow_at!("join", 1, list::join),
    narrow_at!("AT-POS", 1, positional::at_pos),
    narrow_at!("EXISTS-POS", 1, positional::exists_pos),
];

/// `Seq.is-lazy` of a settled `Seq`: its own flag, which a settled body has clear.
// Cost: O(1).
fn is_lazy(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    args.is_empty()
        .then(|| Ok(Value::truth(is_value_lazy(target))))
}

/// The elements of a `Seq` for a coercion that keeps it reusable (Rakudo's
/// `Seq.List`/`.Slip`/`.list`/`.Array` go through `.cache`): marks the body
/// cached so a later touch is served from the stored elements.
// Cost: O(e), e = elements copied.
fn cached_items(target: &Value) -> Option<Result<Vec<Value>, RuntimeError>> {
    let ValueView::Seq(items) = target.view() else {
        return None;
    };
    if items.is_consumed() && !items.is_cached() {
        return Some(Err(crate::value::seq_consumed_error()));
    }
    // TODO: implement proper @-sigil parameter caching separately, and make
    // `.List` on an uncached Seq consume it (strict Raku semantics).
    items.mark_cache_requested();
    Some(Ok(items.to_vec()))
}

/// `Seq.Slip`.
// Cost: O(e), e = elements.
pub(crate) fn slip(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    cached_items(target).map(|items| items.map(Value::slip))
}

/// `Seq.List`.
// Cost: O(e), e = elements.
pub(crate) fn list_type(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    cached_items(target).map(|items| items.map(Value::array))
}

/// `Seq.list`/`Seq.Array`: `.Array` builds a real Array, whose elements are
/// `Scalar` containers, so aggregates itemize on the way in
/// (`((1,2),(3,4)).Seq.Array[0].raku` is `$(1, 2)`); `.list` builds a List.
// Cost: O(e), e = elements.
pub(crate) fn listify(target: &Value, want_array: bool) -> Option<Result<Value, RuntimeError>> {
    cached_items(target).map(|items| {
        items.map(|items| {
            if want_array {
                crate::runtime::utils::itemize_real_array_elements(Value::real_array(items))
            } else {
                Value::array(items)
            }
        })
    })
}

fn list_value(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    args.is_empty().then(|| listify(target, false))?
}

fn array_value(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    args.is_empty().then(|| listify(target, true))?
}

/// `Seq.cache`: rakudo's `.cache` is itself lazy and returns a `List`-typed value,
/// not a `Seq`-typed one. Flag the body so the next touch reifies-and-keeps instead
/// of consuming, and return a second handle over the SAME core (ADR-0038 phase 3):
/// both observe a later reification, and a reified body shares its elements.
// Cost: O(1).
pub(crate) fn cache(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let ValueView::Seq(body) = target.view() else {
        return None;
    };
    body.mark_cache_requested();
    let body = std::sync::Arc::clone(&body);
    Some(Ok(Value::seq_list_view(&body)))
}

/// `Seq.Capture`: the elements as positional arguments.
// Cost: O(e), e = elements.
fn capture(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    args.is_empty().then(|| value_to_capture(target))
}

/// `Seq.sink` of a settled `Seq`: nothing is left to pull.
// Cost: O(1).
fn sink(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    let ValueView::Seq(body) = target.view() else {
        return None;
    };
    // No pull to run: a settled body is reified, so `sink`'s closure is unreachable.
    let _ = body.sink_explicit(|_| unreachable!("a settled Seq has no source to pull"));
    Some(Ok(Value::NIL))
}

/// `Seq.head`: the first element, `Nil` for an empty `Seq`.
// Cost: O(1).
fn head_one(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if !args.is_empty() {
        return None;
    }
    Some(Ok(crate::runtime::with_receiver_items(target, |items| {
        items.first().cloned().unwrap_or(Value::NIL)
    })))
}
