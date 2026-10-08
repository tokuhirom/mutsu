//! `raku` and `Str` on the collections, and `gist`, `WHICH` and `Stringy` on
//! `Capture` and `Seq` (ADR-11276 remainder: the rendering and identity names).
//!
//! Rakudo declares `raku` on `Array`, `List`, `Hash`, `Map`, `Pair`, `Seq` and
//! `Capture`, and `Str` on the same owners (bar `Array`) and `Range`; an immutable
//! `Map` is a `Hash` shape, which `Hash`'s rows answer. Each handler is the
//! rendering the cascades already shared: `raku_value` for a collection's source
//! form, `to_string_value` for its string form, the `capture_text` functions for
//! a `Capture`. `Str` declines what needs the interpreter: an element whose class
//! may define its own `Str` (`list_str_needs_interpreter`) and a lazy list that
//! must be forced. A zero-denominator rational inside the collection dies as its
//! own `.Str` does (GH #9608).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::capture_text::{capture_gist, capture_raku, capture_str};
use crate::value::raku_repr::raku_value;
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};

macro_rules! rows {
    ($name:literal => $handler:ident: $($owner:literal),*) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

macro_rules! interp_rows {
    ($name:literal => $handler:ident: $($owner:literal),*) => {
        &[$(MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: RowFlags::NONE,
            named: &[],
        }),*]
    };
}

/// An `Interp` row, because a typed container's `.raku` names its declared type
/// (`Array[Int].new(...)`), which the interpreter's container registry holds.
pub(super) static RAKU_ROWS: &[MethodRow] = interp_rows!("raku" => raku_row:
    "Array", "List", "Hash", "Pair", "Seq", "Capture");
pub(super) static STR_ROWS: &[MethodRow] = rows!("Str" => str_row:
    "List", "Hash", "Pair", "Seq", "Capture", "Range");
pub(super) static CAPTURE_ROWS: &[MethodRow] = rows!("gist" => capture_gist_row: "Capture");
pub(super) static STRINGY_ROWS: &[MethodRow] = rows!("Stringy" => str_row: "Seq");

/// The `raku` rows' handler: [`raku`] unless the collection needs the
/// interpreter to render. An element whose own `raku` is reachable only through
/// method dispatch (a user instance, a built-in object type) would print its
/// default form, and a container with a declared element or key type renders as
/// `Array[Int].new(...)` / `Hash[Int,Str].new(...)` from the interpreter's
/// registry; both decline to the interpreter's own rendering.
// Cost: O(t) to probe the elements for a dispatch leaf, t = nodes reachable;
// then [`raku`]'s own.
fn raku_row(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    if crate::runtime::container_needs_raku_dispatch(target) {
        return None;
    }
    match target.view() {
        ValueView::Hash(_) if interp.container_type_metadata(target).is_some() => return None,
        ValueView::Array(..)
            if interp
                .container_type_metadata(target)
                .is_some_and(|info| info.value_type != "Any" && info.value_type != "Mu") =>
        {
            return None;
        }
        _ => {}
    }
    raku(target, args)
}

/// `.raku` of a collection: the text that evaluates back to it.
// Cost: O(n), n = chars of the rendering (nested aggregates render in full).
pub(crate) fn raku(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        // A lazy array renders a bounded placeholder, not its capped backing.
        ValueView::Array(_, ArrayKind::Lazy) => Some(Ok(Value::str_from("[...]"))),
        ValueView::Array(..)
        | ValueView::Seq(..)
        | ValueView::Hash(..)
        | ValueView::Pair(..)
        | ValueView::ValuePair(..) => Some(Ok(Value::str(raku_value(target)))),
        ValueView::Capture { positional, named } => {
            Some(Ok(Value::str(capture_raku(positional, named))))
        }
        _ => None,
    }
}

/// `.Str` of a collection: its elements stringified, joined as `to_string_value`
/// joins them.
// Cost: O(n) in the elements; O(1) for a lazy array (a placeholder).
pub(crate) fn str_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Capture { positional, named } => {
            return Some(Ok(Value::str(capture_str(positional, named))));
        }
        // A lazy array stringifies to a bounded placeholder.
        ValueView::Array(_, ArrayKind::Lazy) => return Some(Ok(Value::str_from("..."))),
        ValueView::Array(..)
        | ValueView::Seq(..)
        | ValueView::Hash(..)
        | ValueView::Pair(..)
        | ValueView::ValuePair(..) => {
            if let Some(error) = crate::runtime::utils::zero_denominator_rational_error(target) {
                return Some(Err(error));
            }
            if crate::runtime::Interpreter::list_str_needs_interpreter(target) {
                return None;
            }
        }
        _ if target.is_range() => {}
        _ => return None,
    }
    Some(Ok(Value::str(target.to_string_value())))
}

/// `Capture.gist`: the call shape, `\(1, 2, :a(3))`.
// Cost: O(n), n = chars of the rendering.
fn capture_gist_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Capture { positional, named } => {
            Some(Ok(Value::str(capture_gist(positional, named))))
        }
        _ => None,
    }
}
