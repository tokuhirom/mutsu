//! `Any.splice` (ADR-11276 §9.67): the proto Rakudo declares on `Any` with no
//! candidate of its own. `Array` and `Buf` declare the real `splice`, so a
//! receiver of any other kind (`5`, `"s"`, a `Range`, a `Hash`, a `Seq`, a
//! type object) lands here and gets
//! `X::Multi::NoMatch`.
//!
//! A `Mut` row because that is how the owner chain is walked
//! ([`invoke_mut`](crate::builtins::method_table::invoke_mut)), though it
//! writes nothing: the by-value form and the named form are one answer.

use crate::builtins::method_table::{Handler, MethodRow, Named, ReceiverPlace, RowFlags};
use crate::runtime::Interpreter;
use crate::runtime::methods_signature_errors::make_no_candidates_error;
use crate::value::{RuntimeError, Value, ValueView};

/// Slurpy from zero: any argument list is "no candidates".
pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Any",
    name: "splice",
    arity: 0,
    handler: Handler::Mut(splice_row),
    flags: RowFlags::SLURPY,
    named: &[],
}];

// Cost: O(d + a), d = MRO depth of a user class receiver (memoized), a = arguments
// (one type name each for the message).
fn splice_row(
    interp: &mut Interpreter,
    place: &mut ReceiverPlace<'_>,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let target = place.value().descalarize();
    // Not the value's own kind, or a receiver other entries resolve further:
    // a cell or variable reference around the receiver (it may well hold an
    // `Array`), an instance (an `is Array` / `is Hash` / `is Bag` subclass
    // reaches the row of its native storage through the bridges of
    // `builtins_dispatch_next`; a user class may declare `splice` or
    // `FALLBACK`) and a mixin around a native value. They keep their paths.
    if matches!(
        target.view(),
        ValueView::ContainerRef(_)
            | ValueView::VarRef { .. }
            | ValueView::Instance { .. }
            | ValueView::Mixin(..)
    ) {
        return None;
    }
    // A type object of a user class that declares `splice` answers itself.
    if let ValueView::Package(class_name) = target.view()
        && interp.has_user_method(&class_name.resolve(), "splice")
    {
        return None;
    }
    Some(Err(make_no_candidates_error("splice", target, args)))
}
