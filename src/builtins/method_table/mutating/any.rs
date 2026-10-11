//! `Any.splice` (ADR-11276 §9.67): the proto Rakudo declares on `Any` with no
//! candidate of its own. `Array` and `Buf` declare the real `splice`, so a
//! receiver of any other kind (`5`, `"s"`, a `Range`, a `Hash`, a `Seq`, a
//! type object, an `Instance` that is no `Buf`) lands here and gets
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
    // A user class (or a type object of one) that declares `splice` answers
    // the call itself; `native_lever_a_user_override_sym` cannot see it, an
    // instance's type name there being `Any`.
    if let ValueView::Instance { class_name, .. } | ValueView::Package(class_name) = target.view()
        && interp.has_user_method(&class_name.resolve(), "splice")
    {
        return None;
    }
    Some(Err(make_no_candidates_error("splice", target, args)))
}
