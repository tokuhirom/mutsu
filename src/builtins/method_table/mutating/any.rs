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
use crate::runtime::instance_backing::InstanceBacking;
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
    // See through the wrappers a call receiver can carry: a cell around the
    // value and the transient variable reference of an `is rw` argument. A
    // `Mixin` stays one (its type name is `Int+{Foo}`), but what it wraps is
    // what decides whether another entry resolves the call.
    let unwrapped = crate::runtime::types::unwrap_varref_value_for_dispatch(place.value());
    let target = unwrapped.descalarize();
    let inner = match target.view() {
        ValueView::Mixin(inner, _) => inner.descalarize().clone(),
        _ => target.clone(),
    };
    // An `Array` and a `Buf` have the real `splice` (the rows before this one in
    // the chain, reached here only because a wrapper hid them from the owner
    // lookup), so they are not this row's.
    if matches!(inner.view(), ValueView::Array(..))
        || inner.dispatch_shape() == Some(crate::value::DispatchShape::Buf)
    {
        return None;
    }
    // An instance is answered here when nothing else resolves the call further:
    // not an `is Array` subclass (that reaches `Array.splice` through the
    // bridges of `builtins_dispatch_next`), no user `splice` of its own (the
    // `FALLBACK` of a class is not consulted: `Any.splice` exists), and not a
    // user class rooted at `Mu` (`class C is Mu` has no `splice` at all).
    if let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = inner.view()
    {
        let class = class_name.resolve();
        if InstanceBacking::Array.is_in(&attributes)
            || interp.has_user_method(&class, "splice")
            || interp.is_user_class_without_any(&class)
            // A punned role (`role Ser does DS` with `DS.splice`) keeps the
            // methods of the roles it composes.
            || interp.role_or_parent_has_method(&class, "splice")
        {
            return None;
        }
    }
    // A type object of a user class that declares `splice` answers itself.
    if let ValueView::Package(class_name) = inner.view()
        && interp.has_user_method(&class_name.resolve(), "splice")
    {
        return None;
    }
    Some(Err(make_no_candidates_error("splice", target, args)))
}
