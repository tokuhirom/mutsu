//! `Mu.isa` (ADR-11276 slice 5, §9.104): whether the receiver's type is the
//! argument's or derives from it.
//!
//! Rakudo declares `isa(Mu \type)` and `isa(Str:D $name)` on `Mu`, so every
//! receiver reaches the row: a shape through its owners or the audited `Mu`
//! answers, a receiver with no shape through
//! [`crate::builtins::method_table::answer_shapeless`]. An instance of a
//! class with no shape, a type object and a mixin over an instance decline: a
//! user class hierarchy, a role and a subset are the interpreter's to read.

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Mu",
    name: "isa",
    arity: 1,
    handler: Handler::Narrow(isa),
    // The argument is a type object, a name or any value whose type is meant.
    flags: RowFlags::ANY_ARGS,
    named: &[],
}];

/// `Mu.isa`: a type object or a `Str` names the type; any other value
/// stands for its own type (`3.5.isa(4)` asks for `Int`). A parameterized type
/// (`Array[Hash]`) is the typed container's own type.
// Cost: O(d), d = depth of the receiver's built-in type chain, plus O(n) for a
// parameterized name, n = chars.
fn isa(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        // A built-in class with a shape (`Date`) knows its own type chain.
        ValueView::Instance { .. } if target.dispatch_shape().is_none() => return None,
        ValueView::Package(_) => return None,
        ValueView::Mixin(inner, _) if matches!(inner.view(), ValueView::Instance { .. }) => {
            return None;
        }
        _ => {}
    }
    let arg = args.first()?;
    let type_name = match arg.view() {
        ValueView::Package(name) => name.resolve(),
        ValueView::Str(name) => name.to_string(),
        ValueView::Instance { class_name, .. } => class_name.resolve(),
        _ => crate::value::types::what_type_name(arg),
    };
    if type_name.contains('[') {
        return Some(Ok(Value::truth(
            crate::runtime::embedded_container_type_name(target).as_deref()
                == Some(type_name.as_str()),
        )));
    }
    Some(Ok(Value::truth(target.isa_check(&type_name))))
}
