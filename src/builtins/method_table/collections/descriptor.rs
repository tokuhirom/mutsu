//! `Array.name`, `Hash.name`, `Array.dynamic` and `Hash.dynamic` (ADR-11276
//! §9.67): the container descriptor's two properties, read off the *value*.
//!
//! They are not "the binding's name". Rakudo keeps the descriptor on the
//! `Array`/`Hash` object itself, so `my $r = @a; $r.name` is `@a`, `f(@a)`'s
//! `@x.name` is `@a`, and a container no declaration named (`[1, 2]`,
//! `Hash.new`) is `element`. mutsu stamps the same name on the container's data
//! (`ArrayData::descriptor_name`), so a plain `Handler::Narrow` row answers
//! every receiver and no `ReceiverPlace` is needed.
//!
//! What a row cannot see is the *variable*: `@a.name` and `@a.dynamic` written
//! on an `@`/`%` variable are rewritten by the compiler to `@a.VAR.name` /
//! `@a.VAR.dynamic` (`compile_expr_method_on_var`), whose `is_var_dynamic` and
//! `our`/`state`/attribute name lookups are variable metadata. That rewrite is a
//! compile-time decision about the call site, not a dispatch fallback.
//!
//! `List` has no descriptor and neither method (raku: "No such method"), so the
//! handlers decline a `List`-kind array.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{ArrayKind, RuntimeError, Value, ValueView};

const fn row(
    owner: &'static str,
    name: &'static str,
    handler: crate::builtins::method_table::NarrowFn,
) -> MethodRow {
    MethodRow {
        owner,
        name,
        arity: 0,
        handler: Handler::Narrow(handler),
        flags: RowFlags::NONE,
        named: &[],
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    row("Array", "name", name),
    row("Hash", "name", name),
    row("Array", "dynamic", dynamic),
    row("Hash", "dynamic", dynamic),
];

/// The descriptor name of a real `Array` or a `Hash`, `element` when no
/// declaration named the container.
// Cost: O(1).
pub(crate) fn name(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let descriptor = match target.view() {
        ValueView::Array(data, kind) if !matches!(kind, ArrayKind::List | ArrayKind::ItemList) => {
            data.descriptor_name.as_deref().map(str::to_string)
        }
        ValueView::Hash(data) => data.descriptor_name.as_deref().map(str::to_string),
        _ => return None,
    };
    Some(Ok(Value::str(
        descriptor.unwrap_or_else(|| "element".into()),
    )))
}

/// Always `False` on a container *value*: only a dynamic *variable*
/// (`@*a` / `%*h`) is dynamic, and that call is rewritten to `.VAR.dynamic`.
// Cost: O(1).
pub(crate) fn dynamic(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match target.view() {
        ValueView::Array(_, kind) if !matches!(kind, ArrayKind::List) => Some(Ok(Value::FALSE)),
        ValueView::Hash(_) => Some(Ok(Value::FALSE)),
        _ => None,
    }
}
