//! The renderings a user class gets from composing `Dateish` (ADR-11276
//! §9.181): Rakudo's `multi method Str(Dateish:D:) { self!formatter }` and
//! `multi method gist(Dateish:D:) { self.Str }`.
//!
//! The role carries no public formatter: `Date` and `DateTime` implement the
//! private `!formatter` themselves, and so must a composing class (TOML::Thumb's
//! `Time::Local`); without one the private call raises Rakudo's "No such private
//! method". The built-in classes and their subclasses render natively and are
//! not answered here.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Dateish",
        name: "Str",
        arity: 0,
        handler: Handler::Interp(formatted),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Dateish",
        name: "gist",
        arity: 0,
        handler: Handler::Interp(formatted),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];

/// `self!formatter`, called as from a method of the instance's own class: the
/// role's method, once composed, resolves the private call against it.
// Cost: one private-method call, plus the user code it runs.
fn formatted(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Answer {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    let class = class_name.resolve();
    if !interp.class_does_role(&class, "Dateish")
        || interp
            .class_mro(&class)
            .iter()
            .any(|c| matches!(c.as_str(), "Date" | "DateTime"))
    {
        return None;
    }
    interp.push_method_class(&class);
    let result = interp.call_method_with_values(target.clone(), "!formatter", Vec::new());
    interp.pop_method_class();
    Some(result)
}
