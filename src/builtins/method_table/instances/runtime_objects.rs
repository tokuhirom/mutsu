//! The rows of the objects the runtime mints (ADR-11276 §9.160):
//! `Routine::WrapHandle` (what `.wrap` answers), `HyperConfiguration`
//! (what `HyperSeq.configuration` answers) and `Pod::Block::Declarator`
//! (what a `#|` / `#=` comment builds).
//!
//! Neither has a shape, so every row is `OWNER_ONLY`, found through
//! [`runtime_object_owners_of`] from `call_method_with_values`, together with
//! `WalkList`'s (§9.140).

use super::{Handler, MethodRow, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "Routine::WrapHandle",
        name: "restore",
        arity: 0,
        handler: Handler::Interp(|interp, target, _, _| restore(interp, target)),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "HyperConfiguration",
        name: "batch",
        arity: 0,
        handler: Handler::Narrow(|target, _| attribute(target, "batch")),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "HyperConfiguration",
        name: "degree",
        arity: 0,
        handler: Handler::Narrow(|target, _| attribute(target, "degree")),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "Pod::Block::Declarator",
        name: "_add_leading",
        arity: 1,
        handler: Handler::Narrow(|target, args| add_declarator_text(target, "leading", &args[0])),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "Pod::Block::Declarator",
        name: "_add_trailing",
        arity: 1,
        handler: Handler::Narrow(|target, args| add_declarator_text(target, "trailing", &args[0])),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];

/// The owners whose rows answer `target`: a `WalkList`, a
/// `Routine::WrapHandle`, a `HyperConfiguration` or a
/// `Pod::Block::Declarator` instance.
// Cost: O(1).
pub(crate) fn runtime_object_owners_of(target: &Value) -> Option<&'static [&'static str]> {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    Some(match class_name.as_str() {
        "WalkList" => &["WalkList"],
        "Routine::WrapHandle" => &["Routine::WrapHandle"],
        "HyperConfiguration" => &["HyperConfiguration"],
        "Pod::Block::Declarator" => &["Pod::Block::Declarator"],
        _ => return None,
    })
}

// Cost: O(w), w = wrappers in the chain.
fn restore(interp: &mut Interpreter, target: &Value) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let attributes = attributes.to_map();
    Some(interp.wrap_handle_restore(&attributes))
}

/// The receiver's attribute `name`; `None` (the ordinary dispatch) when unset.
// Cost: O(1).
fn attribute(target: &Value, name: &str) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    attributes.as_map().get(name).cloned().map(Ok)
}

/// `Pod::Block::Declarator._add_leading` / `._add_trailing`: Rakudo builds a
/// declarator pod block by appending each `#|` / `#=` comment through them,
/// space-joining the pieces; they are also the public way to build one by
/// hand for `.^set_why` (`Type/Metamodel/Documenting.rakudoc`). The
/// attribute cell is interior-mutable, so the append is visible through every
/// alias of the block (`my Pod::Block::Declarator $pod .= new;
/// $pod._add_leading(...); $pod`).
// Cost: O(n), n = chars of the accumulated text.
fn add_declarator_text(target: &Value, key: &str, addition: &Value) -> Answer {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let appended = {
        let map = attributes.as_map();
        // Only a non-empty Str counts as accumulated text: a fresh
        // `Pod::Block::Declarator.new` leaves the slot holding the `Any` type
        // object, which would otherwise stringify into the result.
        let prev = match map.get(key).map(Value::view) {
            Some(ValueView::Str(s)) => s.to_string(),
            _ => String::new(),
        };
        let added = addition.to_string_value();
        if prev.is_empty() {
            added
        } else {
            format!("{prev} {added}")
        }
    };
    attributes.insert(key, Value::str(appended.clone()));
    // `contents` is the rendered text `.Str`/`.gist` read (`value/display.rs`):
    // leading and trailing joined by a newline, an absent half contributing
    // nothing.
    let contents = {
        let map = attributes.as_map();
        let part = |k: &str| match map.get(k).map(Value::view) {
            Some(ValueView::Str(s)) if !s.is_empty() => Some(s.to_string()),
            _ => None,
        };
        match (part("leading"), part("trailing")) {
            (Some(l), Some(t)) => format!("{l}\n{t}"),
            (Some(l), None) => l,
            (None, Some(t)) => t,
            (None, None) => String::new(),
        }
    };
    attributes.insert("contents", Value::str(contents));
    // Rakudo hands back the raw `@!leading` / `@!trailing` array. mutsu stores
    // the joined text (the shape `.leading` itself reports), so the
    // accumulated string is returned; every documented idiom uses the call in
    // sink context.
    Some(Ok(Value::str(appended)))
}
