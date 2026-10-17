//! The rows of two result objects the runtime mints (ADR-11276 §9.160):
//! `Routine::WrapHandle` (what `.wrap` answers) and `HyperConfiguration`
//! (what `HyperSeq.configuration` answers).
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
];

/// The owners whose rows answer `target`: a `WalkList`, a
/// `Routine::WrapHandle` or a `HyperConfiguration` instance.
// Cost: O(1).
pub(crate) fn runtime_object_owners_of(target: &Value) -> Option<&'static [&'static str]> {
    let ValueView::Instance { class_name, .. } = target.view() else {
        return None;
    };
    Some(match class_name.as_str() {
        "WalkList" => &["WalkList"],
        "Routine::WrapHandle" => &["Routine::WrapHandle"],
        "HyperConfiguration" => &["HyperConfiguration"],
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
