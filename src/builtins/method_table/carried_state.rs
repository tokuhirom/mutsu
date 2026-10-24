//! The owners of an instance that carries a built-in class's state (ADR-11276,
//! #14126).
//!
//! An instance of a *user subclass* of `Proc::Async`, `Supplier` or
//! `IterationBuffer` keeps the built-in's state in a reserved attribute, so its
//! owner chain is not a function of its class name. This is the one probe that
//! reads the instance's attributes once and maps the state attribute (or the
//! built-in class name itself) to the owners its rows are found under. The
//! attribute names live next to the code that seeds them. A stream-decoder
//! subclass is found by its MRO instead, which needs the interpreter
//! (`Interpreter::carried_state_owners_of`); a `Promise`/`Channel` payload is
//! read by `promise_channel_receiver_of`, which answers on the payload.

use super::io_concurrency::supplier_scheduler::SUPPLIER_SUBCLASS;
use crate::value::{Value, ValueView};

/// The reserved attribute an instance of a user subclass of a repository class
/// carries (`inst` for `CompUnit::Repository::Installation`, `file` for
/// `::FileSystem`): the repository's state is its `prefix` / `next-repo`
/// attributes, which the subclass instance holds itself.
pub(crate) const REPO_SUBCLASS: &str = "__mutsu_repo_subclass";

const INSTALLATION: &[&str] = &["CompUnit::Repository::Installation"];
const FILE_SYSTEM: &[&str] = &["CompUnit::Repository::FileSystem"];

/// `Proc::Async`'s state attribute (`Interpreter::proc_async_state`).
const PROC_ASYNC_STATE: &str = "__mutsu_proc_async_state";

const PROC_ASYNC: &[&str] = &["Proc::Async"];
const SUPPLIER: &[&str] = &["Supplier"];
const ITERATION_BUFFER: &[&str] = &["IterationBuffer"];

/// The owners whose rows answer `target`, an instance of a built-in class or
/// of a user subclass carrying its state; `None` for anything else.
// Cost: O(1): a class-name test and up to three attribute-map probes.
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    match class_name.as_str() {
        "Proc::Async" => return Some(PROC_ASYNC),
        "Supplier" | "Supplier::Preserving" => return Some(SUPPLIER),
        "IterationBuffer" => return Some(ITERATION_BUFFER),
        _ => {}
    }
    let attributes = attributes.as_map();
    if attributes.contains_key(PROC_ASYNC_STATE) {
        Some(PROC_ASYNC)
    } else if let Some(kind) = attributes.get(REPO_SUBCLASS) {
        Some(if kind.to_string_value() == "file" {
            FILE_SYSTEM
        } else {
            INSTALLATION
        })
    } else if attributes.contains_key(SUPPLIER_SUBCLASS) {
        Some(SUPPLIER)
    } else if attributes
        .get(crate::runtime::nqp_ops_list::iteration_buffer_items_key())
        .is_some_and(|items| matches!(items.view(), ValueView::Array(..)))
    {
        Some(ITERATION_BUFFER)
    } else {
        None
    }
}
