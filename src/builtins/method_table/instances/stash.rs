//! `Stash`'s rows (ADR-11276 §9.198): `merge-symbols`, which Rakudo declares
//! on `Stash` and which a loader runs as
//! `GLOBALish.WHO.merge-symbols($handle.globalish-package)` to publish the
//! symbols a loaded unit declared (`CompUnit::Handle.globalish-package`).
//!
//! A `Stash` reached as a value is an instance; the class has no dispatch
//! shape, so the rows are reached through their owner
//! (`RowFlags::OWNER_ONLY`, `invoke_owner`) from the instance dispatch, as
//! `Variable`'s are (§9.195). A `PseudoStash` (`MY::`) is not a `Stash` and
//! declares no `merge-symbols`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Stash",
    name: "merge-symbols",
    arity: 1,
    handler: Handler::Interp(merge_symbols),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];

/// The owners the rows reachable from `target` are found under: a `Stash`.
// Cost: O(1).
pub(crate) fn owners_of(target: &Value) -> Option<&'static [&'static str]> {
    match target.view() {
        ValueView::Instance { class_name, .. } if class_name == "Stash" => Some(&["Stash"]),
        _ => None,
    }
}

/// `Stash.merge-symbols(\globalish)`: makes the symbols the package carries
/// resolvable (`Interpreter::merge_global_symbols`).
// Cost: see `Interpreter::merge_global_symbols`.
fn merge_symbols(
    interp: &mut Interpreter,
    _target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(interp.merge_global_symbols(args.first()?)))
}
