//! `Stash`'s rows (ADR-11276 §9.198): `merge-symbols`, which Rakudo declares
//! on `Stash` and which a loader runs as
//! `GLOBALish.WHO.merge-symbols($handle.globalish-package)` to publish the
//! symbols a loaded unit declared (`CompUnit::Handle.globalish-package`).
//!
//! A `Stash` reached as a value is an instance of the opaque shape
//! `DispatchShape::Stash`: the table's owner lookup sees it, while the hash
//! and associative paths (which read `dispatch_shape`, not `receiver_shape`)
//! do not, so its indexing is unchanged. A `PseudoStash` (`MY::`) is not a
//! `Stash` and declares no `merge-symbols`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Stash",
    name: "merge-symbols",
    arity: 1,
    handler: Handler::Interp(merge_symbols),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];

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
