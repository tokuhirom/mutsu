//! `Any` rows whose handlers need the interpreter (`Handler::Interp`).

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "Any",
    name: "collate",
    arity: 0,
    handler: Handler::Interp(collate),
    flags: RowFlags::NONE,
    named: &[],
}];

/// `Any.collate`: sort by Unicode collation order under the dynamic
/// `$*COLLATION`, which only the interpreter can read. The native cascade's
/// `collate` arm calls the same `dispatch_collate` for the receivers the
/// table does not cover (a `Supply`, a `Seq`, a `Range`).
// Cost: O(e log e) comparisons, e = elements of the invocant, plus one
// dynamic-variable lookup.
fn collate(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    args.is_empty()
        .then(|| interp.dispatch_collate(target.clone()))
}
