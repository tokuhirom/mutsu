//! `Date.IO` and `DateTime.IO` (ADR-11276 section 9.58).
//!
//! `IO` is `Cool`'s: the invocant is stringified (`2024-01-02`, an ISO timestamp)
//! and the string becomes an `IO::Path` rooted at the current `$*CWD`. The path
//! factory is the interpreter's (`$*CWD`, `$*SPEC`), so these are interpreter rows.
//! `:CWD` and `:SPEC` are accepted and ignored, as `Cool.IO` does.

use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

const NAMED: &[&str] = &["CWD", "SPEC"];

const fn row(owner: &'static str) -> MethodRow {
    MethodRow {
        owner,
        name: "IO",
        arity: 0,
        handler: Handler::Interp(io),
        flags: RowFlags::NONE,
        named: NAMED,
    }
}

pub(super) static ROWS: &[MethodRow] = &[row("Date"), row("DateTime")];

/// `.IO`: the path named by the rendering of the date.
// Cost: O(n), n = chars of the rendering.
fn io(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let path = target.to_string_value();
    if path.contains('\0') {
        return Some(Err(RuntimeError::new(
            "X::IO::Null: Found null byte in pathname",
        )));
    }
    Some(Ok(interp.make_io_path_instance(&path)))
}
