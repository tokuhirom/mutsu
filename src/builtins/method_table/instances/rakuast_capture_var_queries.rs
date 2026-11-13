//! `Var::PositionalCapture.sigil`: a hand-built capture variable
//! (`Var::PositionalCapture.new(0)`) answers the empty string, a parsed `$0`
//! the sigil it was written with (rakudo 2026.09), which its constructor form
//! does not show. This file registers the row, which the measured accessor
//! tables do not carry (`Var::NamedCapture`'s is generated).

use super::super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(1).
fn sigil(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let written = match target.view() {
        ValueView::RakuAst(node) => node.origin.3.as_deref().map(str::to_string),
        _ => None,
    };
    Ok(Value::str(written.unwrap_or_default()))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Var::PositionalCapture",
    name: "sigil",
    arity: 0,
    handler: Handler::Pure(sigil),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];
