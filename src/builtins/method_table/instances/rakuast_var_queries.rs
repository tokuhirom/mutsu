//! `RakuAST::Var::Lexical.desigilname`, answered by
//! [`crate::rakuast::var_queries`]; this file only registers the row.

use super::super::{Handler, MethodRow, RowFlags};
use super::rakuast_visit::native_node;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(n), n = length of the variable's spelling.
fn desigilname(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    let answer = native_node(target).and_then(|base| match base.view() {
        ValueView::RakuAst(node) => crate::rakuast::var_queries::desigilname(node),
        _ => None,
    });
    Ok(answer.unwrap_or(Value::NIL))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Var::Lexical",
    name: "desigilname",
    arity: 0,
    handler: Handler::Pure(desigilname),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];
