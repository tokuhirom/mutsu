//! Executable quote processing shared by parsing and RakuAST lowering.
use super::Expr;
use crate::symbol::Symbol;

pub(crate) const FORMAT: &str = "__mutsu_make_format";

// Cost: O(1), the existing string expression is moved into the call.
pub(crate) fn format(string: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern(FORMAT),
        args: vec![string],
        listop: false,
    }
}
