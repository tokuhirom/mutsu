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

/// The routine a shell quote (`qx/.../`, `qqx/.../`, `q:x/.../`) runs its
/// command through. A program cannot write it as a call (`qx(...)` is a quote),
/// so the call is the quote's own, and the RakuAST view shows it as a
/// `QuotedString` with the `exec` processor; a written `QX(...)` stays a call.
pub(crate) const EXEC: &str = "qx";

// Cost: O(1), the command expression is moved into the call.
pub(crate) fn exec(command: Expr) -> Expr {
    Expr::Call {
        name: Symbol::intern(EXEC),
        args: vec![command],
        listop: false,
    }
}
