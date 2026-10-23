//! The method call of a `.=` assignment, `$x .= meth(args)`.
//!
//! The parser marks the expansion (`parser::wrap_dot_assign`) as an
//! `Expr::CompoundAssign` whose `rhs` is the method call applied to the
//! target. Rakudo has one `ApplyDottyInfix` node holding only the call, so the
//! RakuAST converter reads it through [`method_call`], which says whether a
//! marker's `rhs` is the call the parser builds (the lowering goes back
//! through `parser::wrap_dot_assign`).

use super::Expr;

/// The pieces of a `.=` marker's method call that the RakuAST node keeps.
pub(crate) struct DottyCall<'a> {
    pub(crate) name: &'a str,
    pub(crate) args: &'a [Expr],
    /// The dispatch modifier (`.?`, `.!`, `.^`, ...), when there is one.
    pub(crate) modifier: Option<char>,
    /// The method name was written quoted (`.="name"()`).
    pub(crate) quoted: bool,
}

/// The method call a `.=` marker's `rhs` holds, or `None` when `rhs` is not a
/// method call.
// Cost: O(1).
pub(crate) fn method_call(rhs: &Expr) -> Option<DottyCall<'_>> {
    match rhs {
        Expr::MethodCall {
            name,
            args,
            modifier,
            quoted,
            ..
        } => Some(DottyCall {
            name: name.as_str(),
            args,
            modifier: *modifier,
            quoted: *quoted,
        }),
        _ => None,
    }
}
