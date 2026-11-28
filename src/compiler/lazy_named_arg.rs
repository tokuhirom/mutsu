//! The `:$var` arguments of a `CallMethodMut` site that travel lazily (#19384).
//!
//! An eager `MakeNamedArg` promotes the caller's variable to a shared cell and
//! registers its type constraint, in case the callee binds the parameter
//! `is rw` -- per argument and per call, whoever the callee is. A lazy one
//! keeps the `WrapVarRef`-tagged value in the Pair, and the call op makes that
//! decision once it knows the callee (`vm_lazy_named_args`).

use super::Compiler;
use crate::ast::Expr;
use crate::token_kind::TokenKind;

impl Compiler {
    /// The address of the `$var` value of `arg` when `arg` is a `:$var` (or
    /// `:key($var)`) argument naming a plain lexical scalar, else 0. The
    /// fat-arrow compile matches it against the value it is compiling
    /// (`Compiler::lazy_named_arg_value`).
    // Cost: O(1).
    pub(super) fn lazy_named_arg_value_addr(arg: &Expr) -> usize {
        let Expr::Binary {
            op: TokenKind::FatArrow,
            left,
            right,
            ..
        } = Self::peel_assign_parens(arg)
        else {
            return 0;
        };
        match (left.as_ref(), right.as_ref()) {
            (Expr::Literal(_), Expr::Var(name)) if Self::is_plain_lexical_name(name) => {
                right.as_ref() as *const Expr as usize
            }
            _ => 0,
        }
    }
}
