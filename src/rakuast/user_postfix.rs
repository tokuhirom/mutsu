//! A user postfix operator applied as an operator (`5!`, or `$x++` with a user
//! `multi postfix:<++>` in scope).
//!
//! The parser calls the operator routine (`postfix:<!>(5)`) and spells the call
//! [`crate::ast::spelled::Spelling::UserPostfixOperator`]; rakudo keeps the
//! operator application, `ApplyPostfix(operand, Postfix(operator => "!"))`.
//! Red's `red-compile` reads `.number++` in a callback that way while Red's own
//! `postfix:<++>` candidates are imported (Red t/091). The `Postfix` node
//! remembers that its routine is a user one, so the lowering calls it again
//! instead of using the core increment.

use super::view::{leaf_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// The origin mark of a `Postfix` node whose operator is a user routine.
const USER_POSTFIX_MARK: &str = "user-postfix";

/// The `ApplyPostfix` a `Spelling::UserPostfixOperator` call stands for, or
/// the call itself when it is not a one-operand `postfix:<op>` call.
// Cost: O(n), n = size of the operand.
pub(super) fn view_spelled(names: &NameScope, call: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Call { name, args, .. } = call else {
        return view_expr(names, call);
    };
    let (Some(symbol), [operand]) = (operator_symbol(name.as_str()), args.as_slice()) else {
        return view_expr(names, call);
    };
    let mut postfix = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Postfix,
        fields: vec![leaf_field(Some("operator"), Value::str(symbol.to_string()))],
    };
    postfix.origin.3 = Some(USER_POSTFIX_MARK.into());
    super::postfix_grouping::view_node(names, operand, postfix)
}

/// The operator symbol of a `postfix:<op>` routine name.
// Cost: O(k), k = name length.
fn operator_symbol(name: &str) -> Option<&str> {
    name.strip_prefix("postfix:<")?
        .strip_suffix('>')
        .filter(|symbol| !symbol.is_empty())
}

/// The call a `Postfix` node lowers to over `operand` when its operator is a
/// user routine: one the view marked, or a symbol that is no core postfix
/// (a hand-built `Postfix(operator => "!")`). `Err` hands the operand back
/// for a core postfix.
// Cost: O(k), k = operator length.
pub(super) fn lower(postfix: &RakuAstNode, symbol: &str, operand: Expr) -> Result<Expr, Expr> {
    // `++` / `--` are the core postfixes a `Postfix` node names (`i` and the
    // superscript powers are lowered before this).
    let user =
        postfix.origin.3.as_deref() == Some(USER_POSTFIX_MARK) || !matches!(symbol, "++" | "--");
    if !user {
        return Err(operand);
    }
    Ok(Expr::Call {
        name: Symbol::intern(&format!("postfix:<{symbol}>")),
        args: vec![operand],
        listop: false,
    })
}
