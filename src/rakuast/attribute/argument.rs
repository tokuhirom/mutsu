//! Written delimiters of attribute trait arguments.

use super::super::convert::{convert_expr, node_field, statement_expression};
use super::super::lower::{lower_expr, named_child, named_child_or_positional};
use super::super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, spelled::Spelling};
use crate::value::{RuntimeError, ValueView};

pub(super) fn trait_argument(argument: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match argument {
        Expr::Spelled(spelled) if matches!(spelled.spelling, Spelling::BracketTraitArgument) => {
            convert_expr(&spelled.expr)
        }
        Expr::Grouped(inner) if matches!(**inner, Expr::ArrayLiteral(_)) => paren_argument(inner),
        other => paren_argument(other),
    }
}

/// `(EXPR)` as a trait argument.
// Cost: O(e), e = size of the expression.
pub(in crate::rakuast) fn paren_argument(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let semilist = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(convert_expr(expr)?))],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::CircumfixParentheses,
        fields: vec![node_field(None, semilist)],
    })
}

/// The expression inside a `(EXPR)` trait argument.
// Cost: O(e), e = size of the expression.
pub(in crate::rakuast) fn lower_paren_argument(
    owner: &RakuAstNode,
    argument: &RakuAstNode,
) -> Result<Expr, RuntimeError> {
    let refuse = || super::super::lower::unsupported(owner);
    if argument.class != RakuAstClass::CircumfixParentheses {
        return Err(refuse());
    }
    let semilist = named_child_or_positional(argument)?;
    let [statement] = semilist.fields.as_slice() else {
        return Err(refuse());
    };
    let RakuAstFieldValue::Node(statement) = &statement.value else {
        return Err(refuse());
    };
    let ValueView::RakuAst(statement) = statement.view() else {
        return Err(refuse());
    };
    if statement.class != RakuAstClass::StatementExpression || statement.fields.len() != 1 {
        return Err(refuse());
    }
    lower_expr(named_child(statement, "expression")?)
}
