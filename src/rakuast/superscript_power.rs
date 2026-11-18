//! A superscript power (`2²`, `$n⁻¹`) across the RakuAST boundary.
//!
//! The parser reads `x²` as `x ** 2` and spells it
//! [`crate::ast::spelled::Spelling::SuperscriptPower`]; rakudo keeps the
//! postfix: `ApplyPostfix(operand, Postfix::Power(power => 2))`.

use super::view::leaf_field;
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::token_kind::TokenKind;
use crate::value::RuntimeError;

/// The `ApplyPostfix` of the `**` the parser built for a superscript power.
// Cost: O(n), n = size of the operand.
pub(super) fn view_power(names: &NameScope, power: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let Expr::Binary {
        left,
        op: TokenKind::StarStar,
        right,
        ..
    } = power
    else {
        return super::view::view_expr(names, power);
    };
    let Expr::Literal(exponent) = &**right else {
        return super::view::view_expr(names, power);
    };
    let postfix = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::PostfixPower,
        fields: vec![leaf_field(None, exponent.clone())],
    };
    super::postfix_grouping::view_node(names, left, postfix)
}

/// The `**` a `Postfix::Power` node lowers to over `operand`.
// Cost: O(1).
pub(super) fn lower_power(postfix: &RakuAstNode, operand: Expr) -> Result<Expr, RuntimeError> {
    // Positional (`.new(2)`) or named (`power => 2`).
    let Some(RakuAstFieldValue::Node(power)) = postfix.fields.first().map(|f| &f.value) else {
        return Err(super::lower::unsupported(postfix));
    };
    let power = power.clone();
    Ok(Expr::Binary {
        left: Box::new(operand),
        op: TokenKind::StarStar,
        right: Box::new(Expr::Literal(power)),
        form: Default::default(),
    })
}
