//! A superscript power (`2²`, `$n⁻¹`) across the RakuAST boundary.
//!
//! The parser reads `x²` as `x ** 2` and spells it
//! [`crate::ast::spelled::Spelling::SuperscriptPower`]; rakudo keeps the
//! postfix: `ApplyPostfix(operand, Postfix::Power(power => 2))`. The hyper
//! form `@a>>²`, which the parser runs as `@a >>**>> 2`, is spelled the same
//! way and is `ApplyPostfix(operand, MetaPostfix::Hyper(Postfix::Power))`.

use super::view::leaf_field;
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::token_kind::TokenKind;
use crate::value::RuntimeError;

/// The `ApplyPostfix` of the `**` the parser built for a superscript power.
// Cost: O(n), n = size of the operand.
pub(super) fn view_power(names: &NameScope, power: &Expr) -> Result<RakuAstNode, RuntimeError> {
    if let Expr::HyperOp { op, left, right, .. } = power
        && op == "**"
        && let Expr::Literal(exponent) = &**right
    {
        let hyper = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::MetaPostfixHyper,
            fields: vec![super::view::node_field(None, power_node(exponent))],
        };
        return super::postfix_grouping::view_node(names, left, hyper);
    }
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
    super::postfix_grouping::view_node(names, left, power_node(exponent))
}

/// `Postfix::Power(exponent)`.
// Cost: O(1).
fn power_node(exponent: &crate::value::Value) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::PostfixPower,
        fields: vec![leaf_field(None, exponent.clone())],
    }
}

/// The hyper `**` a `MetaPostfix::Hyper(Postfix::Power)` node lowers to.
// Cost: O(1).
pub(super) fn lower_hyper_power(power: &RakuAstNode, operand: Expr) -> Result<Expr, RuntimeError> {
    let Expr::Binary { left, right, .. } = lower_power(power, operand)? else {
        return Err(super::lower::unsupported(power));
    };
    Ok(Expr::HyperOp {
        op: "**".to_string(),
        left,
        right,
        dwim_left: false,
        dwim_right: true,
    })
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
