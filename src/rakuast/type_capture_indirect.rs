//! `::(EXPR) $x`, an indirect type capture, across the RakuAST boundary.
//!
//! Rakudo renders it as `Parameter.type-captures` holding a `Type::Capture`
//! whose `Name` is one `Name::Part::Expression(EXPR)` (no leading empty part,
//! unlike the term `::(EXPR)`). The parser keeps the whole capture as the text
//! `::(EXPR)` in the parameter's `type_constraint`, so the converter reparses
//! the expression text and lowering spells a plain string literal back.

use super::convert::{convert_expr, node_field, unsupported};
use super::lower::lower_expr;
use super::{RakuAstClass, RakuAstNode, name_parts};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The captured name `(EXPR)` of an indirect capture, as the parser spells it.
pub(super) fn is_indirect(name: &str) -> bool {
    name.starts_with('(') && name.ends_with(')')
}

/// The `Type::Capture` node of the indirect capture `(EXPR)`.
// Cost: O(n), n = size of the expression text.
pub(super) fn capture_node(name: &str) -> Result<RakuAstNode, RuntimeError> {
    let inner = &name[1..name.len() - 1];
    let expr = crate::parser::parse_adverb_argument(inner)
        .ok_or_else(|| unsupported("indirect type capture the converter cannot reparse"))?;
    let part = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::NamePartExpression,
        fields: vec![node_field(None, convert_expr(scope, &expr)?)],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TypeCapture,
        fields: vec![node_field(
            None,
            name_parts::name_from_parts(vec![Value::rakuast(Box::new(part))]),
        )],
    })
}

/// The parser's captured name `(EXPR)` for the `Name` of a `Type::Capture`
/// that is one expression part, when the expression is a plain string.
// Cost: O(n), n = size of the expression.
pub(super) fn lowered_name(name: &RakuAstNode) -> Option<String> {
    let field = name.fields.first()?;
    let super::RakuAstFieldValue::List(parts) = &field.value else {
        return None;
    };
    let [part] = parts.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(part) = part.view() else {
        return None;
    };
    if part.class != RakuAstClass::NamePartExpression {
        return None;
    }
    let super::RakuAstFieldValue::Node(expr) = &part.fields.first()?.value else {
        return None;
    };
    let ValueView::RakuAst(expr) = expr.view() else {
        return None;
    };
    let Ok(Expr::Literal(value)) = lower_expr(expr) else {
        return None;
    };
    let text = value.as_str()?;
    if text.contains(['\'', '\\']) {
        return None;
    }
    Some(format!("('{text}')"))
}
