//! Preserve parser parentheses that Rakudo omits from a postfix operand.

use super::convert::{node_field, postfix_operand};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

const SOURCE_GROUPING: &str = "source-grouped-operand";
const MAX_GROUPING: usize = 128;

/// Build an ApplyPostfix while retaining the parser's parentheses as hidden
/// provenance. Rakudo omits one layer from the visible operand node.
// Cost: O(p + n), p = parentheses around the operand, n = operand node size.
pub(super) fn convert(operand: &Expr, postfix: RakuAstNode) -> Result<RakuAstNode, RuntimeError> {
    let mut depth = 0;
    let mut inner = operand;
    while let Expr::Grouped(next) = inner {
        depth += 1;
        if depth > MAX_GROUPING {
            return Err(RuntimeError::new(
                "RakuAST: too many grouped postfix operands",
            ));
        }
        inner = next;
    }
    let mut fields = vec![
        node_field(Some("operand"), postfix_operand(operand)?),
        node_field(Some("postfix"), postfix),
    ];
    if depth != 0 {
        fields.push(RakuAstField {
            name: Some(SOURCE_GROUPING),
            value: RakuAstFieldValue::Node(Value::int(depth as i64)),
        });
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyPostfix,
        fields,
    })
}

/// Restore the parser's grouping, or Rakudo's visible doubled Whatever
/// grouping on a hand-built node without source provenance.
// Cost: O(p), p = parentheses around the operand (bounded by MAX_GROUPING).
pub(super) fn restore(
    node: &RakuAstNode,
    operand_node: &RakuAstNode,
    mut operand: Expr,
) -> Result<Expr, RuntimeError> {
    if let Some(field) = node.fields.iter().find(|f| f.name == Some(SOURCE_GROUPING)) {
        let RakuAstFieldValue::Node(value) = &field.value else {
            return Err(super::lower::unsupported(node));
        };
        let ValueView::Int(depth) = value.view() else {
            return Err(super::lower::unsupported(node));
        };
        if !(1..=MAX_GROUPING as i64).contains(&depth) {
            return Err(super::lower::unsupported(node));
        }
        for _ in 0..depth {
            operand = Expr::Grouped(Box::new(operand));
        }
    } else if operand_node.class == RakuAstClass::CircumfixParentheses
        && matches!(
            operand,
            Expr::Whatever | Expr::WhateverArg | Expr::HyperWhatever | Expr::WhateverCurry(_)
        )
    {
        operand = Expr::Grouped(Box::new(Expr::Grouped(Box::new(operand))));
    }
    Ok(operand)
}

// Cost: O(1).
pub(super) fn is_source_field(field: &RakuAstField) -> bool {
    field.name == Some(SOURCE_GROUPING)
}
