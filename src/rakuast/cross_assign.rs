//! RakuAST conversion for the cross assignment metaoperator: `@a X= 1`.
//!
//! The parser keeps the statement as an assignment of the cross back to its
//! target (`Stmt::Assign { name: "@a", expr: MetaOp { meta: "X", op: "=",
//! left: @a, right } }`); a written `@a = (@a X= 1)` nests an `AssignExpr`
//! instead, so the shape is the operator's own. Rakudo 2026.09 builds
//! `ApplyListInfix(infix => MetaInfix::Cross(Assignment(:item)), operands =>
//! (@a, 1))`, and lowering turns that node back into the parser's form.

use super::convert::{convert_expr, node_field};
use super::lower::{list_field, lower_expr, named_child, rakuast_node_of, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::value::{RuntimeError, Value};

/// The variable a cross assignment's left operand names, as `Stmt::Assign`
/// spells it (`@a`, `x`, `%h`).
// Cost: O(k), k = length of the name.
fn target_name(left: &Expr) -> Option<String> {
    match left {
        Expr::Var(name) => Some(name.clone()),
        Expr::ArrayVar(name) => Some(format!("@{name}")),
        Expr::HashVar(name) => Some(format!("%{name}")),
        _ => None,
    }
}

/// Convert the parser's cross assignment statement, or `None` when `stmt` is
/// not one.
// Cost: O(n), n = size of the operands.
pub(super) fn convert(stmt: &Stmt) -> Option<Result<RakuAstNode, RuntimeError>> {
    let Stmt::Assign {
        name,
        expr:
            Expr::MetaOp {
                meta,
                op,
                left,
                right,
            },
        op: AssignOp::Assign,
        ..
    } = stmt
    else {
        return None;
    };
    if meta != "X" || op != "=" || target_name(left).as_ref() != Some(name) {
        return None;
    }
    Some((|| {
        let assignment = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Assignment,
            fields: vec![RakuAstField {
                name: None,
                value: RakuAstFieldValue::Adverb("item"),
            }],
        };
        Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(
                    Some("infix"),
                    RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::MetaInfixCross,
                        fields: vec![node_field(None, assignment)],
                    },
                ),
                RakuAstField {
                    name: Some("operands"),
                    value: RakuAstFieldValue::List(vec![
                        Value::rakuast(Box::new(convert_expr(left)?)),
                        Value::rakuast(Box::new(convert_expr(right)?)),
                    ]),
                },
            ],
        })
    })())
}

/// Whether `node` applies `MetaInfix::Cross` over an `Assignment`.
// Cost: O(1).
pub(super) fn is_cross_assignment(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ApplyListInfix
        && named_child(node, "infix").is_ok_and(|infix| {
            infix.class == RakuAstClass::MetaInfixCross
                && super::lower::named_child_or_positional(infix)
                    .is_ok_and(|child| child.class == RakuAstClass::Assignment)
        })
}

/// Lower a cross assignment back into the parser's assignment of the cross.
// Cost: O(n), n = size of both operands.
pub(super) fn lower(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let operands = list_field(node, "operands")?;
    if operands.len() != 2 {
        return Err(unsupported(node));
    }
    let (left, right) = (&operands[0], &operands[1]);
    let left = lower_expr(rakuast_node_of(left).ok_or_else(|| unsupported(node))?)?;
    let right = lower_expr(rakuast_node_of(right).ok_or_else(|| unsupported(node))?)?;
    let name = target_name(&left).ok_or_else(|| unsupported(node))?;
    Ok(Expr::AssignExpr {
        name,
        expr: Box::new(Expr::MetaOp {
            meta: "X".to_string(),
            op: "=".to_string(),
            left: Box::new(left),
            right: Box::new(right),
        }),
        is_bind: false,
    })
}
