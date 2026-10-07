//! The atomic operators in RakuAST, measured on rakudo 2026.09.
//!
//! `⚛$x` is `ApplyPrefix(Prefix "⚛", Var::Lexical)`, `$x ⚛= 5` an
//! `ApplyInfix(Infix "⚛=")`, `$x⚛++` an `ApplyPostfix(Postfix operator =>
//! "⚛++")`, `++⚛$x` a prefix `"++⚛"` and `$x ⚛+= 2` an infix `"⚛+="`: plain
//! operator nodes, the atomicity being in the operator's name. The parser's
//! spelling of each is `ast::atomic_op`'s.

use super::convert::{convert_expr, leaf_field, node_field, plain_infix, postfix_operand};
use super::lower::{leaf_str, lower_expr, named_child, positional_leaf};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::atomic_op::{self, Atomic, Category};
use crate::ast::{Expr, temporize};
use crate::value::{RuntimeError, Value, ValueView};

fn prefix_op(operator: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Prefix,
        fields: vec![leaf_field(None, Value::str(operator.to_string()))],
    }
}

fn postfix_op(operator: &str) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::Postfix,
        fields: vec![leaf_field(
            Some("operator"),
            Value::str(operator.to_string()),
        )],
    }
}

/// The node of the atomic operator the call `name(args)` is, or `None` when it
/// is not one.
// Cost: O(n), n = size of the operands.
pub(super) fn convert(name: &str, args: &[Expr]) -> Option<Result<RakuAstNode, RuntimeError>> {
    let atomic = atomic_op::recognize(name, args)?;
    Some((|| match atomic {
        Atomic::Fetch(var) => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(Some("prefix"), prefix_op("⚛")),
                node_field(
                    Some("operand"),
                    convert_expr(&temporize::variable_expr(var))?,
                ),
            ],
        }),
        Atomic::Store(var, value) => Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), convert_expr(&temporize::variable_expr(var))?),
                node_field(Some("infix"), plain_infix("⚛=")),
                node_field(Some("right"), convert_expr(value)?),
            ],
        }),
        Atomic::Unary {
            category: Category::Prefix,
            operator,
            operand,
        } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPrefix,
            fields: vec![
                node_field(Some("prefix"), prefix_op(operator)),
                node_field(Some("operand"), convert_expr(operand)?),
            ],
        }),
        Atomic::Unary {
            category: _,
            operator,
            operand,
        } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyPostfix,
            fields: vec![
                node_field(Some("operand"), postfix_operand(operand)?),
                node_field(Some("postfix"), postfix_op(operator)),
            ],
        }),
        Atomic::Binary {
            operator,
            left,
            right,
        } => Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), convert_expr(left)?),
                node_field(Some("infix"), plain_infix(operator)),
                node_field(Some("right"), convert_expr(right)?),
            ],
        }),
    })())
}

/// The operator string of a `Prefix` / `Postfix` / `Infix` node, when it is an
/// atomic one.
fn atomic_operator(node: &RakuAstNode) -> Option<String> {
    let value = match node.class {
        RakuAstClass::Postfix => leaf_str(node, "operator").ok()?,
        RakuAstClass::Prefix | RakuAstClass::Infix => match positional_leaf(node).ok()?.view() {
            ValueView::Str(s) => s.to_string(),
            _ => return None,
        },
        _ => return None,
    };
    value.contains('⚛').then_some(value)
}

/// The call the parser builds for the atomic operator `node` is, or `None`
/// when `node` is not one.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    let (category, operator) = match node.class {
        RakuAstClass::ApplyPrefix => (
            Category::Prefix,
            atomic_operator(named_child(node, "prefix").ok()?)?,
        ),
        RakuAstClass::ApplyPostfix => (
            Category::Postfix,
            atomic_operator(named_child(node, "postfix").ok()?)?,
        ),
        RakuAstClass::ApplyInfix => (
            Category::Infix,
            atomic_operator(named_child(node, "infix").ok()?)?,
        ),
        _ => return None,
    };
    Some((|| {
        let operand_name = if category == Category::Infix {
            "left"
        } else {
            "operand"
        };
        let first = lower_expr(named_child(node, operand_name)?)?;
        match category {
            Category::Infix => {
                let right = lower_expr(named_child(node, "right")?)?;
                // `$x ⚛= v` writes the variable by name.
                if operator == "⚛="
                    && let Some(name) = first.container_var_key()
                {
                    return Ok(atomic_op::store_var(name, right));
                }
                Ok(atomic_op::operator_call(
                    category,
                    &operator,
                    vec![first, right],
                ))
            }
            // `⚛$x` reads the variable by name; an element goes through the
            // parser's element form.
            Category::Prefix if operator == "⚛" => match &first {
                Expr::Var(name) => Ok(atomic_op::fetch_var(name.clone())),
                other => crate::parser::atomic_elem_update("atomic-fetch", other)
                    .ok_or_else(|| super::convert::unsupported("atomic fetch of this operand")),
            },
            _ => Ok(atomic_op::operator_call(category, &operator, vec![first])),
        }
    })())
}
