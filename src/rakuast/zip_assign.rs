//! RakuAST conversion for zip assignment and declaration forms.

use super::convert::{convert_expr, node_field, unsupported as unsupported_expr};
use super::lower::{list_field, lower_expr, named_child, rakuast_node_of, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::value::{RuntimeError, Value};

fn assignment_node(left: RakuAstNode, right: &Expr) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(
                Some("infix"),
                RakuAstNode {
                    class: RakuAstClass::MetaInfixZip,
                    fields: vec![node_field(
                        None,
                        RakuAstNode {
                            class: RakuAstClass::Assignment,
                            fields: vec![RakuAstField {
                                name: None,
                                value: RakuAstFieldValue::Adverb("item"),
                            }],
                        },
                    )],
                },
            ),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(vec![
                    Value::rakuast(Box::new(left)),
                    Value::rakuast(Box::new(convert_expr(right)?)),
                ]),
            },
        ],
    })
}

fn zip_call(expr: &Expr) -> Option<(&Expr, &Expr)> {
    match expr {
        Expr::Call { name, args, .. }
            if name.resolve() == "__mutsu_zip_assign" && args.len() == 2 =>
        {
            Some((&args[0], &args[1]))
        }
        _ => None,
    }
}

/// Convert the parser's private zip-assignment expansion to Rakudo's node.
// Cost: O(n), n = size of the assignment operands.
pub(super) fn convert_zip_assign(stmt: &Stmt) -> Option<Result<RakuAstNode, RuntimeError>> {
    let convert = || match stmt {
        Stmt::Assign { expr, .. } => {
            let (left, right) = zip_call(expr).ok_or_else(|| unsupported_expr("zip assignment"))?;
            assignment_node(convert_expr(left)?, right)
        }
        Stmt::SyntheticBlock(stmts) => {
            let [Stmt::VarDecl { .. }, Stmt::Assign { expr, .. }] = stmts.as_slice() else {
                return Err(unsupported_expr("zip declaration assignment"));
            };
            let (_, right) =
                zip_call(expr).ok_or_else(|| unsupported_expr("zip declaration assignment"))?;
            let declaration = super::convert::convert_stmt(&stmts[0])?
                .ok_or_else(|| unsupported_expr("zip declaration"))?;
            let left = super::convert::expression_of(&declaration)
                .ok_or_else(|| unsupported_expr("zip declaration"))?;
            assignment_node(left, right)
        }
        _ => Err(unsupported_expr("zip assignment")),
    };
    match stmt {
        Stmt::Assign { expr, .. } if zip_call(expr).is_some() => Some(convert()),
        Stmt::SyntheticBlock(stmts) if matches!(stmts.as_slice(), [Stmt::VarDecl { .. }, Stmt::Assign { expr, .. }] if zip_call(expr).is_some()) => {
            Some(convert())
        }
        _ => None,
    }
}

fn is_zip_assignment(node: &RakuAstNode) -> bool {
    if node.class != RakuAstClass::ApplyListInfix {
        return false;
    }
    let Ok(infix) = named_child(node, "infix") else {
        return false;
    };
    infix.class == RakuAstClass::MetaInfixZip
        && super::lower::named_child_or_positional(infix)
            .is_ok_and(|child| child.class == RakuAstClass::Assignment)
}

/// Lower a zip assignment expression through the parser's existing marker.
// Cost: O(n), n = size of both operands.
pub(super) fn lower(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let operands = list_field(node, "operands")?;
    if operands.len() != 2 {
        return Err(unsupported(node));
    }
    let left_node = rakuast_node_of(&operands[0]).ok_or_else(|| unsupported(node))?;
    let right_node = rakuast_node_of(&operands[1]).ok_or_else(|| unsupported(node))?;
    let left = lower_expr(left_node)?;
    let right = lower_expr(right_node)?;
    let call = |lhs| Expr::Call {
        name: crate::symbol::Symbol::intern("__mutsu_zip_assign"),
        args: vec![lhs, right.clone()],
        listop: false,
    };
    match left {
        Expr::Var(name) => Ok(Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(call(Expr::Var(name))),
            is_bind: false,
        }),
        Expr::ArrayVar(name) => Ok(Expr::AssignExpr {
            name: format!("@{name}"),
            expr: Box::new(call(Expr::ArrayVar(name))),
            is_bind: false,
        }),
        Expr::DoStmt(inner) => match *inner {
            Stmt::VarDecl {
                anonymous,
                name,
                expr,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint,
            } => Ok(Expr::DoStmt(Box::new(Stmt::SyntheticBlock(vec![
                Stmt::VarDecl {
                    anonymous,
                    name: name.clone(),
                    expr,
                    type_constraint,
                    is_state,
                    is_our,
                    is_dynamic,
                    is_export,
                    export_tags,
                    custom_traits,
                    where_constraint,
                },
                Stmt::Assign {
                    name: name.clone(),
                    expr: call(Expr::Var(name)),
                    op: AssignOp::Assign,
                    target_is_sigilless: false,
                },
            ])))),
            other => Err(RuntimeError::new(format!(
                "RakuAST: unsupported zip assignment target {other:?}"
            ))),
        },
        other => Err(RuntimeError::new(format!(
            "RakuAST: unsupported zip assignment target {other:?}"
        ))),
    }
}

/// Lower declaration-form zip assignment as a scopeless declaration block.
// Cost: O(n), n = size of the declaration and RHS.
pub(super) fn lower_declaration(node: &RakuAstNode) -> Option<Result<Stmt, RuntimeError>> {
    if !is_zip_assignment(node) {
        return None;
    }
    let operands = match list_field(node, "operands") {
        Ok(operands) if operands.len() == 2 => operands,
        _ => return Some(Err(unsupported(node))),
    };
    let declaration_node = rakuast_node_of(&operands[0])?;
    if declaration_node.class != RakuAstClass::VarDeclarationSimple {
        return None;
    }
    let right_node = rakuast_node_of(&operands[1])?;
    Some((|| {
        let declaration = super::lower::lower_stmt(declaration_node)?;
        let Stmt::VarDecl { name, .. } = &declaration else {
            return Err(unsupported(node));
        };
        let name = name.clone();
        let rhs = lower_expr(right_node)?;
        Ok(Stmt::SyntheticBlock(vec![
            declaration,
            Stmt::Assign {
                name: name.clone(),
                expr: Expr::Call {
                    name: crate::symbol::Symbol::intern("__mutsu_zip_assign"),
                    args: vec![Expr::Var(name.clone()), rhs],
                    listop: false,
                },
                op: AssignOp::Assign,
                target_is_sigilless: false,
            },
        ]))
    })())
}
