//! RakuAST node tree → internal AST (`Stmt`/`Expr`) — the write direction that
//! backs `EVAL($rakuast)` (ADR-0011 Phase 5). The lowered AST is fed to the
//! existing compiler; there is no second execution engine.
//!
//! Slice 1 covers the literal cluster (and the `StatementList` /
//! `Statement::Expression` wrappers around it). Constructs outside that set
//! produce an explicit `RuntimeError` (the documented coverage boundary).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

fn unsupported(node: &RakuAstNode) -> RuntimeError {
    RuntimeError::new(format!(
        "RakuAST: EVAL does not yet support lowering `{}`",
        node.class.printed_name()
    ))
}

/// Lower a top-level RakuAST node to a statement list. A bare expression node
/// (e.g. `EVAL(RakuAST::IntLiteral.new(42))`) becomes a single expression
/// statement.
pub fn lower(node: &RakuAstNode) -> Result<Vec<Stmt>, RuntimeError> {
    match node.class {
        RakuAstClass::StatementList => {
            let mut stmts = Vec::with_capacity(node.fields.len());
            for f in &node.fields {
                stmts.push(lower_stmt(child_node(&f.value)?)?);
            }
            Ok(stmts)
        }
        _ => Ok(vec![lower_stmt(node)?]),
    }
}

fn lower_stmt(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    match node.class {
        RakuAstClass::StatementExpression => {
            let expr = named_child(node, "expression")?;
            Ok(Stmt::Expr(lower_expr(expr)?))
        }
        _ => Ok(Stmt::Expr(lower_expr(node)?)),
    }
}

fn lower_expr(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match node.class {
        RakuAstClass::IntLiteral | RakuAstClass::RatLiteral | RakuAstClass::StrLiteral => {
            Ok(Expr::Literal(positional_leaf(node)?))
        }
        // `"..."` parses to a QuotedString wrapping StrLiteral segments; a single
        // plain segment lowers to its string literal.
        RakuAstClass::QuotedString => {
            let segments = list_field(node, "segments")?;
            if segments.len() == 1
                && let ValueView::RakuAst(seg) = segments[0].view()
                && seg.class == RakuAstClass::StrLiteral
            {
                return Ok(Expr::Literal(positional_leaf(seg)?));
            }
            Err(unsupported(node))
        }
        RakuAstClass::StatementExpression => lower_expr(named_child(node, "expression")?),
        RakuAstClass::ApplyInfix => {
            let left = lower_expr(named_child(node, "left")?)?;
            let right = lower_expr(named_child(node, "right")?)?;
            let op = infix_token(named_child(node, "infix")?)?;
            Ok(Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            })
        }
        RakuAstClass::ApplyPrefix => {
            let operand = lower_expr(named_child(node, "operand")?)?;
            let op = infix_token(named_child(node, "prefix")?)?;
            Ok(Expr::Unary {
                op,
                expr: Box::new(operand),
            })
        }
        _ => Err(unsupported(node)),
    }
}

/// The `TokenKind` for an `Infix`/`Prefix` operator node (its positional operator
/// string), or an error for an operator the lowerer doesn't handle yet.
fn infix_token(node: &RakuAstNode) -> Result<crate::token_kind::TokenKind, RuntimeError> {
    let name = positional_leaf(node)?;
    let ValueView::Str(s) = name.view() else {
        return Err(unsupported(node));
    };
    crate::compiler::helpers_ops::op_name_to_token_kind(&s).ok_or_else(|| unsupported(node))
}

/// The value of a node's single positional (name-less) leaf field.
fn positional_leaf(node: &RakuAstNode) -> Result<Value, RuntimeError> {
    match node.fields.first() {
        Some(f) if f.name.is_none() => match &f.value {
            RakuAstFieldValue::Node(v) => Ok(v.clone()),
            _ => Err(unsupported(node)),
        },
        _ => Err(unsupported(node)),
    }
}

/// The child RakuAST node of a named field.
fn named_child<'a>(node: &'a RakuAstNode, name: &str) -> Result<&'a RakuAstNode, RuntimeError> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::RakuAst(child) => Some(child),
                _ => None,
            },
            _ => None,
        })
        .ok_or_else(|| unsupported(node))
}

/// The child RakuAST node wrapped in a `Node` field value.
fn child_node(fv: &RakuAstFieldValue) -> Result<&RakuAstNode, RuntimeError> {
    if let RakuAstFieldValue::Node(v) = fv
        && let ValueView::RakuAst(child) = v.view()
    {
        return Ok(child);
    }
    Err(RuntimeError::new("RakuAST: EVAL expected a child node"))
}

fn list_field<'a>(node: &'a RakuAstNode, name: &str) -> Result<&'a [Value], RuntimeError> {
    match node.fields.iter().find(|f| f.name == Some(name)) {
        Some(f) => match &f.value {
            RakuAstFieldValue::List(items) => Ok(items),
            _ => Err(unsupported(node)),
        },
        None => Err(unsupported(node)),
    }
}
