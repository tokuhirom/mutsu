//! Recover hyper call-assignment nodes from indexed writeback expansions.

use crate::ast::{Expr, IndexSpelling, Stmt};
use crate::value::RuntimeError;

use super::convert::{convert_expr, method_call_postfix, node_field};
use super::{RakuAstClass, RakuAstNode};

/// Recover source-level `@a[$i]>>.=method` from its indexed writeback
/// expansion. The temporary and assignment retain the original index and call.
pub(super) fn convert(body: &[Stmt]) -> Result<Option<RakuAstNode>, RuntimeError> {
    let [
        Stmt::VarDecl {
            name: temp,
            expr: index,
            ..
        },
        Stmt::Expr(Expr::IndexAssign {
            target,
            index: index_expr,
            value,
            is_positional: true,
            spelling,
        }),
    ] = body
    else {
        return Ok(None);
    };
    if !matches!(index_expr.as_ref(), Expr::Var(index_temp) if index_temp == temp)
        || *spelling != IndexSpelling::Subscript
    {
        return Ok(None);
    }
    let Expr::HyperMethodCall {
        target: call_target,
        name,
        args,
        quoted,
        ..
    } = value.as_ref()
    else {
        return Ok(None);
    };
    let Expr::Index {
        target: inner_target,
        index: inner_index,
        is_positional: true,
        spelling: inner_spelling,
    } = call_target.as_ref()
    else {
        return Ok(None);
    };
    let same_container = matches!(
        (inner_target.as_ref(), target.as_ref()),
        (Expr::ArrayVar(a), Expr::ArrayVar(b)) if a == b
    ) || matches!(
        (inner_target.as_ref(), target.as_ref()),
        (Expr::HashVar(a), Expr::HashVar(b)) if a == b
    );
    if !same_container
        || !matches!(inner_index.as_ref(), Expr::Var(name) if name == temp)
        || *inner_spelling != IndexSpelling::Subscript
    {
        return Ok(None);
    }
    let left = convert_expr(&Expr::Index {
        target: target.clone(),
        index: Box::new(index.clone()),
        is_positional: true,
        spelling: *spelling,
    })?;
    let call = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::MetaPostfixHyper,
        fields: vec![node_field(
            None,
            method_call_postfix(name.as_str(), args, Some('='), *quoted)?,
        )],
    };
    Ok(Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyDottyInfix,
        fields: vec![
            node_field(Some("left"), left),
            node_field(
                Some("infix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::DottyInfixCallAssign,
                    fields: Vec::new(),
                },
            ),
            node_field(Some("right"), call),
        ],
    }))
}
