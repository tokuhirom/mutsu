//! `react { … }`, `whenever SUPPLY BLOCK` and `done` across the RakuAST
//! boundary, in both directions.
//!
//! Measured on rakudo 2026.09:
//! - `react { … }` is a `StatementPrefix::React` around its `Block`;
//! - `whenever S { … }` is a `Statement::Whenever(trigger => S, body =>
//!   Block)`, and `whenever S -> $v { … }` has a `PointyBlock` body;
//! - `done` is a `Call::Name::WithoutParentheses`.
//!
//! The parser takes a `whenever`'s pointy block apart into the statement's
//! `params` / `param_defs` / `body`, the way `Expr::Lambda` and
//! `Expr::AnonSubParams` hold them, so the converter rebuilds that expression
//! for the body and the lowering takes it apart again. It records `done` and
//! `done()` as the same `Stmt::ReactDone`; that renders as the bare call,
//! which lowers to the bare word the compiler reads as the completion unless
//! a lexical `&done` shadows it.

use super::convert::{block_node, blockoid, convert_expr, name_from_identifier, node_field};
use super::lower::{lower_block, lower_expr, named_child, named_child_or_positional};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, ParamDef, RoutineDeclarator, Stmt};
use crate::value::{RuntimeError, Value};

/// `react { body }`, or `react STATEMENT` (`blorst`), whose one statement the
/// node holds directly.
// Cost: O(n), n = size of the body.
pub(super) fn convert_react(body: &[Stmt], blorst: bool) -> Result<RakuAstNode, RuntimeError> {
    let blorst = match (blorst, body) {
        (false, _) => block_node(body)?,
        (
            true,
            [
                Stmt::Whenever {
                    supply,
                    params,
                    param_defs,
                    body,
                },
            ],
        ) => convert_whenever(supply, params, param_defs, body)?,
        (true, [Stmt::Expr(expr)]) => convert_expr(expr)?,
        (true, _) => return Err(super::convert::unsupported("react with a statement body")),
    };
    Ok(RakuAstNode {
        class: RakuAstClass::StatementPrefixReact,
        fields: vec![node_field(None, blorst)],
    })
}

/// The pointy-block or bare-block expression a `whenever`'s parts came from.
fn whenever_block(params: &[String], param_defs: &[ParamDef], body: &[Stmt]) -> Expr {
    match (params, param_defs) {
        ([param], []) => Expr::Lambda {
            param: param.clone(),
            body: body.to_vec(),
            is_whatever_code: false,
            param_sigilless: false,
        },
        _ => Expr::AnonSubParams {
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            return_type: None,
            body: body.to_vec(),
            is_rw: false,
            is_raw: false,
            custom_traits: Default::default(),
            is_whatever_code: false,
            declarator: RoutineDeclarator::Block,
        },
    }
}

/// `whenever SUPPLY BLOCK`.
// Cost: O(n), n = size of the statement.
pub(super) fn convert_whenever(
    supply: &Expr,
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> Result<RakuAstNode, RuntimeError> {
    // A bare block topicalizes the emitted value, which rakudo marks with
    // three flags ahead of the body.
    let block = if params.is_empty() && param_defs.is_empty() {
        let flag = |name| RakuAstField {
            name: Some(name),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        };
        RakuAstNode {
            class: RakuAstClass::Block,
            fields: vec![
                flag("implicit-topic"),
                flag("required-topic"),
                flag("may-have-signature"),
                node_field(Some("body"), blockoid(body)?),
            ],
        }
    } else {
        convert_expr(&whenever_block(params, param_defs, body))?
    };
    Ok(RakuAstNode {
        class: RakuAstClass::StatementWhenever,
        fields: vec![
            node_field(Some("trigger"), convert_expr(supply)?),
            node_field(Some("body"), block),
        ],
    })
}

/// The written body a `supply { … }` expansion records, given the
/// `Supply.on-demand` call's arguments.
// Cost: O(1).
pub(super) fn supply_record(args: &[Expr]) -> Option<&[Stmt]> {
    let [Expr::Lambda { body, .. }] = args else {
        return None;
    };
    match body.first()? {
        Stmt::SourceForm(form) => match form.as_ref() {
            crate::ast::SourceForm::SupplyBlock(body) => Some(body),
            _ => None,
        },
        _ => None,
    }
}

/// `supply { body }` -> `StatementPrefix::Supply(Block)`.
// Cost: O(n), n = size of the body.
pub(super) fn convert_supply(body: &[Stmt]) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::StatementPrefixSupply,
        fields: vec![node_field(None, block_node(body)?)],
    })
}

/// `StatementPrefix::Supply(Block)` -> the parser's expansion of the block.
// Cost: O(n), n = size of the node.
pub(super) fn lower_supply(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let block = named_child_or_positional(node)?;
    if block.class != RakuAstClass::Block {
        return Err(super::lower::unsupported(node));
    }
    Ok(crate::parser::supply_block(lower_block(block)?))
}

/// `done`, as the bare call rakudo parses it to.
pub(super) fn convert_done() -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::CallNameWithoutParentheses,
        fields: vec![node_field(Some("name"), name_from_identifier("done"))],
    }
}

/// `StatementPrefix::React` -> `Stmt::React`.
// Cost: O(n), n = size of the node.
pub(super) fn lower_react(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let blorst = named_child_or_positional(node)?;
    let (body, is_blorst) = match blorst.class {
        RakuAstClass::Block => (lower_block(blorst)?, false),
        RakuAstClass::StatementWhenever => (vec![lower_whenever(blorst)?], true),
        _ => (vec![Stmt::Expr(lower_expr(blorst)?)], true),
    };
    Ok(Stmt::React {
        body,
        blorst: is_blorst,
    })
}

/// `Statement::Whenever` -> `Stmt::Whenever`.
// Cost: O(n), n = size of the node.
pub(super) fn lower_whenever(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let supply = lower_expr(named_child(node, "trigger")?)?;
    let block = named_child(node, "body")?;
    if block.class == RakuAstClass::Block {
        return Ok(Stmt::Whenever {
            supply,
            params: Vec::new(),
            param_defs: Vec::new(),
            body: lower_block(block)?,
        });
    }
    let (params, param_defs, body) = match lower_expr(block)? {
        Expr::Lambda { param, body, .. } => (vec![param], Vec::new(), body),
        Expr::AnonSubParams {
            params,
            param_defs,
            body,
            ..
        } => (params, param_defs, body),
        _ => return Err(super::lower::unsupported(node)),
    };
    Ok(Stmt::Whenever {
        supply,
        params,
        param_defs,
        body,
    })
}
