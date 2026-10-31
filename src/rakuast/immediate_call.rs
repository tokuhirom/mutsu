//! `sub foo($n) { … }(21)` ↔ `ApplyPostfix(operand => Sub, postfix =>
//! Call::Term)` (measured on rakudo 2026.09).
//!
//! The parser keeps the written form (`ParsedStmt::ImmediateCall`); the
//! converter renders it from the declaration and the call arguments, and the
//! lowering hands a measured node back to the same expansion function
//! (`ast::immediate_call::expand`), so the routine is declared in the
//! enclosing scope as the parsed program declares it.

use crate::scope::NameScope;
use super::convert::{arg_list, convert_stmt, expression_of, node_field};
use super::lower::{arg_exprs, lower_stmt, named_child, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::value::RuntimeError;

/// The `ApplyPostfix` node of an immediate call: the routine `sub_decl` and the
/// arguments of the call.
// Cost: O(n), n = size of the routine and its arguments.
pub(super) fn convert(names: &NameScope, sub_decl: &Stmt, args: &[Expr]) -> Result<RakuAstNode, RuntimeError> {
    let statement = convert_stmt(names, sub_decl)?
        .ok_or_else(|| super::convert::unsupported("immediately called sub"))?;
    let routine = expression_of(&statement)
        .ok_or_else(|| super::convert::unsupported("immediately called sub"))?;
    // raku omits `args` for an argument-less call, as for `$f()`.
    let mut call = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallTerm,
        fields: Vec::new(),
    };
    if !args.is_empty() {
        call.fields.push(node_field(Some("args"), arg_list(names, args)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), routine),
            node_field(Some("postfix"), call),
        ],
    })
}

/// Whether `node` is an `ApplyPostfix` calling a named `Sub` declared in
/// place.
// Cost: O(f), f = fields of the node and its operand.
pub(super) fn is_immediate_call(node: &RakuAstNode) -> bool {
    node.class == RakuAstClass::ApplyPostfix
        && named_child(node, "postfix").is_ok_and(|p| p.class == RakuAstClass::CallTerm)
        && named_child(node, "operand").is_ok_and(|o| {
            o.class == RakuAstClass::Sub && o.fields.iter().any(|f| f.name == Some("name"))
        })
}

/// The parser's expansion of an [`is_immediate_call`] node.
// Cost: O(n), n = size of the routine and its arguments.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let sub_decl = lower_stmt(names, named_child(node, "operand")?)?;
    if !matches!(sub_decl, Stmt::SubDecl { .. }) {
        return Err(unsupported(node));
    }
    let args = arg_exprs(names, named_child(node, "postfix")?)?;
    Ok(crate::ast::immediate_call::expand(sub_decl, args))
}
