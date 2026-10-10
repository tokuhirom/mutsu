//! Preserve the parser's runtime choices for names supplied by EXPORT hooks.

use super::convert::{convert_expr, convert_literal, leaf_field};
use super::lower::{leaf_str, lower_expr, unsupported};
use super::{RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

// Cost: O(n), n = nodes of the fallback expression.
pub(super) fn convert(expr: &Expr) -> Option<Result<RakuAstNode, RuntimeError>> {
    match expr {
        Expr::ExportTermOrCall { name, call } => Some(convert_expr(call).map(|mut node| {
            node.fields.push(leaf_field(
                Some("export-term-name"),
                Value::str(name.resolve()),
            ));
            node
        })),
        Expr::ShadowableTermKeyword { name, value } => {
            Some(convert_literal(value).map(|mut node| {
                node.fields.push(leaf_field(
                    Some("shadowable-term-name"),
                    Value::str(name.resolve()),
                ));
                node.fields
                    .push(leaf_field(Some("shadowable-term-fallback"), value.clone()));
                node
            }))
        }
        _ => None,
    }
}

// Cost: O(n), n = nodes of the fallback expression.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    if node
        .fields
        .iter()
        .any(|field| field.name == Some("export-term-name"))
    {
        return Some((|| {
            let name = Symbol::intern(&leaf_str(node, "export-term-name")?);
            let mut call = node.clone();
            call.fields
                .retain(|field| field.name != Some("export-term-name"));
            Ok(Expr::ExportTermOrCall {
                name,
                call: Box::new(lower_expr(&call)?),
            })
        })());
    }
    if node
        .fields
        .iter()
        .any(|field| field.name == Some("shadowable-term-name"))
    {
        return Some((|| {
            let name = Symbol::intern(&leaf_str(node, "shadowable-term-name")?);
            let value = node
                .fields
                .iter()
                .find_map(|field| match &field.value {
                    RakuAstFieldValue::Node(value)
                        if field.name == Some("shadowable-term-fallback") =>
                    {
                        Some(value.clone())
                    }
                    _ => None,
                })
                .ok_or_else(|| unsupported(node))?;
            Ok(Expr::ShadowableTermKeyword { name, value })
        })());
    }
    None
}
