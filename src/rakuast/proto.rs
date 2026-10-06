//! `proto sub` / `proto method` <-> `RakuAST::Sub` / `RakuAST::Method` with
//! `multiness => "proto"`, in both directions.
//!
//! Measured on rakudo 2026.09: a proto is the ordinary routine node led by
//! `multiness => "proto"`. A body that is only `{*}` is `body => OnlyStar`
//! itself; a `{*}` among other statements is a `Statement::Expression` holding
//! an `OnlyStar`. The parser keeps the first as the `Whatever` term and the
//! second as the onlystar dispatch call (`Expr::onlystar_dispatch`).

use super::convert::{leaf_field, routine_node, unsupported};
use super::lower::{
    call_name_str, lower_routine_stmts, named_child, named_child_or_positional,
    routine_return_type, signature_positional_params,
};
use super::routine_traits::{IsTraits, add_flags};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::{Expr, ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// The fields of a `Stmt::ProtoDecl` the RakuAST node is built from.
pub(super) struct ProtoDecl<'a> {
    pub name: Symbol,
    pub param_defs: &'a [ParamDef],
    pub return_type: Option<&'a str>,
    pub body: &'a [Stmt],
    pub is_export: bool,
    /// Empty for the untagged `is export`, unlike `SubDecl`'s `DEFAULT`.
    pub export_tags: &'a [String],
    pub has_traits: bool,
    pub is_method: bool,
    pub is_our: bool,
}

/// Whether a proto body is the bare `{*}`, ignoring line markers.
// Cost: O(n), n = statements in the body.
fn is_onlystar_body(body: &[Stmt]) -> bool {
    let mut statements = body
        .iter()
        .filter(|stmt| !matches!(stmt, Stmt::SetLine(..)));
    matches!(
        (statements.next(), statements.next()),
        (Some(Stmt::Expr(Expr::Whatever)), None)
    )
}

/// The `Sub` / `Method` node for a proto declaration.
// Cost: O(n), n = size of the declaration.
pub(super) fn convert(proto: ProtoDecl<'_>) -> Result<RakuAstNode, RuntimeError> {
    if proto.has_traits || proto.is_our || proto.return_type.is_some() {
        return Err(unsupported("proto with traits / scope / a return type"));
    }
    let class = if proto.is_method {
        RakuAstClass::Method
    } else {
        RakuAstClass::Sub
    };
    let onlystar = is_onlystar_body(proto.body);
    let body: &[Stmt] = if onlystar { &[] } else { proto.body };
    let mut node = routine_node(class, &proto.name.resolve(), proto.param_defs, body, None)?;
    if onlystar {
        let field = node
            .fields
            .iter_mut()
            .find(|f| f.name == Some("body"))
            .ok_or_else(|| unsupported("proto without a body"))?;
        field.value = super::RakuAstFieldValue::Node(Value::rakuast(Box::new(RakuAstNode {
            class: RakuAstClass::OnlyStar,
            fields: Vec::new(),
        })));
    }
    let export_tags = match (proto.is_export, proto.export_tags) {
        (false, _) => Vec::new(),
        (true, []) => vec!["DEFAULT".to_string()],
        (true, tags) => tags.to_vec(),
    };
    let traits = IsTraits {
        export_tags,
        ..IsTraits::default()
    };
    add_flags(&mut node, false, false, &traits)?;
    node.fields
        .insert(0, leaf_field(Some("multiness"), Value::str_from("proto")));
    Ok(node)
}

/// Whether a routine node is a `proto`.
// Cost: O(f), f = fields of the node.
pub(super) fn is_proto(node: &RakuAstNode) -> bool {
    node.fields.iter().any(|f| {
        f.name == Some("multiness")
            && matches!(&f.value, super::RakuAstFieldValue::Node(v)
                if matches!(v.view(), crate::value::ValueView::Str(s) if s.as_str() == "proto"))
    })
}

/// A `proto` `Sub` / `Method` -> `Stmt::ProtoDecl`.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let (params, param_defs) = signature_positional_params(node)?;
    let mut is_traits = IsTraits::default();
    let (return_type, custom_traits) = routine_return_type(node, Some(&mut is_traits))?;
    if return_type.is_some() || !custom_traits.is_empty() || is_traits.is_rw || is_traits.is_raw {
        return Err(super::lower::unsupported(node));
    }
    let body_node = named_child(node, "body")?;
    let body = if body_node.class == RakuAstClass::OnlyStar {
        vec![Stmt::Expr(Expr::Whatever)]
    } else {
        lower_routine_stmts(named_child_or_positional(body_node)?)?
    };
    Ok(Stmt::ProtoDecl {
        name: Symbol::intern(&name),
        params,
        param_defs,
        return_type: None,
        body,
        is_export: !is_traits.export_tags.is_empty(),
        export_tags: match is_traits.export_tags.as_slice() {
            [only] if only == "DEFAULT" => Vec::new(),
            _ => is_traits.export_tags,
        },
        custom_traits: Vec::new(),
        trait_args: Vec::new(),
        is_method: node.class == RakuAstClass::Method,
        is_our: false,
    })
}
