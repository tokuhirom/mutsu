//! Callable signature constraints carried through the frontend round trip.
//!
//! Rakudo renders the whitespace `&cb (Int --> Str)` spelling as a parameter
//! sub-signature, but omits the `&cb:(Int --> Str)` constraint from `.raku`.
//! Keep the latter as model metadata, just like statement origins: dropping
//! it would silently change callback checking and multi dispatch. The parser
//! already distinguishes the two with `ParamDef::sub_signature`.

use super::convert::{node_field, signature};
use super::lower::{lower_signature_parameters, named_child, simple_type_name};
use super::{RakuAstField, RakuAstNode};
use crate::ast::ParamDef;
use crate::value::RuntimeError;

const CONSTRAINT: &str = "code-signature";

// Cost: O(s), s = size of the callable signature.
pub(super) fn attach_constraint(
    node: &mut RakuAstNode,
    def: &ParamDef,
    type_setting: bool,
) -> Result<(), RuntimeError> {
    if let Some((params, returns)) = &def.code_signature
        && def.sub_signature.is_none()
    {
        node.fields.push(node_field(
            Some(CONSTRAINT),
            signature(params, type_setting, returns.as_deref())?,
        ));
    }
    Ok(())
}

/// Lower a pointy block without discarding its binding or return constraints.
// Cost: O(n), n = size of the block and signature.
pub(super) fn lower_pointy(node: &RakuAstNode) -> Result<crate::ast::Expr, RuntimeError> {
    use super::lower::{lower_block, signature_positional_params};
    use crate::ast::Expr;
    let (params, param_defs) = signature_positional_params(node)?;
    let body = lower_block(node)?;
    let return_type = match named_child(node, "signature") {
        Ok(signature)
            if signature
                .fields
                .iter()
                .any(|field| field.name == Some("returns")) =>
        {
            Some(simple_type_name(node, named_child(signature, "returns")?)?)
        }
        _ => None,
    };
    match params.len() {
        // Only a plain parameter fits `Lambda`; an optional (`$p?`),
        // slurpy (`*@a`, `|c`), trait-carrying or destructuring
        // (`-> [$a, $b]`) one keeps its `ParamDef`, as the parser does.
        1 if param_defs.first().is_some_and(|param| {
            return_type.is_none()
                && !param.name.starts_with(['@', '%', '&'])
                && !param.named
                && param.type_constraint.is_none()
                && param.type_capture.is_none()
                && param.default.is_none()
                && param.literal_value.is_none()
                && !param.optional_marker
                && param.traits.is_empty()
                && param.sub_signature.is_none()
                && param.code_signature.is_none()
                && param.outer_sub_signature.is_none()
                && param.where_constraint.is_none()
                && param.shape_constraints.is_none()
                && !param.onearg
                && !param.slurpy
                && !param.double_slurpy
        }) =>
        {
            Ok(Expr::Lambda {
                param: params.into_iter().next().unwrap(),
                body,
                is_whatever_code: false,
                param_sigilless: param_defs.first().is_some_and(|pd| pd.sigilless),
            })
        }
        _ => Ok(Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            body,
            is_rw: false,
            is_raw: false,
            custom_traits: Default::default(),
            is_whatever_code: false,
            declarator: crate::ast::RoutineDeclarator::Block,
        }),
    }
}

// Cost: O(1).
pub(super) fn is_metadata(field: &RakuAstField) -> bool {
    matches!(field.name, Some(CONSTRAINT | "multi-invocant"))
}

// Cost: O(s), s = size of the callable signature.
pub(super) fn lower_constraint(node: &RakuAstNode, def: &mut ParamDef) -> Result<(), RuntimeError> {
    let constraint = if node
        .fields
        .iter()
        .any(|field| field.name == Some(CONSTRAINT))
    {
        Some(named_child(node, CONSTRAINT)?)
    } else if def.name.starts_with('&') {
        named_child(node, "sub-signature").ok()
    } else {
        None
    };
    if let Some(signature) = constraint {
        let params = lower_signature_parameters(signature, node)?;
        let returns = if signature
            .fields
            .iter()
            .any(|field| field.name == Some("returns"))
        {
            Some(simple_type_name(node, named_child(signature, "returns")?)?)
        } else {
            None
        };
        def.code_signature = Some((params, returns));
    }
    Ok(())
}
