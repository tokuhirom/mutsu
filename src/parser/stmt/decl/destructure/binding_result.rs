//! The value of a declaring signature binding.

use crate::ast::{Expr, SignatureDecl};

/// Binding a declaring signature yields that signature, independently of
/// the values it bound. Use the same materialization as signature literals.
/// The shared expansion validates the signature copy budget before calling.
// Cost: O(n), n = signature nodes.
pub(super) fn binding_result(decl: &SignatureDecl) -> Expr {
    let params: Vec<_> = decl
        .vars
        .iter()
        .map(|var| {
            let mut param = crate::ast::ParamDef::sigilless_term(&var.name);
            param.sigilless = var.sigilless;
            param.named = var.is_named;
            param.slurpy = var.is_slurpy;
            param.required = !var.is_named && !var.is_optional && !var.is_slurpy;
            param.optional_marker = var.is_optional;
            param.default = var.default.clone();
            param.type_constraint = var.per_var_type_constraint.clone();
            param.where_constraint = var.where_constraint.clone().map(Box::new);
            param.literal_value = match &var.literal_value {
                Some(Expr::Literal(value) | Expr::LiteralSrc(value, _)) => Some(value.clone()),
                _ => None,
            };
            if let Some(trait_kind) = var.param_trait {
                param.traits.push(
                    match trait_kind {
                        crate::ast::ParamTrait::Rw => "rw",
                        crate::ast::ParamTrait::Raw => "raw",
                        crate::ast::ParamTrait::Copy => "copy",
                        crate::ast::ParamTrait::Readonly => "readonly",
                    }
                    .into(),
                );
            }
            param
        })
        .collect();
    let info =
        crate::value::signature::param_defs_to_sig_info(&params, decl.type_constraint.clone());
    Expr::Literal(crate::value::signature::make_signature_value(info, None))
}
