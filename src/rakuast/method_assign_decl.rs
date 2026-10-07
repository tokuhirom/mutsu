//! RakuAST conversion of method-assignment declarations and calls.

use super::convert::{Initializer, arg_list, leaf_field, node_field, unsupported, var_declaration};
use super::{RakuAstClass, RakuAstNode, decl_traits};
use crate::ast::Expr;
use crate::ast::method_assign_decl::MethodAssignDecl;
use crate::value::{RuntimeError, Value};

/// A declaration with a source-level `.=` initializer.
pub(super) fn convert(form: &MethodAssignDecl) -> Result<RakuAstNode, RuntimeError> {
    if form.where_constraint.is_some()
        || form
            .custom_traits
            .iter()
            .any(|(name, arg)| !decl_traits::is_rendered(name, arg))
    {
        return Err(unsupported("method-assignment declaration with traits"));
    }
    let scope = if form.is_our {
        Some("our")
    } else if form.is_state {
        Some("state")
    } else {
        None
    };
    let dynamic_name;
    let (name, twigil) = if form.is_dynamic {
        dynamic_name = form.name.replacen('*', "", 1);
        (dynamic_name.as_str(), Some("*"))
    } else {
        (form.name.as_str(), None)
    };
    let mut decl = var_declaration(
        name,
        Some(Initializer::CallAssign(form)),
        scope,
        form.type_constraint.as_deref(),
        twigil,
        None,
    )?;
    decl_traits::insert(&mut decl, decl_traits::convert(&form.custom_traits)?);
    Ok(decl)
}

/// `.method` / `.method(args)` -> `Call::Method(name => Name, [args => ArgList])`.
pub(super) fn call_method(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = RakuAstNode {
        class: RakuAstClass::Name,
        fields: vec![leaf_field(None, Value::str(name.to_string()))],
    };
    // Field order matches raku: name, args, dispatch.
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    // `self!priv(...)`: raku has a class of its own for the private call, and
    // no `dispatch` string.
    if modifier == Some('!') {
        return Ok(RakuAstNode {
            class: RakuAstClass::CallPrivateMethod,
            fields,
        });
    }
    if let Some(m) = modifier {
        // `.?` / `.+` / `.*` become a `dispatch` string.
        fields.push(leaf_field(Some("dispatch"), Value::str(format!(".{m}"))));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::CallMethod,
        fields,
    })
}
