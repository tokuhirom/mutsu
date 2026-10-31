//! Named subroutine lowering.
use crate::scope::NameScope;
use super::RakuAstNode;
use super::lower::*;
use crate::ast::{Stmt, SubScope};
use crate::value::RuntimeError;

/// Lower `sub NAME (SIG) { … }` to `Stmt::SubDecl`. Only bare positional scalar
/// parameters are handled; typed/named/slurpy/defaulted parameters and anonymous
/// subs in expression position are the current coverage boundary.
pub(super) fn lower_sub(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let (name, name_expr) = declarator_name(names, node)?;
    let (params, param_defs) = signature_positional_params(names, node)?;
    let mut is_traits = super::routine_traits::IsTraits::default();
    let (return_type, mut custom_traits) = routine_return_type(names, node, Some(&mut is_traits))?;
    let multi = multiness(node)?;
    let scope = match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => SubScope::My,
        Some(_) => match leaf_str(node, "scope")?.as_str() {
            "our" => SubScope::Our,
            "anon" => SubScope::Anon,
            "my" => SubScope::My,
            _ => return Err(unsupported(node)),
        },
    };
    // A Sub's `body` is the Blockoid directly (not a Block wrapping one).
    let mut body = lower_routine_stmts(names, named_child_or_positional(named_child(node, "body")?)?)?;
    super::routine_source::retain(names, node, &mut body)?;
    // A sub with no signature of its own takes its placeholder variables.
    let (params, mut param_defs) =
        crate::ast::implicit_placeholder_signature(params, param_defs, &body);
    crate::ast::does_target::mark_does_targets(&name, &mut param_defs, &body);
    let associativity = is_traits
        .assoc
        .clone()
        .or_else(|| is_traits.precedence.as_ref().map(|(kind, _)| kind.clone()));
    // An operator sub that declares its precedence carries the record the
    // parser derives from the traits.
    custom_traits.extend(crate::parser::op_prec_trait(
        &name,
        multi,
        associativity.as_ref(),
        is_traits.precedence.as_ref(),
    ));
    Ok(Stmt::SubDecl {
        is_hidden_from_backtrace: is_traits.is_hidden_from_backtrace,
        spans: Default::default(),
        name: crate::symbol::Symbol::intern(&name),
        name_expr,
        params,
        param_defs,
        return_type,
        // `is tighter(&infix:<+>)` also names its kind as the associativity, as
        // the parser records it.
        associativity,
        precedence_trait: is_traits.precedence.clone(),
        signature_alternates: super::routine_source::lower_alternates(names, node)?,
        body,
        multi,
        is_rw: is_traits.is_rw,
        is_raw: is_traits.is_raw,
        is_export: !is_traits.export_tags.is_empty(),
        export_tags: is_traits.export_tags,
        is_test_assertion: custom_traits.iter().any(|(t, _)| t == "test-assertion"),
        supersede: super::routine_source::has_supersede(node),
        scope,
        custom_traits,
    })
}

/// A declarator's name and, for `::(EXPR)`, its name expression.
// Cost: O(n), n = nodes of the name expression.
pub(super) fn declarator_name(names: &NameScope, 
    node: &RakuAstNode,
) -> Result<(String, Option<crate::ast::Expr>), RuntimeError> {
    match super::symbolic_deref::lower_declarator_name(names, named_child(node, "name")?)? {
        Some((name, expr)) => Ok((name, Some(expr))),
        None => Ok((call_name_str(node)?, None)),
    }
}
