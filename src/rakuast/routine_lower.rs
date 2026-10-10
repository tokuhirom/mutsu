//! Named subroutine lowering.
use super::RakuAstNode;
use super::lower::*;
use crate::ast::Stmt;
use crate::value::RuntimeError;

/// Lower `sub NAME (SIG) { … }` to `Stmt::SubDecl`. Only bare positional scalar
/// parameters are handled; typed/named/slurpy/defaulted parameters and anonymous
/// subs in expression position are the current coverage boundary.
pub(super) fn lower_sub(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let name = call_name_str(node)?;
    let (params, param_defs) = signature_positional_params(node)?;
    let mut is_traits = super::routine_traits::IsTraits::default();
    let (return_type, mut custom_traits) = routine_return_type(node, Some(&mut is_traits))?;
    let multi = multiness(node)?;
    match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => {}
        Some(_) => match leaf_str(node, "scope")?.as_str() {
            "our" => custom_traits.push((super::convert::OUR_SCOPED.to_string(), None)),
            "my" => {}
            _ => return Err(unsupported(node)),
        },
    }
    // A Sub's `body` is the Blockoid directly (not a Block wrapping one).
    let mut body = lower_routine_stmts(named_child_or_positional(named_child(node, "body")?)?)?;
    super::routine_source::retain(node, &mut body)?;
    // A sub with no signature of its own takes its placeholder variables.
    let (params, param_defs) =
        crate::ast::implicit_placeholder_signature(params, param_defs, &body);
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
        name: crate::symbol::Symbol::intern(&name),
        name_expr: None,
        params,
        param_defs,
        return_type,
        // `is tighter(&infix:<+>)` also names its kind as the associativity, as
        // the parser records it.
        associativity,
        precedence_trait: is_traits.precedence.clone(),
        signature_alternates: Vec::new(),
        body,
        multi,
        is_rw: is_traits.is_rw,
        is_raw: is_traits.is_raw,
        is_export: !is_traits.export_tags.is_empty(),
        export_tags: is_traits.export_tags,
        is_test_assertion: custom_traits.iter().any(|(t, _)| t == "test-assertion"),
        supersede: false,
        custom_traits,
    })
}
