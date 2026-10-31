//! RakuAST conversion of method-assignment declarations and calls.

use super::convert::{Initializer, arg_list, leaf_field, node_field, unsupported, var_declaration};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode, decl_traits};
use crate::ast::method_assign_decl::MethodAssignDecl;
use crate::ast::parsed::ParsedStmt;
use crate::ast::{Expr, Stmt};
use crate::value::{RuntimeError, Value, ValueView};

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
        form.anonymous,
    )?;
    decl_traits::insert(&mut decl, decl_traits::convert(&form.custom_traits)?);
    if let Some(dims) = &form.shape {
        super::keyed_hash::insert_dimensions(&mut decl, dims)?;
    }
    match &form.chain {
        // `my $o .= $m.uc`: the declaration is the operand of the chain
        // (`ApplyPostfix(operand => VarDeclaration, …)`, rakudo 2026.09).
        Some(chain) => with_innermost_operand(&super::convert::convert_expr(chain)?, decl),
        None => Ok(decl),
    }
}

/// `chain` with the operand at the bottom of its postfixes replaced by
/// `operand`.
// Cost: O(d), d = length of the chain.
fn with_innermost_operand(
    chain: &RakuAstNode,
    operand: RakuAstNode,
) -> Result<RakuAstNode, RuntimeError> {
    let unsupported_chain = || unsupported("a `.=` declaration chain");
    if chain.class != RakuAstClass::ApplyPostfix {
        return Err(unsupported_chain());
    }
    let mut fields = Vec::with_capacity(chain.fields.len());
    for field in &chain.fields {
        if field.name != Some("operand") {
            fields.push(field.clone());
            continue;
        }
        let RakuAstFieldValue::Node(value) = &field.value else {
            return Err(unsupported_chain());
        };
        let ValueView::RakuAst(child) = value.view() else {
            return Err(unsupported_chain());
        };
        let replaced = if child.class == RakuAstClass::ApplyPostfix {
            with_innermost_operand(child, operand.clone())?
        } else {
            operand.clone()
        };
        fields.push(node_field(Some("operand"), replaced));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: chain.class,
        fields,
    })
}

/// Whether `node` is a postfix chain over a `.=` declaration
/// (`my $o .= $m.uc`), the statement [`lower_chained`] takes.
// Cost: O(d), d = length of the chain.
pub(super) fn is_chained_declaration(node: &RakuAstNode) -> bool {
    let mut current = node.clone();
    while current.class == RakuAstClass::ApplyPostfix {
        let Ok(operand) = super::lower::named_child(&current, "operand") else {
            return false;
        };
        current = operand.clone();
    }
    current.class == RakuAstClass::VarDeclarationSimple
        && super::lower::named_child(&current, "initializer")
            .is_ok_and(|init| init.class == RakuAstClass::InitializerCallAssign)
}

/// The parser's statement for `my $o .= $m.uc`: the declaration's source
/// form, now carrying the chain, applied over the declared variable.
// Cost: O(n), n = size of the declaration and the chain.
pub(super) fn lower_chained(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let mut declaration = node.clone();
    while declaration.class == RakuAstClass::ApplyPostfix {
        declaration = super::lower::named_child(&declaration, "operand")?.clone();
    }
    let Stmt::Ast(kept) = super::lower::lower_var_decl_kept(&declaration)? else {
        return Err(unsupported("a `.=` declaration chain"));
    };
    let ParsedStmt::MethodAssignDecl(form) = kept.as_ref() else {
        return Err(unsupported("a `.=` declaration chain"));
    };
    if form.name.starts_with(['@', '%', '&']) {
        return Err(unsupported("a `.=` declaration chain"));
    }
    let variable = super::convert::var_lexical("$", &form.name);
    let chain = super::lower::lower_expr(&with_innermost_operand(node, variable)?)?;
    let mut form = form.clone();
    form.chain = Some(Box::new(chain));
    super::lower_parsed_stmt(ParsedStmt::MethodAssignDecl(form))
}

/// `.method` / `.method(args)` -> `Call::Method(name => Name, [args => ArgList])`.
pub(super) fn call_method(
    name: &str,
    args: &[Expr],
    modifier: Option<char>,
) -> Result<RakuAstNode, RuntimeError> {
    let name_node = super::convert::name_from_identifier(name);
    // Field order matches raku: name, args, dispatch.
    let mut fields = vec![node_field(Some("name"), name_node)];
    if !args.is_empty() {
        fields.push(node_field(Some("args"), arg_list(args)?));
    }
    // `self!priv(...)`: raku has a class of its own for the private call, and
    // no `dispatch` string.
    if modifier == Some('!') {
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::CallPrivateMethod,
            fields,
        });
    }
    if let Some(m) = modifier {
        // `.?` / `.+` / `.*` become a `dispatch` string.
        fields.push(leaf_field(Some("dispatch"), Value::str(format!(".{m}"))));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallMethod,
        fields,
    })
}
