//! `Num(EXPR)` / `Hash[Int](...)`: calling a type name.
//!
//! Raku has one node for it, a call on the type term:
//! `ApplyPostfix(Type::Simple, Call::Term(args))` -- the same node `Type.(EXPR)`
//! is (measured on 2026.09). mutsu runs the two differently: a `Type(EXPR)` is
//! the coercion call (`Expr::Call` named after the type) and `Type.(EXPR)` a
//! call on the type object (`Expr::CallOn`), and they differ for a role with a
//! `CALL-ME` and for `Type.()`. The coercion spelling therefore leaves a hidden
//! marker on its `Call::Term`, which the lowering reads back; a hand-built
//! `Call::Term` has none and stays a call on the type object.

use super::lower::arg_exprs;
use super::view::{arg_list, node_field};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, bareword};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// The hidden marker field's name.
const FIELD: &str = "type-call";

/// Whether `field` is the hidden marker, which no renderer shows.
// Cost: O(1).
pub(super) fn is_marker(field: &RakuAstField) -> bool {
    field.name == Some(FIELD)
}

/// `Num(EXPR)` -> the call on the type term; `None` when `name` is not a type
/// name. `args` is absent for `Int()`.
// Cost: O(k + n), k = length of `name`, n = size of the arguments.
pub(super) fn view_node(
    names: &NameScope,
    name: &str,
    args: &[Expr],
) -> Result<Option<RakuAstNode>, RuntimeError> {
    if !name.starts_with(char::is_uppercase) {
        return Ok(None);
    }
    let Some(base) = bareword::view_node(names, name) else {
        return Ok(None);
    };
    if base.class != RakuAstClass::TypeSimple {
        return Ok(None);
    }
    // `Int()` / `Int(Str)`: a call whose only argument is a type name is the
    // coercion type (measured on 2026.09), not a call of the type term. A sub
    // of the same name does not change the tree (the type wins `Type()`).
    {
        let written: Vec<&Expr> = args
            .iter()
            .filter(|a| !super::view::is_injected_named_arg(a))
            .collect();
        match written.as_slice() {
            [] => return coercion_node(names, name, None).map(Some),
            [Expr::BareWord(constraint)]
                if bareword::view_node(names, constraint)
                    .is_some_and(|node| node.class == RakuAstClass::TypeSimple) =>
            {
                return coercion_node(names, name, Some(constraint)).map(Some);
            }
            _ => {}
        }
    }
    let mut call_term = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::CallTerm,
        fields: Vec::new(),
    };
    if !args.is_empty() {
        call_term
            .fields
            .push(node_field(Some("args"), arg_list(names, args)?));
    }
    call_term.fields.push(RakuAstField {
        name: Some(FIELD),
        value: RakuAstFieldValue::Node(Value::TRUE),
    });
    Ok(Some(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            node_field(Some("operand"), base),
            node_field(Some("postfix"), call_term),
        ],
    }))
}

/// The coercion call `Type(EXPR)` a marked `Call::Term` on `operand` spells;
/// `None` for an unmarked one or an operand that is not a type name.
// Cost: O(n), n = size of the arguments.
pub(super) fn lower(
    names: &NameScope,
    operand: &Expr,
    postfix: &RakuAstNode,
) -> Result<Option<Expr>, RuntimeError> {
    if !postfix.fields.iter().any(is_marker) {
        return Ok(None);
    }
    let Expr::BareWord(type_name) = operand else {
        return Ok(None);
    };
    Ok(Some(Expr::Call {
        name: crate::symbol::Symbol::intern(type_name),
        args: arg_exprs(names, postfix)?,
        listop: false,
    }))
}

/// `Type::Coercion(base-type, constraint?)` over the type `base` and its
/// optional constraint type.
// Cost: O(k), k = length of the names.
pub(super) fn coercion_node(
    names: &NameScope,
    base: &str,
    constraint: Option<&str>,
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![node_field(
        Some("base-type"),
        bareword::view_node(names, base).unwrap_or_else(|| bareword::simple_type_node(base)),
    )];
    if let Some(constraint) = constraint {
        fields.push(node_field(
            Some("constraint"),
            bareword::view_node(names, constraint)
                .unwrap_or_else(|| bareword::simple_type_node(constraint)),
        ));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TypeCoercion,
        fields,
    })
}

/// The expression a `Type::Coercion` in term position lowers to: the coercion
/// call `Type(CONSTRAINT)` the parser builds, or the package `Type(Any)` when
/// a same-named sub is declared too (the type wins `Type()` there).
// Cost: O(n), n = size of the spelling.
pub(super) fn lower_coercion(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let spelling = super::lower::simple_type_name(names, node, node)?;
    let Some(open) = spelling.find('(') else {
        return Err(super::lower::unsupported(node));
    };
    let base = &spelling[..open];
    let inner = &spelling[open + 1..spelling.len() - 1];
    if inner.is_empty() && super::declared_routines::is_declared(base) {
        return Ok(Expr::Literal(Value::package(
            crate::symbol::Symbol::intern(&format!("{base}(Any)")),
        )));
    }
    Ok(Expr::Call {
        name: crate::symbol::Symbol::intern(base),
        args: if inner.is_empty() {
            Vec::new()
        } else {
            vec![Expr::BareWord(inner.to_string())]
        },
        listop: false,
    })
}
