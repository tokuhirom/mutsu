//! Named, user-declared and flip-flop infixes across the RakuAST boundary.
//!
//! An infix the parser cannot fold into a `TokenKind` operator -- a word
//! (`minmax`, `ff`, `precedes`), a symbol a unit declares (`infix:<⊕>`), or a
//! built-in symbol the unit overloads (`infix:<==>`) -- is an
//! [`Expr::InfixFunc`] that calls the routine of that name at run time.
//! Measured against rakudo 2026.09 it is an ordinary application of an `Infix`:
//!
//! ```text
//! $a foo $b            ApplyInfix(left, Infix("foo"), right)
//! $a minmax $b         ApplyListInfix(Infix("minmax"), operands)   (list-assoc)
//! $a | $b | $c         ApplyListInfix(Infix("|"), operands)        (flat chain)
//! ```
//!
//! The lowering cannot see the parser's scope, so it re-derives "the unit
//! overloads this operator" from the declarations the unit holds (an
//! `infix:<…>` sub or `&infix:<…>` variable, see `declared_routines`).

use super::convert::{
    convert_expr, leaf_field, node_field, plain_infix, unsupported as unsupported_expr,
};
use super::lower::{
    list_field, lower_expr, named_child, positional_leaf, rakuast_node_of, unsupported,
};
use super::meta_infix::is_list_assoc;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// Whether `name` is a real infix: the parser also uses `InfixFunc` as a
/// vehicle for the class-trait `is` and for its own `__*` helpers.
// Cost: O(1).
fn is_operator_name(name: &str) -> bool {
    !name.is_empty() && name != "is" && name != "\u{a7}" && !name.starts_with("__")
}

/// Whether `name` is one of the flip-flop operators, which rakudo gives a
/// `FlipFlop` node of their own.
// Cost: O(1).
fn is_flip_flop(name: &str) -> bool {
    matches!(
        name,
        "ff" | "^ff" | "ff^" | "^ff^" | "fff" | "^fff" | "fff^" | "^fff^"
    )
}

/// Whether `expr` is the trailing colonpair adverb `attach_trailing_adverbs`
/// appends to an operator's operands.
fn is_adverb(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Binary { left, op, .. }
            if *op == crate::token_kind::TokenKind::FatArrow
                && matches!(&**left, Expr::Literal(v) if matches!(v.view(), ValueView::Str(_)))
    )
}

/// An [`Expr::InfixFunc`] as its `ApplyInfix` / `ApplyListInfix`.
// Cost: O(n), n = nodes of the operands.
pub(super) fn convert(
    name: &str,
    left: &Expr,
    right: &[Expr],
    modifier: &Option<String>,
    whole: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    if modifier.is_some() || !is_operator_name(name) || right.is_empty() {
        return Err(unsupported_expr(&format!("{whole:?}")));
    }
    // A named argument on the operator has no node of its own here.
    if right.iter().skip(1).any(is_adverb) {
        return Err(unsupported_expr(&format!("{whole:?}")));
    }
    let infix = if is_flip_flop(name) {
        RakuAstNode {
            class: RakuAstClass::FlipFlop,
            fields: vec![leaf_field(None, Value::str(name.to_string()))],
        }
    } else {
        plain_infix(name)
    };
    if right.len() == 1 && !is_list_assoc(name) {
        return Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), convert_expr(left)?),
                node_field(Some("infix"), infix),
                node_field(Some("right"), convert_expr(&right[0])?),
            ],
        });
    }
    // List-associative: the left spine of the same operator is one operand
    // list, and a multi-operand node (`a op b op c`) already is.
    let mut operands: Vec<&Expr> = Vec::new();
    let mut current = left;
    let mut spine: Vec<&Expr> = right.iter().collect();
    while let Expr::InfixFunc {
        name: inner,
        left: inner_left,
        right: inner_right,
        modifier: None,
    } = current
        && inner == name
        && !inner_right.iter().any(is_adverb)
    {
        for r in inner_right.iter().rev() {
            spine.insert(0, r);
        }
        current = inner_left;
    }
    operands.push(current);
    operands.extend(spine);
    let mut nodes = Vec::with_capacity(operands.len());
    for operand in operands {
        nodes.push(Value::rakuast(Box::new(convert_expr(operand)?)));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), infix),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(nodes),
            },
        ],
    })
}

/// The spelling of an `Infix` node.
fn infix_name(infix: &RakuAstNode) -> Option<String> {
    if !matches!(infix.class, RakuAstClass::Infix | RakuAstClass::FlipFlop) {
        return None;
    }
    let leaf = positional_leaf(infix).ok()?;
    match leaf.view() {
        ValueView::Str(s) => Some(s.to_string()),
        _ => None,
    }
}

/// Whether the unit declares the operator `name` (an `infix:<name>` routine).
// Cost: O(1).
fn is_declared(name: &str) -> bool {
    super::declared_routines::is_declared(&format!("infix:<{name}>"))
}

/// An infix application the parser makes an [`Expr::InfixFunc`] of, or `None`
/// when `node` lowers as a plain `Expr::Binary` (or is not an infix at all).
// Cost: O(n), n = nodes of the operands.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    match node.class {
        RakuAstClass::ApplyInfix | RakuAstClass::ApplyListInfix => {}
        _ => return None,
    }
    let infix = named_child(node, "infix").ok()?;
    let name = infix_name(infix)?;
    if name == "," {
        return None;
    }
    let known = crate::compiler::helpers_ops::op_name_to_token_kind(&name).is_some();
    // A built-in operator stays an `Expr::Binary` unless the unit overloads it.
    if known && !is_declared(&name) {
        return None;
    }
    Some(fold(node, name))
}

fn fold(node: &RakuAstNode, name: String) -> Result<Expr, RuntimeError> {
    let call = |left: Expr, right: Vec<Expr>| Expr::InfixFunc {
        name: name.clone(),
        left: Box::new(left),
        right,
        modifier: None,
    };
    if node.class == RakuAstClass::ApplyInfix {
        let left = lower_expr(named_child(node, "left")?)?;
        let right = lower_expr(named_child(node, "right")?)?;
        return Ok(call(left, vec![right]));
    }
    let mut operands = Vec::new();
    for v in list_field(node, "operands")? {
        let Some(child) = rakuast_node_of(v) else {
            return Err(unsupported(node));
        };
        operands.push(lower_expr(child)?);
    }
    let mut items = operands.into_iter();
    let first = items.next().ok_or_else(|| unsupported(node))?;
    let rest: Vec<Expr> = items.collect();
    if rest.is_empty() {
        return Err(unsupported(node));
    }
    // The junction operators the parser nests to the left (the compiler
    // flattens them); any other list-associative operator holds its operands
    // in one node.
    if matches!(name.as_str(), "|" | "&" | "^") {
        return Ok(rest.into_iter().fold(first, |acc, r| call(acc, vec![r])));
    }
    Ok(call(first, rest))
}
