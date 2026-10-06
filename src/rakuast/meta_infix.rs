//! The `Z` / `X` / `R` metaoperators across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! @a Z @b       ApplyListInfix(Infix("Z"), operands => (@a, @b))
//! @a Z+ @b      ApplyListInfix(MetaInfix::Zip(Infix("+")), operands => (…))
//! @a X~ @b      ApplyListInfix(MetaInfix::Cross(Infix("~")), operands => (…))
//! @a R- @b      ApplyInfix(left, MetaInfix::Reverse(Infix("-")), right)
//! @a R, @b      ApplyListInfix(MetaInfix::Reverse(Infix(",")), operands => (…))
//! ```
//!
//! `Z` and `X` are list-associative, so a chain of the same operator is one
//! flat operand list; `R` is a list application only over a list-associative
//! base operator (`,`, `min`, `(|)`, ...). mutsu keeps every one of them as a
//! left-nested [`Expr::MetaOp`], so the converter flattens the chain and the
//! lowering folds the operands back.

use super::convert::{convert_expr, node_field, plain_infix, unsupported as unsupported_expr};
use super::lower::{
    list_field, lower_expr, named_child, positional_leaf, rakuast_node_of, unsupported,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// Whether the base operator `op` is list-associative, which makes a reversed
/// application of it an `ApplyListInfix` (measured: `,` `min` `max` `minmax`
/// `andthen` `orelse` `notandthen` `|` `&` `^` `^^` `xor` `...` and the set
/// operators; `and`, `or`, `//`, `~` and the like are ordinary infixes).
// Cost: O(1).
pub(super) fn is_list_assoc(op: &str) -> bool {
    matches!(
        op,
        "," | "min"
            | "max"
            | "minmax"
            | "andthen"
            | "orelse"
            | "notandthen"
            | "|"
            | "&"
            | "^"
            | "^^"
            | "xor"
            | "..."
            | "...^"
            | "\u{2026}"
            | "\u{2026}^"
            | "(|)"
            | "(&)"
            | "(-)"
            | "(^)"
            | "(+)"
            | "(.)"
            | "\u{222a}"
            | "\u{2229}"
            | "\u{2216}"
            | "\u{2296}"
            | "\u{228e}"
            | "\u{228d}"
    )
}

fn meta_class(meta: &str) -> Option<RakuAstClass> {
    match meta {
        "Z" => Some(RakuAstClass::MetaInfixZip),
        "X" => Some(RakuAstClass::MetaInfixCross),
        "R" => Some(RakuAstClass::MetaInfixReverse),
        _ => None,
    }
}

/// The metaoperator spelling (`Z`, `X`, `R`) of a `MetaInfix::*` class.
fn meta_of(class: RakuAstClass) -> Option<&'static str> {
    match class {
        RakuAstClass::MetaInfixZip => Some("Z"),
        RakuAstClass::MetaInfixCross => Some("X"),
        RakuAstClass::MetaInfixReverse => Some("R"),
        _ => None,
    }
}

/// The operands `expr` chains: the left spine of same-operator metaoperator
/// applications, then the right operand of each.
// Cost: O(n), n = operands of the chain.
fn chain_operands<'a>(expr: &'a Expr, meta: &str, op: &str) -> Vec<&'a Expr> {
    let mut operands = Vec::new();
    let mut current = expr;
    loop {
        match current {
            Expr::MetaOp {
                meta: m,
                op: o,
                left,
                right,
            } if m == meta && o == op => {
                operands.push(&**right);
                current = left;
            }
            _ => {
                operands.push(current);
                break;
            }
        }
    }
    operands.reverse();
    operands
}

/// The base operator of a meta-assignment `op` (`+` of `X+=`): the parser spells
/// `@a X+= @b` as a [`Expr::MetaOp`] whose `op` keeps the trailing `=`. An
/// operator that itself ends in `=` (`==`, `<=`, `>=`, `!=`, `===`, `=:=`, `=~=`)
/// is not one.
// Cost: O(1).
fn assign_base(op: &str) -> Option<&str> {
    const COMPARISONS: [&str; 7] = ["==", "<=", ">=", "!=", "===", "=:=", "=~="];
    if COMPARISONS.contains(&op) {
        return None;
    }
    op.strip_suffix('=').filter(|base| !base.is_empty())
}

/// `@a X+= @b` -> `ApplyInfix(left, MetaInfix::Assign(MetaInfix::Cross(Infix("+"))), right)`.
// Cost: O(n), n = nodes of the operands.
fn convert_assign(
    meta: &str,
    base: &str,
    left: &Expr,
    right: &Expr,
    whole: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let class = match meta {
        "X" | "Z" => meta_class(meta).ok_or_else(|| unsupported_expr(&format!("{whole:?}")))?,
        _ => return Err(unsupported_expr(&format!("{whole:?}"))),
    };
    let inner = RakuAstNode {
        class,
        fields: vec![node_field(None, plain_infix(base))],
    };
    let assign = RakuAstNode {
        class: RakuAstClass::MetaInfixAssign,
        fields: vec![node_field(None, inner)],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), convert_expr(left)?),
            node_field(Some("infix"), assign),
            node_field(Some("right"), convert_expr(right)?),
        ],
    })
}

/// An [`Expr::MetaOp`] as its `ApplyListInfix` / `ApplyInfix`.
// Cost: O(n), n = nodes of the operands.
pub(super) fn convert(
    meta: &str,
    op: &str,
    left: &Expr,
    right: &Expr,
    whole: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let Some(class) = meta_class(meta) else {
        return Err(unsupported_expr(&format!("{whole:?}")));
    };
    if let Some(base) = assign_base(op) {
        return convert_assign(meta, base, left, right, whole);
    }
    let infix = if op.is_empty() {
        // The bare `Z` / `X` operator.
        if meta == "R" {
            return Err(unsupported_expr(&format!("{whole:?}")));
        }
        plain_infix(meta)
    } else {
        RakuAstNode {
            class,
            fields: vec![node_field(None, plain_infix(op))],
        }
    };
    let list = meta != "R" || is_list_assoc(op);
    if !list {
        return Ok(RakuAstNode {
            class: RakuAstClass::ApplyInfix,
            fields: vec![
                node_field(Some("left"), convert_expr(left)?),
                node_field(Some("infix"), infix),
                node_field(Some("right"), convert_expr(right)?),
            ],
        });
    }
    let mut operands = chain_operands(left, meta, op);
    operands.push(right);
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

/// The `(meta, op)` an `infix` node of a metaoperator application spells, or
/// `None` for an ordinary infix.
// Cost: O(1).
fn spelling(infix: &RakuAstNode) -> Result<Option<(&'static str, String)>, RuntimeError> {
    if let Some(meta) = meta_of(infix.class) {
        let base = super::lower::named_child_or_positional(infix)?;
        if base.class != RakuAstClass::Infix {
            return Err(unsupported(infix));
        }
        let leaf = positional_leaf(base)?;
        let ValueView::Str(op) = leaf.view() else {
            return Err(unsupported(infix));
        };
        return Ok(Some((meta, op.to_string())));
    }
    if infix.class == RakuAstClass::Infix {
        let leaf = positional_leaf(infix)?;
        if let ValueView::Str(op) = leaf.view() {
            match op.to_string().as_str() {
                "Z" => return Ok(Some(("Z", String::new()))),
                "X" => return Ok(Some(("X", String::new()))),
                _ => {}
            }
        }
    }
    Ok(None)
}

/// A metaoperator application node as the parser's [`Expr::MetaOp`] chain, or
/// `None` when `node` is not one.
// Cost: O(n), n = nodes of the operands.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    match node.class {
        RakuAstClass::ApplyListInfix | RakuAstClass::ApplyInfix => {}
        _ => return None,
    }
    let infix = named_child(node, "infix").ok()?;
    if infix.class == RakuAstClass::MetaInfixAssign && node.class == RakuAstClass::ApplyInfix {
        return lower_assign(node, infix);
    }
    let (meta, op) = match spelling(infix) {
        Ok(Some(found)) => found,
        Ok(None) => return None,
        Err(e) => return Some(Err(e)),
    };
    Some(fold(node, meta, op))
}

/// `ApplyInfix(left, MetaInfix::Assign(MetaInfix::Cross|Zip(Infix(OP))), right)`
/// as the parser's `MetaOp` over `OP=`, or `None` for a plain `OP=`.
// Cost: O(n), n = nodes of the operands.
fn lower_assign(node: &RakuAstNode, assign: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    let inner = super::lower::named_child_or_positional(assign).ok()?;
    let meta = meta_of(inner.class).filter(|m| *m != "R")?;
    Some((|| {
        let (_, base) = spelling(inner)?.ok_or_else(|| unsupported(node))?;
        Ok(Expr::MetaOp {
            meta: meta.to_string(),
            op: format!("{base}="),
            left: Box::new(lower_expr(named_child(node, "left")?)?),
            right: Box::new(lower_expr(named_child(node, "right")?)?),
        })
    })())
}

fn fold(node: &RakuAstNode, meta: &str, op: String) -> Result<Expr, RuntimeError> {
    let operands = if node.class == RakuAstClass::ApplyInfix {
        vec![
            lower_expr(named_child(node, "left")?)?,
            lower_expr(named_child(node, "right")?)?,
        ]
    } else {
        let mut items = Vec::new();
        for v in list_field(node, "operands")? {
            let Some(child) = rakuast_node_of(v) else {
                return Err(unsupported(node));
            };
            items.push(lower_expr(child)?);
        }
        items
    };
    let mut items = operands.into_iter();
    let (Some(first), Some(second)) = (items.next(), items.next()) else {
        return Err(unsupported(node));
    };
    let mut acc = Expr::MetaOp {
        meta: meta.to_string(),
        op: op.clone(),
        left: Box::new(first),
        right: Box::new(second),
    };
    for right in items {
        acc = Expr::MetaOp {
            meta: meta.to_string(),
            op: op.clone(),
            left: Box::new(acc),
            right: Box::new(right),
        };
    }
    // A standalone `*` operand of `X` / `Z` makes the whole a WhateverCode: the
    // parser's own decision.
    Ok(crate::parser::maybe_curry_xz_metaop(acc))
}
