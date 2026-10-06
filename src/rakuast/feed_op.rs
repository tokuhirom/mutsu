//! The feed operators `==>` and `<==` across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, a feed chain is one flat
//! `ApplyListInfix(Feed("==>"), operands)` in the order the operands are
//! written, whichever way the data flows: `1 ==> f() ==> g()` is
//! `(1, f(), g())` and `f() <== g() <== 1` is `(f(), g(), 1)`. (`==>>` and
//! `<<==` are not implemented by rakudo, so they stay refused.)
//!
//! The parser keeps a feed as a deferred [`Expr::Feed`] node, left-nested for
//! `==>` (the source is the inner feed) and right-nested for `<==` (the
//! source is the inner feed too, but written last).

use super::convert::{convert_expr, node_field, unsupported as unsupported_expr};
use super::lower::{list_field, lower_expr, positional_leaf, rakuast_node_of, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// The operands of the feed chain `source -> sink` in written order.
// Cost: O(n), n = operands of the chain.
fn written_operands<'a>(source: &'a Expr, sink: &'a Expr, to_right: bool) -> Vec<&'a Expr> {
    let mut operands = Vec::new();
    // The data flows source -> sink; a leftwards chain is written sink first.
    let mut current = source;
    let mut tail = vec![sink];
    loop {
        match current {
            Expr::Feed {
                source: inner_source,
                sink: inner_sink,
                append: false,
                left_is_source,
            } if *left_is_source == to_right => {
                tail.push(inner_sink);
                current = inner_source;
            }
            _ => {
                operands.push(current);
                break;
            }
        }
    }
    // `tail` holds the sinks from the outermost inwards.
    if to_right {
        operands.extend(tail.into_iter().rev());
    } else {
        // `f() <== g() <== 1`: the outermost sink is written first.
        let mut written: Vec<&Expr> = tail;
        written.push(operands[0]);
        return written;
    }
    operands
}

/// An [`Expr::Feed`] as its `ApplyListInfix` over a `Feed`.
// Cost: O(n), n = nodes of the operands.
pub(super) fn convert(
    source: &Expr,
    sink: &Expr,
    append: bool,
    left_is_source: bool,
) -> Result<RakuAstNode, RuntimeError> {
    if append {
        return Err(unsupported_expr("append feed operator"));
    }
    let mut nodes = Vec::new();
    for operand in written_operands(source, sink, left_is_source) {
        nodes.push(Value::rakuast(Box::new(convert_expr(operand)?)));
    }
    let feed = RakuAstNode {
        class: RakuAstClass::Feed,
        fields: vec![super::convert::leaf_field(
            None,
            Value::str(if left_is_source { "==>" } else { "<==" }.to_string()),
        )],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), feed),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(nodes),
            },
        ],
    })
}

/// A feed chain node as the parser's [`Expr::Feed`] nesting, or `None` when
/// `node` is not one.
// Cost: O(n), n = nodes of the operands.
pub(super) fn lower(node: &RakuAstNode) -> Option<Result<Expr, RuntimeError>> {
    if node.class != RakuAstClass::ApplyListInfix {
        return None;
    }
    let infix = super::lower::named_child(node, "infix").ok()?;
    if infix.class != RakuAstClass::Feed {
        return None;
    }
    Some(fold(node, infix))
}

fn fold(node: &RakuAstNode, infix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let leaf = positional_leaf(infix)?;
    let ValueView::Str(op) = leaf.view() else {
        return Err(unsupported(node));
    };
    let to_right = match op.to_string().as_str() {
        "==>" => true,
        "<==" => false,
        _ => return Err(unsupported(node)),
    };
    let mut operands = Vec::new();
    for v in list_field(node, "operands")? {
        let Some(child) = rakuast_node_of(v) else {
            return Err(unsupported(node));
        };
        operands.push(lower_expr(child)?);
    }
    if operands.len() < 2 {
        return Err(unsupported(node));
    }
    let feed = |source: Expr, sink: Expr| Expr::Feed {
        source: Box::new(source),
        sink: Box::new(sink),
        append: false,
        left_is_source: to_right,
    };
    if to_right {
        // `a ==> b ==> c` is `(a ==> b) ==> c`.
        let mut items = operands.into_iter();
        let first = items.next().ok_or_else(|| unsupported(node))?;
        Ok(items.fold(first, feed))
    } else {
        // `a <== b <== c` flows from `c`, through `b`, into `a`.
        let mut items = operands.into_iter().rev();
        let first = items.next().ok_or_else(|| unsupported(node))?;
        Ok(items.fold(first, feed))
    }
}
