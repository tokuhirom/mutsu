//! `:5($n)`: a radix conversion written with parentheses, as rakudo's RakuAST
//! frontend keeps it (#14880).
//!
//! The parser runs `:5($n)` as the call `UNBASE(5, $n)`, marked with
//! `Spelling::RadixNumber`. RakuAST keeps a `Term::RadixNumber` with the radix,
//! `multi-part => False` and the parenthesized value
//! (`Circumfix::Parentheses(SemiList(Statement::Expression($n)))`).
//! [`lower`] maps the term back to the parser's marked call.

use super::lower::{lower_expr, named_child, unsupported};
use super::view::{leaf_field, node_field, parenthesized_expression, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::ast::spelled::Spelling;
use crate::scope::NameScope;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The `Term::RadixNumber` of a marked `UNBASE(radix, value)` call; any other
/// marked shape renders as the call it wraps.
// Cost: O(n), n = size of the value.
pub(super) fn view_node(names: &NameScope, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    if let Expr::Call { name, args, .. } = expr
        && name.resolve() == "UNBASE"
        && let [Expr::Literal(radix), value] = args.as_slice()
        && let ValueView::Int(radix) = radix.view()
    {
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TermRadixNumber,
            fields: vec![
                leaf_field(Some("radix"), Value::int(radix)),
                leaf_field(Some("multi-part"), Value::FALSE),
                node_field(Some("value"), parenthesized_expression(names, value)?),
            ],
        });
    }
    view_expr(names, expr)
}

/// `Term::RadixNumber(radix, value)` back to the parser's marked
/// `UNBASE(radix, value)`. A value that is not one parenthesized expression is
/// refused.
// Cost: O(n), n = size of the value.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let radix = node
        .fields
        .iter()
        .find(|f| f.name == Some("radix"))
        .and_then(|f| match &f.value {
            super::RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Int(radix) if (2..=36).contains(&radix) => Some(radix),
                _ => None,
            },
            _ => None,
        })
        .ok_or_else(|| unsupported(node))?;
    let value = named_child(node, "value")?;
    if value.class != RakuAstClass::CircumfixParentheses {
        return Err(unsupported(node));
    }
    let value = match lower_expr(names, value)? {
        Expr::Grouped(inner) => *inner,
        _ => return Err(unsupported(node)),
    };
    Ok(Expr::spelled(
        Expr::Call {
            name: Symbol::intern("UNBASE"),
            args: vec![Expr::Literal(Value::int(radix)), value],
            listop: false,
        },
        || Spelling::RadixNumber,
    ))
}
