//! `Term::Reduce`: `[+] 1, 2` / `[Z+] @a, @b` / `[\R-] ...`.
//!
//! Measured against rakudo 2026.11:
//!
//! - the argument list holds the reduced list's elements (`[+] 1, 2` is
//!   `ArgList(IntLiteral(1), IntLiteral(2))`), not one comma `ApplyListInfix`;
//! - a metaoperator reduction carries the metaoperator node as its infix
//!   (`[Z+]` is `MetaInfix::Zip(Infix("+"))`, `[R-]` `MetaInfix::Reverse(...)`),
//!   where `[Z]` / `[X]` alone stay plain infixes.
//!
//! mutsu keeps `Expr::Reduction { op, expr }` with the metaoperator as part of
//! the operator string and the list as one expression, so the view splits them
//! and the lowering joins them back.

use super::lower::{child_node, lower_expr, named_child, positional_leaf, unsupported};
use super::view::{node_field, plain_infix, view_expr};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

/// The metaoperator class a reduction operator's first letter names, and the
/// operator it wraps (`Z+` -> Zip over `+`), or `None` for a plain infix.
// Cost: O(1).
fn meta_split(op: &str) -> Option<(RakuAstClass, &str)> {
    let class = match op.chars().next()? {
        'Z' => RakuAstClass::MetaInfixZip,
        'X' => RakuAstClass::MetaInfixCross,
        'R' => RakuAstClass::MetaInfixReverse,
        _ => return None,
    };
    let rest = &op[1..];
    (!rest.is_empty()).then_some((class, rest))
}

/// The metaoperator spelling of `class`.
// Cost: O(1).
fn meta_letter(class: RakuAstClass) -> Option<char> {
    match class {
        RakuAstClass::MetaInfixZip => Some('Z'),
        RakuAstClass::MetaInfixCross => Some('X'),
        RakuAstClass::MetaInfixReverse => Some('R'),
        _ => None,
    }
}

/// The `Term::Reduce` node of `[op] expr`.
// Cost: O(n), n = nodes of the reduced list.
pub(super) fn view(names: &NameScope, op: &str, expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    let (triangle, infix_op) = match op.strip_prefix('\\') {
        Some(stripped) => (true, stripped),
        None => (false, op),
    };
    let infix = match meta_split(infix_op) {
        Some((class, base)) => RakuAstNode {
            origin: Default::default(),
            class,
            fields: vec![node_field(None, plain_infix(base))],
        },
        None => plain_infix(infix_op),
    };
    let mut args = Vec::new();
    for item in expr.comma_items() {
        args.push(node_field(None, view_expr(names, item)?));
    }
    let arglist = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ArgList,
        fields: args,
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermReduce,
        fields: vec![
            RakuAstField {
                name: Some("triangle"),
                value: RakuAstFieldValue::Node(Value::truth(triangle)),
            },
            node_field(Some("infix"), infix),
            node_field(Some("args"), arglist),
        ],
    })
}

/// `Term::Reduce` back to `Expr::Reduction`.
// Cost: O(n), n = nodes of the node.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let infix = named_child(node, "infix")?;
    let base = |inner: &RakuAstNode| -> Result<String, RuntimeError> {
        match positional_leaf(inner)?.view() {
            ValueView::Str(s) => Ok(s.to_string()),
            _ => Err(unsupported(node)),
        }
    };
    let op = match infix.class {
        RakuAstClass::Infix => base(infix)?,
        class => {
            let letter = meta_letter(class).ok_or_else(|| unsupported(node))?;
            let inner = match infix.fields.first().map(|f| &f.value) {
                Some(RakuAstFieldValue::Node(v)) => match v.view() {
                    ValueView::RakuAst(inner) if inner.class == RakuAstClass::Infix => inner,
                    _ => return Err(unsupported(node)),
                },
                _ => return Err(unsupported(node)),
            };
            format!("{letter}{}", base(inner)?)
        }
    };
    let triangle = super::lower::bool_field(node, "triangle")?;
    let args = named_child(node, "args")?;
    let mut items = Vec::with_capacity(args.fields.len());
    for field in &args.fields {
        items.push(lower_expr(names, child_node(&field.value)?)?);
    }
    let expr = match items.len() {
        1 => items.remove(0),
        _ => Expr::ArrayLiteral(items),
    };
    Ok(Expr::Reduction {
        op: if triangle { format!("\\{op}") } else { op },
        expr: Box::new(expr),
    })
}
