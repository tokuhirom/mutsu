//! Hyper subscripts retain their operator identity instead of becoming an
//! explicit AT-POS/AT-KEY method call. Measured against Rakudo 2026.07.
use super::lower::{lower_expr, named_child, named_child_or_positional, unsupported};
use super::view::{node_field, view_expr};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::symbol::Symbol;
use crate::value::RuntimeError;

// Cost: O(n), n = index expression size.
pub(super) fn view_node(
    names: &NameScope,
    name: &str,
    args: &[Expr],
) -> Option<Result<RakuAstNode, RuntimeError>> {
    // `@a>>++`: the hyper of a postfix operator, `MetaPostfix::Hyper(Postfix)`.
    if args.is_empty()
        && let Some(op) = name
            .strip_prefix("postfix:<")
            .and_then(|rest| rest.strip_suffix('>'))
    {
        return Some(Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::Postfix,
            fields: vec![super::view::leaf_field(
                Some("operator"),
                crate::value::Value::str(op.to_string()),
            )],
        }));
    }
    // `%h>>.<k>`: the parser kept the raw key text, which rakudo keeps as one
    // word quote under a `LiteralHashIndex` (`%h<k>` without the hyper).
    if matches!(name, "AT-KEY" | "postcircumfix:<{ }>")
        && let [Expr::Spelled(spelled)] = args
        && let crate::ast::spelled::Spelling::Words(text) = &spelled.spelling
    {
        return Some(Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::PostcircumfixLiteralHashIndex,
            fields: vec![node_field(Some("index"), super::view::word_quote(text))],
        }));
    }
    let class = match name {
        "postcircumfix:<[ ]>" => RakuAstClass::PostcircumfixArrayIndex,
        "postcircumfix:<{ }>" => RakuAstClass::PostcircumfixHashIndex,
        _ => return None,
    };
    let [index] = args else { return None };
    Some((|| {
        let statement = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementExpression,
            fields: vec![node_field(Some("expression"), view_expr(names, index)?)],
        };
        let semilist = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::SemiList,
            fields: vec![node_field(None, statement)],
        };
        Ok(RakuAstNode {
            origin: Default::default(),
            class,
            fields: vec![node_field(Some("index"), semilist)],
        })
    })())
}

// Cost: O(1).
pub(super) fn is_subscript(node: &RakuAstNode) -> bool {
    matches!(
        node.class,
        RakuAstClass::PostcircumfixArrayIndex
            | RakuAstClass::PostcircumfixHashIndex
            | RakuAstClass::PostcircumfixLiteralHashIndex
    )
}

// Cost: O(n), n = index expression size.
pub(super) fn lower(
    names: &NameScope,
    operand: Expr,
    node: &RakuAstNode,
) -> Result<Expr, RuntimeError> {
    let index = named_child(node, "index")?;
    if node.class == RakuAstClass::PostcircumfixLiteralHashIndex {
        // `%h>>.<a b>` slices each element; one word is a plain key lookup.
        let key = lower_expr(names, index)?;
        let words = super::angle_subscript::angle_key_text(&key)
            .map_or(2, |text| text.split_whitespace().count());
        return Ok(Expr::HyperMethodCall {
            target: Box::new(operand),
            name: Symbol::intern(if words == 1 {
                "AT-KEY"
            } else {
                "postcircumfix:<{ }>"
            }),
            args: vec![key],
            modifier: None,
            quoted: false,
        });
    }
    if index.fields.len() != 1 {
        return Err(unsupported(node));
    }
    let statement = named_child_or_positional(index)?;
    Ok(Expr::HyperMethodCall {
        target: Box::new(operand),
        name: Symbol::intern(if node.class == RakuAstClass::PostcircumfixArrayIndex {
            "postcircumfix:<[ ]>"
        } else {
            "postcircumfix:<{ }>"
        }),
        args: vec![lower_expr(names, statement)?],
        modifier: None,
        quoted: false,
    })
}

/// `@a>>++`: the `Postfix` inside a `MetaPostfix::Hyper`, as the call the parser
/// builds for the hyper of a postfix operator.
// Cost: O(n), n = size of the operand.
pub(super) fn lower_postfix_operator(
    operand: Expr,
    node: &RakuAstNode,
) -> Result<Expr, RuntimeError> {
    let op = super::lower::leaf_str(node, "operator")?;
    Ok(Expr::HyperMethodCall {
        target: Box::new(operand),
        name: Symbol::intern(&format!("postfix:<{op}>")),
        args: Vec::new(),
        modifier: None,
        quoted: false,
    })
}
