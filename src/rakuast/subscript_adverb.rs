//! Subscript adverbs across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, an adverb on a subscript is a colonpair
//! of the postcircumfix itself, in source order:
//!
//! ```text
//! @a[0]:!exists     Postcircumfix::ArrayIndex(index => …,
//!                       colonpairs => (ColonPair::False("exists"),))
//! %h{"x"}:delete:v  Postcircumfix::HashIndex(index => …,
//!                       colonpairs => (ColonPair::True("delete"), ColonPair::True("v")))
//! @a[0]:exists(0)   … colonpairs => (ColonPair::Value(key => "exists",
//!                       value => Circumfix::Parentheses(…)))
//! ```
//!
//! The parser expands the adverbs into the shapes the compiler executes;
//! `ast::subscript_adverb` holds that expansion. The converter reads an
//! expansion back with `ast::subscript_adverb::adverbs`, and lowering rebuilds
//! it with `ast::subscript_adverb::expand`.

use super::convert::{
    angle_key_text, angle_subscript_node, colonpair_value_expr, leaf_field, subscript_dims_node,
};
use super::lower::{list_field, lower_expr, unsupported};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::subscript_adverb::{self, Adverb};
use crate::value::{RuntimeError, Value, ValueView};

/// The postcircumfix node for `expr` when it is a subscript with adverbs;
/// `None` when it is not one.
// Cost: O(n), n = AST nodes under `expr`.
pub(super) fn convert(expr: &Expr) -> Option<Result<RakuAstNode, RuntimeError>> {
    if !matches!(
        expr,
        Expr::Exists { .. } | Expr::Call { .. } | Expr::MethodCall { .. } | Expr::Ternary { .. }
    ) {
        return None;
    }
    let (subscript, adverbs) = subscript_adverb::adverbs(expr)?;
    let (target, dims, is_positional) = match &subscript {
        Expr::Index {
            target,
            index,
            is_positional: false,
            spelling: crate::ast::IndexSpelling::Angle,
        } if angle_key_text(index).is_some() => {
            return Some(
                adverbs
                    .iter()
                    .map(colonpair)
                    .collect::<Result<Vec<_>, _>>()
                    .and_then(|colonpairs| angle_subscript_node(target, index, None, colonpairs)),
            );
        }
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => (target, std::slice::from_ref(&**index), *is_positional),
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => (target, dimensions.as_slice(), *is_positional),
        _ => return None,
    };
    Some(
        adverbs
            .iter()
            .map(colonpair)
            .collect::<Result<Vec<_>, _>>()
            .and_then(|colonpairs| {
                subscript_dims_node(target, dims, is_positional, None, colonpairs)
            }),
    )
}

/// `:key` / `:!key` / `:key(value)`.
fn colonpair((key, value): &Adverb) -> Result<Value, RuntimeError> {
    let flag = match value {
        Expr::Literal(v) => match v.view() {
            ValueView::Bool(true) => Some(RakuAstClass::ColonPairTrue),
            ValueView::Bool(false) => Some(RakuAstClass::ColonPairFalse),
            _ => None,
        },
        _ => None,
    };
    let node = match flag {
        Some(class) => RakuAstNode {
            class,
            fields: vec![leaf_field(None, Value::str(key.clone()))],
        },
        None => colonpair_value_expr(&Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(key.clone()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(value.clone()),
            form: Default::default(),
        })?,
    };
    Ok(Value::rakuast(Box::new(node)))
}

/// `subscript` with the postcircumfix's `colonpairs` applied; `subscript`
/// itself when it has none.
// Cost: O(a), a = colonpairs (plus lowering their values).
pub(super) fn lower(subscript: Expr, postfix: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let Ok(colonpairs) = list_field(postfix, "colonpairs") else {
        return Ok(subscript);
    };
    if colonpairs.is_empty() {
        return Ok(subscript);
    }
    let adverbs = colonpairs
        .iter()
        .map(|pair| {
            let ValueView::RakuAst(node) = pair.view() else {
                return Err(unsupported(postfix));
            };
            if !matches!(
                node.class,
                RakuAstClass::ColonPairTrue
                    | RakuAstClass::ColonPairFalse
                    | RakuAstClass::ColonPairValue
                    | RakuAstClass::ColonPairVariable
            ) {
                return Err(unsupported(node));
            }
            match lower_expr(node)? {
                Expr::Binary {
                    left,
                    op: crate::token_kind::TokenKind::FatArrow,
                    right,
                    ..
                } => match left.as_ref() {
                    Expr::Literal(key) => match key.view() {
                        ValueView::Str(key) => Ok((key.to_string(), *right)),
                        _ => Err(unsupported(node)),
                    },
                    _ => Err(unsupported(node)),
                },
                _ => Err(unsupported(node)),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    subscript_adverb::expand(subscript, &adverbs).ok_or_else(|| unsupported(postfix))
}

/// `RakuAST::Postcircumfix::ArrayIndex.new(index => …, colonpairs => (…),
/// assignee => …)` and its `HashIndex` / `LiteralHashIndex` siblings; `None`
/// for any other constructor.
// Cost: O(a), a = colonpairs.
pub(super) fn construct(class_name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = match class_name {
        "RakuAST::Postcircumfix::ArrayIndex" => RakuAstClass::PostcircumfixArrayIndex,
        "RakuAST::Postcircumfix::HashIndex" => RakuAstClass::PostcircumfixHashIndex,
        "RakuAST::Postcircumfix::LiteralHashIndex" => RakuAstClass::PostcircumfixLiteralHashIndex,
        _ => return None,
    };
    let constructor = format!("{class_name}.new");
    let build = || {
        let index = super::named_arg(args, "index")
            .ok_or_else(|| RuntimeError::new(format!("{constructor} requires `index`")))?;
        super::require_any_rakuast(&index, &constructor, "index")?;
        let mut fields = vec![RakuAstField {
            name: Some("index"),
            value: RakuAstFieldValue::Node(index),
        }];
        if let Some(colonpairs) = super::named_arg(args, "colonpairs") {
            let colonpairs = colonpairs
                .as_list_items()
                .map(<[Value]>::to_vec)
                .ok_or_else(|| {
                    RuntimeError::new(format!("{constructor} expects `colonpairs` to be a list"))
                })?;
            for pair in &colonpairs {
                let is_colonpair = matches!(pair.view(), ValueView::RakuAst(node) if matches!(
                    node.class,
                    RakuAstClass::ColonPairTrue
                        | RakuAstClass::ColonPairFalse
                        | RakuAstClass::ColonPairValue
                        | RakuAstClass::ColonPairVariable
                ));
                if !is_colonpair {
                    return Err(RuntimeError::new(format!(
                        "{constructor} expects `colonpairs` to hold RakuAST::ColonPair nodes"
                    )));
                }
            }
            if !colonpairs.is_empty() {
                fields.push(RakuAstField {
                    name: Some("colonpairs"),
                    value: RakuAstFieldValue::List(colonpairs),
                });
            }
        }
        if let Some(assignee) = super::named_arg(args, "assignee") {
            super::require_any_rakuast(&assignee, &constructor, "assignee")?;
            fields.push(RakuAstField {
                name: Some("assignee"),
                value: RakuAstFieldValue::Node(assignee),
            });
        }
        Ok(Value::rakuast(Box::new(RakuAstNode { class, fields })))
    };
    Some(build())
}
