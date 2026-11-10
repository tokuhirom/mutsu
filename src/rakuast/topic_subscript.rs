//! `.<k>` / `.[i]` / `.{k}` on the topic: raku's `Term::TopicCall` over a
//! bare subscript postfix (`Postcircumfix::LiteralHashIndex` / `HashIndex` /
//! `ArrayIndex`), where `$_<k>` is an `ApplyPostfix` on `$_`. The parser keeps
//! the two apart with [`IndexSpelling::TopicSubscript`] / [`IndexSpelling::TopicAngle`].

use super::{RakuAstClass, RakuAstField, RakuAstNode};
use crate::ast::{Expr, IndexSpelling};
use crate::scope::NameScope;
use crate::value::RuntimeError;

/// The `TopicCall` for the `ApplyPostfix` `node` that was built over `$_`: it
/// keeps the postfix only.
// Cost: O(f), f = fields of the node.
pub(super) fn wrap(node: RakuAstNode) -> RakuAstNode {
    let postfix = node
        .fields
        .into_iter()
        .find(|field| field.name == Some("postfix"));
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TermTopicCall,
        fields: postfix
            .map(|field| RakuAstField {
                name: None,
                value: field.value,
            })
            .into_iter()
            .collect(),
    }
}

/// Whether `call` is a subscript postfix.
// Cost: O(1).
pub(super) fn is_subscript(call: &RakuAstNode) -> bool {
    matches!(
        call.class,
        RakuAstClass::PostcircumfixArrayIndex
            | RakuAstClass::PostcircumfixHashIndex
            | RakuAstClass::PostcircumfixLiteralHashIndex
    )
}

/// The expression for `TopicCall(subscript)`: the subscript lowered over `$_`,
/// marked as the topic's own.
// Cost: O(n), n = nodes of the subscript.
pub(super) fn lower(names: &NameScope, call: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let node = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyPostfix,
        fields: vec![
            super::view::node_field(Some("operand"), super::view::var_lexical("$", "_")),
            RakuAstField {
                name: Some("postfix"),
                value: super::RakuAstFieldValue::Node(crate::value::Value::rakuast(Box::new(
                    call.clone(),
                ))),
            },
        ],
    };
    match super::lower::lower_expr(names, &node)? {
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
        } => Ok(Expr::Index {
            target,
            index,
            is_positional,
            spelling: match spelling {
                IndexSpelling::Angle => IndexSpelling::TopicAngle,
                _ => IndexSpelling::TopicSubscript,
            },
        }),
        other => Ok(other),
    }
}
