//! The angle subscript `%h<a b>` as rakudo's `LiteralHashIndex` over one word-quote.

use super::view::{node_field, unsupported, view_expr, word_quote};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// The raw text of the keys of a `%h<a b>` subscript: the words joined by a
/// space, or `None` when the index is not a list of word literals.
// Cost: O(n), n = length of the keys.
pub(super) fn angle_key_text(index: &Expr) -> Option<String> {
    // `%h<<$k>>` / `%h«a b»`: the parser kept the raw text of the quote.
    if let Some(text) = spelled_angle_text(index) {
        return Some(text.to_string());
    }
    let word = |expr: &Expr| match expr {
        Expr::Literal(v) => Some(v.to_string_value()),
        _ => None,
    };
    match index {
        Expr::ArrayLiteral(items) => {
            let words = items.iter().map(word).collect::<Option<Vec<_>>>()?;
            Some(words.join(" "))
        }
        other => word(other),
    }
}

/// The raw text of an interpolating angle subscript (`%h<<$k>>`), when the
/// parser kept it.
fn spelled_angle_text(index: &Expr) -> Option<&str> {
    use crate::ast::parsed::{ParsedExpr, WordSource};
    match index {
        Expr::Ast(node) => match node.as_ref() {
            ParsedExpr::WordList(list) => match &list.source {
                WordSource::Plain {
                    quotewords: true,
                    text,
                }
                | WordSource::Interpolating { text, .. } => Some(text),
                WordSource::Plain { .. } => None,
            },
            _ => None,
        },
        _ => None,
    }
}

/// `%h<a b>` as `ApplyPostfix(operand, Postcircumfix::LiteralHashIndex(index =>
/// QuotedString(processors => <words val>, segments => ("a b",))))`: rakudo
/// keeps the raw text of an angle subscript as one word-quote (measured on
/// 2026.09), where a `{...}` subscript is a `HashIndex` over a `SemiList`.
// Cost: O(n), n = nodes of the target plus the length of the keys.
pub(super) fn angle_subscript_node(
    names: &NameScope,
    target: &Expr,
    index: &Expr,
    assignee: Option<&Expr>,
    colonpairs: Vec<Value>,
) -> Result<RakuAstNode, RuntimeError> {
    let quote = if spelled_angle_text(index).is_some() {
        view_expr(names, index)?
    } else {
        let text = angle_key_text(index).ok_or_else(|| unsupported("angle subscript key"))?;
        word_quote(&text)
    };
    let mut index_node = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::PostcircumfixLiteralHashIndex,
        fields: vec![node_field(Some("index"), quote)],
    };
    if !colonpairs.is_empty() {
        index_node.fields.push(RakuAstField {
            name: Some("colonpairs"),
            value: RakuAstFieldValue::List(colonpairs),
        });
    }
    if let Some(value) = assignee {
        index_node
            .fields
            .push(node_field(Some("assignee"), view_expr(names, value)?));
    }
    super::postfix_grouping::view_node(names, target, index_node)
}
