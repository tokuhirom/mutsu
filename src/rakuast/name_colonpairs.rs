//! The colonpairs of a declared variable's extended name across the RakuAST
//! boundary: `my $today:foo<g h>`.
//!
//! Measured on rakudo 2026.09, the declaration's `desigilname` is
//! `Name.from-identifier("today", colonpairs => (ColonPair::Value(key => "foo",
//! value => QuotedString(<words val>, "g h")),))`, each colonpair spelled as the
//! source spelled it (`«a b»` is a `quotewords` quote, `['a','b']` an array
//! composer), and a key-less `:<home>` is the quote alone. mutsu's parser
//! keeps the canonical name (`today:foo<g h>`), which is what the variable is
//! looked up by; a `.AST` parse also records the written colonpairs
//! ([`crate::ast::var_marks::VarMarks::name_colonpairs`]), which conversion reads back. Lowering
//! turns the nodes into the canonical name again by the same rule the parser
//! applies to the source.

use super::convert::{convert_expr, node_field};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// Put the written colonpairs of the declared name onto `decl`'s
/// `desigilname`, when the parse recorded them.
// Cost: O(n), n = length of the written colonpairs.
pub(super) fn apply(
    decl: &mut RakuAstNode,
    marks: &crate::ast::var_marks::VarMarks,
) -> Result<(), RuntimeError> {
    let Some(written) = marks.name_colonpairs.as_deref() else {
        return Ok(());
    };
    let Some(field) = decl
        .fields
        .iter_mut()
        .find(|f| f.name == Some("desigilname"))
    else {
        return Ok(());
    };
    let RakuAstFieldValue::Node(name) = &field.value else {
        return Ok(());
    };
    let ValueView::RakuAst(name) = name.view() else {
        return Ok(());
    };
    let Some(identifier) = identifier(name) else {
        return Ok(());
    };
    let (_, canonical) = crate::parser::canonical_name_colonpairs(written, String::new());
    let Some(stem) = identifier
        .strip_suffix(canonical.as_str())
        .filter(|stem| !stem.is_empty())
    else {
        return Ok(());
    };
    let exprs = crate::parser::name_colonpair_exprs(written)
        .ok_or_else(|| super::convert::unsupported("a variable name's colonpairs"))?;
    let pairs = exprs
        .iter()
        .map(|e| convert_expr(scope, e).map(|n| Value::rakuast(Box::new(n))))
        .collect::<Result<Vec<_>, _>>()?;
    let mut named = super::convert::name_from_identifier(stem);
    named.fields.push(RakuAstField {
        name: Some("colonpairs"),
        value: RakuAstFieldValue::List(pairs),
    });
    *field = node_field(Some("desigilname"), named);
    Ok(())
}

/// The identifier of a `Name.from-identifier("…")` without colonpairs.
// Cost: O(1).
fn identifier(name: &RakuAstNode) -> Option<String> {
    if name.class != RakuAstClass::Name || name.fields.len() != 1 {
        return None;
    }
    match (&name.fields[0].name, &name.fields[0].value) {
        (None, RakuAstFieldValue::Node(v)) => v.as_str().map(str::to_string),
        _ => None,
    }
}

/// The canonical variable name of a `desigilname` that carries colonpairs:
/// the identifier, then each colonpair as the parser canonicalizes its source
/// (`:foo<g h>`, `:bar`, `:<home>`). `None` for a name without colonpairs.
// Cost: O(n), n = size of the colonpairs.
pub(super) fn lower(name: &RakuAstNode) -> Option<Result<String, RuntimeError>> {
    let pairs = name.fields.iter().find(|f| f.name == Some("colonpairs"))?;
    let RakuAstFieldValue::List(pairs) = &pairs.value else {
        return None;
    };
    let stem = name.fields.iter().find(|f| f.name.is_none())?;
    let RakuAstFieldValue::Node(stem) = &stem.value else {
        return None;
    };
    let stem = stem.as_str()?.to_string();
    let mut source = String::new();
    for pair in pairs {
        let ValueView::RakuAst(pair) = pair.view() else {
            return Some(Err(unsupported()));
        };
        match colonpair_source(pair) {
            Some(text) => source.push_str(&text),
            None => return Some(Err(unsupported())),
        }
    }
    let (rest, canonical) = crate::parser::canonical_name_colonpairs(&source, stem);
    if !rest.is_empty() {
        return Some(Err(unsupported()));
    }
    Some(Ok(canonical))
}

/// Source text for one colonpair node of a name, which the parser's name
/// rule canonicalizes back.
// Cost: O(n), n = size of the pair.
fn colonpair_source(pair: &RakuAstNode) -> Option<String> {
    let key = || match pair
        .fields
        .iter()
        .find(|f| f.name == Some("key") || f.name.is_none())
    {
        Some(RakuAstField {
            value: RakuAstFieldValue::Node(k),
            ..
        }) => k.as_str().map(str::to_string),
        _ => None,
    };
    match pair.class {
        RakuAstClass::ColonPairTrue => Some(format!(":{}", key()?)),
        RakuAstClass::ColonPairValue => {
            let value = pair.fields.iter().find(|f| f.name == Some("value"))?;
            let RakuAstFieldValue::Node(value) = &value.value else {
                return None;
            };
            let ValueView::RakuAst(value) = value.view() else {
                return None;
            };
            Some(format!(":{}{}", key()?, value_source(value)?))
        }
        // A key-less colonpair is the quote alone.
        _ => Some(format!(":{}", value_source(pair)?)),
    }
}

/// The bracketed source of a colonpair's value, in a spelling the parser's
/// name rule canonicalizes the way it does the original: a word quote keeps
/// its brackets, a list of strings is a `[…]` of quoted items, anything else
/// is its expression in `(…)`.
// Cost: O(n), n = size of the value.
fn value_source(value: &RakuAstNode) -> Option<String> {
    if let Some((processors, text)) = quoted_words(value) {
        if text.contains(['<', '>', '\u{00AB}', '\u{00BB}']) {
            return None;
        }
        return match processors {
            QuoteWords::Words => Some(format!("<{text}>")),
            QuoteWords::QuoteWords => Some(format!("\u{00AB}{text}\u{00BB}")),
        };
    }
    let expr = super::lower::lower_expr(circumfix_content(value).unwrap_or(value)).ok()?;
    let quoted = |s: &str| (!s.contains(['\'', '\\'])).then(|| format!("'{s}'"));
    match &expr {
        Expr::Literal(v) if v.as_str().is_some() => Some(format!("[{}]", quoted(v.as_str()?)?)),
        Expr::ArrayLiteral(items) => {
            let items = items
                .iter()
                .map(|item| match item {
                    Expr::Literal(v) => v.as_str().and_then(quoted),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()?;
            Some(format!("[{}]", items.join(", ")))
        }
        _ => Some(format!(
            "({})",
            crate::regex_tree::expression_source(&expr)?
        )),
    }
}

fn unsupported() -> RuntimeError {
    RuntimeError::new("RakuAST: a variable name whose colonpairs cannot be spelled".to_string())
}

/// A word quote's processor: `<…>` or `«…»`.
enum QuoteWords {
    Words,
    QuoteWords,
}

/// The processor and the one text segment of a `QuotedString` that splits
/// words (`<a b>`, `«a b»`), or `None` for any other node.
// Cost: O(1).
fn quoted_words(node: &RakuAstNode) -> Option<(QuoteWords, String)> {
    if node.class != RakuAstClass::QuotedString {
        return None;
    }
    let RakuAstFieldValue::List(processors) = &node
        .fields
        .iter()
        .find(|f| f.name == Some("processors"))?
        .value
    else {
        return None;
    };
    let kind = match processors.first()?.as_str()? {
        "words" => QuoteWords::Words,
        "quotewords" => QuoteWords::QuoteWords,
        _ => return None,
    };
    if !processors[1..].iter().all(|p| p.as_str() == Some("val")) {
        return None;
    }
    Some((kind, super::name_parts::quoted_text(node)?))
}

/// The one expression a `[…]` or `(…)` circumfix holds.
// Cost: O(1).
fn circumfix_content(node: &RakuAstNode) -> Option<&RakuAstNode> {
    if !matches!(
        node.class,
        RakuAstClass::CircumfixArrayComposer | RakuAstClass::CircumfixParentheses
    ) {
        return None;
    }
    let semilist = only(node)?;
    let statement = only(semilist)?;
    (statement.class == RakuAstClass::StatementExpression).then_some(())?;
    only(statement)
}

/// The node a node's one positional field holds.
// Cost: O(1).
fn only(node: &RakuAstNode) -> Option<&RakuAstNode> {
    match node.fields.as_slice() {
        [
            RakuAstField {
                value: RakuAstFieldValue::Node(v),
                ..
            },
        ] => match v.view() {
            ValueView::RakuAst(n) => Some(n),
            _ => None,
        },
        _ => None,
    }
}
