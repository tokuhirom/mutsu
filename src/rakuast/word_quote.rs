//! A `quotewords` string that interpolates or quotes a word (`«a $b "c d"»`,
//! `<<a $b>>`, `qqww/a $b/`, `Qww/"a b"/`).
//!
//! Measured against rakudo 2026.09, it is a `QuotedString` with
//! `processors => <quotewords val>` whose segments are the text as written:
//! every unquoted run (words and the whitespace between them) is one string
//! segment, interpolated only when the quote permits it. Every quoted word is
//! a `QuoteWordsAtom` over its own `QuotedString`. The parser keeps the raw text
//! (`Spelling::InterpolatingWords`), so the segments are re-derived here; the
//! lowering splits them back into words and hands them to `word_list::lower_quotewords`.

use super::convert::{convert_expr, interp_segment, node_field};
use super::lower::{
    list_field, lower_expr, named_child_or_positional, positional_leaf, unsupported,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::parser::QuoteWordsPart;
use crate::value::{RuntimeError, Value, ValueView};

/// The node for the interpolating `quotewords` quote written as `text`.
// Cost: O(n), n = length of `text`.
pub(super) fn convert(
    val: bool,
    text: &str,
    delimiter: Option<char>,
    interpolates: bool,
) -> Result<RakuAstNode, RuntimeError> {
    convert_nested(val, text, delimiter, interpolates, 0)
}

fn convert_nested(
    val: bool,
    text: &str,
    delimiter: Option<char>,
    interpolates: bool,
    depth: usize,
) -> Result<RakuAstNode, RuntimeError> {
    if depth >= 128 {
        return Err(super::convert::unsupported(
            "a word quote nested beyond 128 levels",
        ));
    }
    let parts = crate::parser::quotewords_spelled_parts(text, delimiter, interpolates)
        .ok_or_else(|| super::convert::unsupported("a word quote that does not parse"))?;
    let mut segments = Vec::new();
    for part in &parts {
        match part {
            QuoteWordsPart::Text(Expr::StringInterpolation(items)) => {
                for item in items {
                    segments.push(Value::rakuast(Box::new(interp_segment(item)?)));
                }
            }
            QuoteWordsPart::Text(expr) => {
                segments.push(Value::rakuast(Box::new(interp_segment(expr)?)));
            }
            QuoteWordsPart::Atom(expr) => {
                segments.push(Value::rakuast(Box::new(RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::QuoteWordsAtom,
                    fields: vec![node_field(None, convert_expr(expr)?)],
                })));
            }
            QuoteWordsPart::Nested(inner) => {
                segments.push(Value::rakuast(Box::new(convert_nested(
                    val,
                    inner,
                    delimiter,
                    interpolates,
                    depth + 1,
                )?)));
            }
        }
    }
    let mut processors = vec![Value::str("quotewords".to_string())];
    if val {
        processors.push(Value::str("val".to_string()));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::QuotedString,
        fields: vec![
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(processors),
            },
            RakuAstField {
                name: Some("segments"),
                value: RakuAstFieldValue::List(segments),
            },
        ],
    })
}

/// The interpolating `words` string a `WordsAtom` spells: `Some(val)` for a
/// `words` quote whose segments are not one literal run.
// Cost: O(p), p = processors of the node.
pub(super) fn interpolating_plain_words(node: &RakuAstNode, segments: &[Value]) -> Option<bool> {
    let processors = list_field(node, "processors").ok()?;
    let names: Vec<_> = processors.iter().filter_map(|p| p.as_str()).collect();
    let val = match names.as_slice() {
        ["words"] => false,
        ["words", "val"] => true,
        _ => return None,
    };
    let one_literal = matches!(segments, [s]
        if matches!(s.view(), ValueView::RakuAst(seg) if seg.class == RakuAstClass::StrLiteral));
    (!one_literal).then_some(val)
}

/// Lower the segments of a `quotewords` string back to the word list they
/// spell: runs split at whitespace, a variable or block joins the word it
/// touches, a `QuoteWordsAtom` is a word of its own.
// Cost: O(s + n), s = segments, n = their text.
pub(super) fn lower(node: &RakuAstNode, val: bool) -> Result<Expr, RuntimeError> {
    Ok(super::word_list::lower_quotewords(
        val,
        lower_parts(node, val, 0)?,
    ))
}

fn lower_parts(
    node: &RakuAstNode,
    val: bool,
    depth: usize,
) -> Result<Vec<(bool, Expr)>, RuntimeError> {
    if depth >= 128 {
        return Err(unsupported(node));
    }
    let mut words: Vec<(bool, Expr)> = Vec::new();
    let mut current: Vec<Expr> = Vec::new();
    let flush = |words: &mut Vec<(bool, Expr)>, current: &mut Vec<Expr>| {
        match current.len() {
            0 => {}
            1 => words.push((false, current.remove(0))),
            _ => words.push((false, Expr::StringInterpolation(std::mem::take(current)))),
        }
        current.clear();
    };
    for segment in list_field(node, "segments")? {
        let ValueView::RakuAst(seg) = segment.view() else {
            return Err(unsupported(node));
        };
        match seg.class {
            RakuAstClass::StrLiteral => {
                let leaf = positional_leaf(seg)?;
                let ValueView::Str(text) = leaf.view() else {
                    return Err(unsupported(seg));
                };
                let mut rest: &str = &text;
                while !rest.is_empty() {
                    let ws = rest.starts_with(char::is_whitespace);
                    let end = rest
                        .find(|c: char| c.is_whitespace() != ws)
                        .unwrap_or(rest.len());
                    let (token, next) = rest.split_at(end);
                    if ws {
                        flush(&mut words, &mut current);
                    } else {
                        current.push(Expr::Literal(Value::str(token.to_string())));
                    }
                    rest = next;
                }
            }
            RakuAstClass::QuoteWordsAtom => {
                flush(&mut words, &mut current);
                words.push((true, lower_expr(named_child_or_positional(seg)?)?));
            }
            RakuAstClass::QuotedString if is_nested_quotewords(seg, val) => {
                flush(&mut words, &mut current);
                words.extend(lower_parts(seg, val, depth + 1)?);
            }
            // An interpolated block is evaluated, not a closure value.
            RakuAstClass::Block => {
                current.push(Expr::DoStmt(Box::new(Stmt::Block(
                    super::lower::lower_block(seg)?,
                ))));
            }
            _ => current.push(lower_expr(seg)?),
        }
    }
    flush(&mut words, &mut current);
    Ok(words)
}

fn is_nested_quotewords(node: &RakuAstNode, val: bool) -> bool {
    let Ok(processors) = list_field(node, "processors") else {
        return false;
    };
    let names: Vec<_> = processors.iter().filter_map(|p| p.as_str()).collect();
    names.as_slice()
        == if val {
            &["quotewords", "val"][..]
        } else {
            &["quotewords"][..]
        }
}
