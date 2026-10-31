//! Quoted string conversion and lowering, including ordered format processing.
use super::lower::{
    list_field, lower_block, lower_expr, lower_statement_sequence, named_child_or_positional,
    positional_leaf, unsupported,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

const SOURCE_CLOSED_BLOCK_HEREDOC: &str = "source-closed-block-heredoc";

/// Keep the parser's lexical-scope diagnostic while exposing the public
/// interpolated string tree.
// Cost: O(n), n = heredoc content size.
pub(super) fn convert_closed_block_heredoc(
    names: &NameScope,
    content: &str,
) -> Result<RakuAstNode, RuntimeError> {
    let mut quoted =
        super::convert::convert_expr(names, &crate::parser::interpolate_heredoc_content(content)?)?;
    if quoted.class != RakuAstClass::QuotedString {
        return Err(super::convert::unsupported("closed-block heredoc contents"));
    }
    quoted.fields.push(super::convert::leaf_field(
        Some(SOURCE_CLOSED_BLOCK_HEREDOC),
        Value::str(content.to_string()),
    ));
    Ok(quoted)
}

// Cost: O(1).
pub(super) fn is_source_field(field: &RakuAstField) -> bool {
    field.name == Some(SOURCE_CLOSED_BLOCK_HEREDOC)
}

// Cost: O(n), n = quoted-string expression size.
pub(super) fn convert_format(
    names: &NameScope,
    args: &[Expr],
) -> Result<RakuAstNode, RuntimeError> {
    let [string] = args else {
        return Err(super::convert::unsupported("format quote arguments"));
    };
    let mut quoted = super::convert::convert_expr(names, string)?;
    if quoted.class != RakuAstClass::QuotedString {
        return Err(super::convert::unsupported("format quote contents"));
    }
    if let Some(field) = quoted
        .fields
        .iter_mut()
        .find(|f| f.name == Some("processors"))
    {
        let RakuAstFieldValue::List(processors) = &mut field.value else {
            return Err(unsupported(&quoted));
        };
        processors.insert(0, Value::str_from("format"));
    } else {
        quoted.fields.insert(
            0,
            RakuAstField {
                name: Some("processors"),
                value: RakuAstFieldValue::List(vec![Value::str_from("format")]),
            },
        );
    }
    Ok(quoted)
}

/// The processors and text of a word quote (`processors => <words val>` over one
/// literal segment), lowered through the parser's own `angle_words_expr`;
/// `None` for a string without processors. Any other processor list stays
/// the boundary.
// Cost: O(p), p = processors of the node.
pub(super) fn word_quote_content(
    node: &RakuAstNode,
) -> Result<Option<(bool, bool, String)>, RuntimeError> {
    if !node.fields.iter().any(|f| f.name == Some("processors")) {
        return Ok(None);
    }
    let processors = list_field(node, "processors")?;
    let names: Vec<_> = processors.iter().filter_map(|p| p.as_str()).collect();
    if processors.is_empty() {
        return Ok(None);
    }
    let (quotewords, val) = match names.as_slice() {
        ["words", "val"] => (false, true),
        ["words"] => (false, false),
        ["quotewords", "val"] => (true, true),
        ["quotewords"] => (true, false),
        _ => return Err(unsupported(node)),
    };
    let [segment] = list_field(node, "segments")? else {
        return Err(unsupported(node));
    };
    let ValueView::RakuAst(segment) = segment.view() else {
        return Err(unsupported(node));
    };
    if segment.class != RakuAstClass::StrLiteral {
        return Err(unsupported(node));
    }
    match positional_leaf(segment)?.view() {
        ValueView::Str(text) => Ok(Some((quotewords, val, text.to_string()))),
        _ => Err(unsupported(node)),
    }
}

/// `Some(val)` for a `quotewords` string whose segments are not one literal
/// run: an interpolating word quote (see `word_quote`).
// Cost: O(p), p = processors of the node.
fn interpolating_words(
    node: &RakuAstNode,
    segments: &[Value],
) -> Result<Option<bool>, RuntimeError> {
    if !node.fields.iter().any(|f| f.name == Some("processors")) {
        return Ok(None);
    }
    let processors = list_field(node, "processors")?;
    let names: Vec<_> = processors.iter().filter_map(|p| p.as_str()).collect();
    let val = match names.as_slice() {
        ["quotewords", "val"] => true,
        ["quotewords"] => false,
        _ => return Ok(None),
    };
    let one_literal = matches!(segments, [s]
        if matches!(s.view(), ValueView::RakuAst(seg) if seg.class == RakuAstClass::StrLiteral));
    Ok((!one_literal).then_some(val))
}

// Cost: O(n), n = quoted-string node size.
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    if let Some(field) = node
        .fields
        .iter()
        .find(|field| field.name == Some(SOURCE_CLOSED_BLOCK_HEREDOC))
    {
        let RakuAstFieldValue::Node(content) = &field.value else {
            return Err(unsupported(node));
        };
        let ValueView::Str(content) = content.view() else {
            return Err(unsupported(node));
        };
        return Ok(Expr::HeredocInterpolation(content.to_string(), true));
    }
    if let Some(literal) = super::nf_quote::lower(node) {
        return literal;
    }
    if node
        .fields
        .iter()
        .any(|field| field.name == Some("processors"))
    {
        let processors = list_field(node, "processors")?;
        if processors.first().and_then(Value::as_str) == Some("format") {
            let mut contents = node.clone();
            let field = contents
                .fields
                .iter_mut()
                .find(|field| field.name == Some("processors"))
                .ok_or_else(|| unsupported(node))?;
            let RakuAstFieldValue::List(processors) = &mut field.value else {
                return Err(unsupported(node));
            };
            // The first processor was checked above; take only its own list.
            *processors = processors.iter().skip(1).cloned().collect();
            return Ok(crate::ast::quote_processing::format(lower_contents(
                names, &contents,
            )?));
        }
    }
    lower_contents(names, node)
}

// Cost: O(n), n = segments and their expression sizes.
fn lower_contents(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let segments = list_field(node, "segments")?;
    if let Some(val) = interpolating_words(node, segments)? {
        return super::word_quote::lower(names, node, val);
    }
    // `qqw/a $x/`: the interpolated string, split into words at run time.
    if node.fields.iter().any(|f| f.name == Some("processors"))
        && let Some(val) = super::word_quote::interpolating_plain_words(node, segments)
    {
        let mut plain = node.clone();
        plain.fields.retain(|f| f.name != Some("processors"));
        return Ok(super::word_list::lower_words_atom(
            lower_expr(names, &plain)?,
            val,
        ));
    }
    if let Some((quotewords, val, content)) = word_quote_content(node)? {
        // `<…>` keeps its own lowering (a lone numeric word is a
        // literal term there).
        return Ok(if !quotewords && val {
            crate::parser::angle_words_expr(&content)
        } else {
            super::word_list::lower_word_quote(quotewords, val, &content)
        });
    }
    if segments.len() == 1
        && let ValueView::RakuAst(seg) = segments[0].view()
        && seg.class == RakuAstClass::StrLiteral
    {
        return Ok(Expr::Literal(positional_leaf(seg)?));
    }
    // A multi-segment (interpolated) string -> StringInterpolation of the
    // lowered segments (`StrLiteral` runs and interpolated terms alike).
    let mut parts = Vec::with_capacity(segments.len());
    for s in segments {
        let ValueView::RakuAst(seg) = s.view() else {
            return Err(unsupported(node));
        };
        // An interpolated code block (`"a{ $x }b"`) is a `Block`
        // segment, but as a *segment* it is evaluated, not a closure
        // value. mutsu spells that `DoStmt(Block)`; lowering it through
        // the ordinary expression path would build a closure and
        // interpolate its stringification instead of its result.
        if seg.class == RakuAstClass::Block {
            parts.push(Expr::DoStmt(Box::new(Stmt::Block(lower_block(
                names, seg,
            )?))));
            continue;
        }
        // `"a$(EXPR)b"` is the parser's `DoStmt(Expr(EXPR))`.
        if seg.class == RakuAstClass::ContextualizerItem
            && let Some(contents) = super::contextualizer::lower_segment(names, seg)?
        {
            parts.push(Expr::DoStmt(Box::new(Stmt::Expr(contents))));
            continue;
        }
        // `"a$(my $x = 1; $x)b"`: the parser's scope-transparent block.
        if seg.class == RakuAstClass::ContextualizerItem
            && let Ok(sequence) = named_child_or_positional(seg)
            && sequence.class == RakuAstClass::StatementSequence
            && sequence.fields.len() > 1
        {
            parts.push(Expr::desugar_block(lower_statement_sequence(
                names, sequence,
            )?));
            continue;
        }
        parts.push(lower_expr(names, seg)?);
    }
    Ok(Expr::StringInterpolation(parts))
}
