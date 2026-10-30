//! Source positions on statement nodes (ADR-10723 §4, "Source positions").
//!
//! The internal AST keeps line information as `Stmt::SetLine` markers between
//! statements; a RakuAST node has no position of its own. Dropping them in the
//! round trip changed what a program does: every line number an error, a
//! `warn` or a backtrace reports came out wrong, and a `return` raised in an
//! `EVAL` string escaped the routine that called it.
//!
//! Rakudo keeps an `origin` on every node. The statement nodes `convert`
//! builds carry the line their statement began on as a hidden `origin` field,
//! and `lower` puts the `SetLine` back in front of the statement it lowers. A
//! hand-built node has none, so a hand-built tree lowers without markers, as
//! it always did.
//!
//! The field is part of the model (an accessor answers it) but not of the
//! node's constructor form: Rakudo's `.raku` shows no origin either.

use std::cell::Cell;

use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Stmt;
use crate::value::{RuntimeError, Value, ValueView};

thread_local! {
    /// The line of the statement being lowered, for the call-site markers the
    /// parser attaches to calls (see [`current_line`]).
    static CURRENT_LINE: Cell<Option<i64>> = const { Cell::new(None) };
}

const TRACE_FIELD: &str = "source-trace";

/// Keep a parsed statement's numbered trace hook on its RakuAST statement.
// Cost: O(n), n = source text length.
pub(super) fn trace_field(stmt: &Stmt) -> Option<RakuAstField> {
    let Stmt::Trace { id, line, source } = stmt else {
        return None;
    };
    Some(RakuAstField {
        name: Some(TRACE_FIELD),
        value: RakuAstFieldValue::List(vec![
            Value::int(i64::from(*id)),
            Value::int(*line),
            Value::str(source.clone()),
        ]),
    })
}

/// Restore a trace hook before compiling the statement that owns it.
// Cost: O(n), n = source text length.
pub(super) fn trace_of(node: &RakuAstNode) -> Result<Option<Stmt>, RuntimeError> {
    let Some(field) = node.fields.iter().find(|f| f.name == Some(TRACE_FIELD)) else {
        return Ok(None);
    };
    let RakuAstFieldValue::List(values) = &field.value else {
        return Err(super::lower::unsupported(node));
    };
    let [id, line, source] = values.as_slice() else {
        return Err(super::lower::unsupported(node));
    };
    let (ValueView::Int(id), ValueView::Int(line), ValueView::Str(source)) =
        (id.view(), line.view(), source.view())
    else {
        return Err(super::lower::unsupported(node));
    };
    let id = u32::try_from(id).map_err(|_| super::lower::unsupported(node))?;
    Ok(Some(Stmt::Trace {
        id,
        line,
        source: source.to_string(),
    }))
}

/// Give `node`, a statement that began on `line` and occupies `span` of the
/// unit's source, its origin.
// Cost: O(1) for ASCII text, O(log n) otherwise, n = characters of the unit.
pub(super) fn set(node: &mut RakuAstNode, line: i64, span: crate::ast::SrcSpan) {
    // The lowering reads only the line; the ranges are for `.AST`.
    if super::passthrough::executing() {
        node.origin.0 = Some(Value::int(line));
        return;
    }
    node.origin.0 = Some(super::unit_source::origin_value(line, span));
    super::use_origins::module_name(node, span);
    if node.class == super::RakuAstClass::StatementWhenever {
        super::expr_block_origins::whenever(node, span);
    }
    // A `Statement::Expression` with no statement modifier was written as just
    // its expression, which therefore has the statement's range (a `Sub`, a
    // `Class`, a `VarDeclaration`, a call) unless the parser placed it itself.
    if node.class == super::RakuAstClass::StatementExpression
        && let [field] = node.fields.as_mut_slice()
        && field.name == Some("expression")
        && let RakuAstFieldValue::Node(held) = &mut field.value
        && let ValueView::RakuAst(expression) = held.view()
        && expression.origin.0.is_none()
    {
        let mut expression = RakuAstNode::clone(expression);
        set_span(&mut expression, span);
        super::infix_origins::derive(&mut expression);
        *held = Value::rakuast(Box::new(expression));
    }
}

/// The line an `origin` field value records: the `line` an `Origin` carries, or
/// the plain number an `END` phaser's hidden index is.
// Cost: O(1).
fn line_in(value: &Value) -> Option<i64> {
    match value.view() {
        ValueView::Int(line) => Some(line),
        ValueView::Instance { attributes, .. } => {
            match attributes
                .as_map()
                .get(super::unit_source::LINE_ATTR)?
                .view()
            {
                ValueView::Int(line) => Some(line),
                _ => None,
            }
        }
        _ => None,
    }
}

/// The `RakuAST::Origin` a node carries, if it has one.
// Cost: O(1).
pub(super) fn origin_of(node: &RakuAstNode) -> Option<Value> {
    node.origin.0.clone()
}

/// The hidden field a main-compunit `END` phaser carries: the source-order
/// number the parser gave it. The main program installs every `END` up front
/// by that number, so a lowered `END` without it would never be installed.
const END_INDEX: &str = "end-index";

/// The `end-index` field of the `END` phaser numbered `index`.
// Cost: O(1).
pub(super) fn end_index_field(index: u32) -> RakuAstField {
    RakuAstField {
        name: Some(END_INDEX),
        value: RakuAstFieldValue::Node(Value::int(i64::from(index))),
    }
}

/// The source-order number `node` (an `END` phaser) carries, if any.
// Cost: O(f), f = fields of `node`.
pub(super) fn end_index_of(node: &RakuAstNode) -> Option<u32> {
    let field = node.fields.iter().find(|f| f.name == Some(END_INDEX))?;
    let RakuAstFieldValue::Node(value) = &field.value else {
        return None;
    };
    match value.view() {
        ValueView::Int(index) => u32::try_from(index).ok(),
        _ => None,
    }
}

/// Whether `field` is the hidden origin or `END` number, which no renderer shows.
// Cost: O(1).
pub(super) fn is_origin(field: &RakuAstField) -> bool {
    field.name == Some(END_INDEX) || field.name == Some(TRACE_FIELD)
}

/// The line `node`'s statement began on, when the node carries one.
// Cost: O(f), f = fields of `node`.
pub(super) fn line_of(node: &RakuAstNode) -> Option<i64> {
    if let Some(origin) = &node.origin.0 {
        return line_in(origin);
    }
    let field = node.fields.iter().find(|f| f.name == Some(END_INDEX))?;
    let RakuAstFieldValue::Node(value) = &field.value else {
        return None;
    };
    line_in(value)
}

/// Run `f` with `line` as the line of the statement being lowered. A node
/// without an origin keeps the enclosing statement's line, so an expression
/// block inside a statement is still attributed to it.
// Cost: O(1) plus `f`.
pub(super) fn with_line<T>(line: Option<i64>, f: impl FnOnce() -> T) -> T {
    let previous = CURRENT_LINE.with(|c| c.get());
    CURRENT_LINE.with(|c| c.set(line.or(previous)));
    let result = f();
    CURRENT_LINE.with(|c| c.set(previous));
    result
}

/// The line of the statement being lowered, `None` for a hand-built tree: the
/// parser stamps the line of a call onto the call as a call-site marker, and a
/// tree that never had a source line must not invent one.
// Cost: O(1).
pub(super) fn current_line() -> Option<i64> {
    CURRENT_LINE.with(|c| c.get())
}

/// Give `node`, below the statement level, the origin the parser recorded
/// at `span` (ADR-10723 Stage 2), when the unit's source is known and the
/// node has none yet.
// Cost: O(log n), n = lines of the unit.
pub(super) fn set_span(node: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    if node.origin.0.is_none() {
        node.origin.0 = super::unit_source::span_origin_value(span);
    }
}

/// The range of a unit's top-level `StatementList`: from the start of its
/// first statement to the end of its last, the `;` terminators after it
/// included, as for a block's statements. `SrcSpan::NONE` when anything but
/// whitespace and comments surrounds the statements (Pod, which Rakudo keeps
/// as nodes, or text no statement accounts for).
// Cost: O(s + n), s = top-level statements, n = length of the text around them.
pub(super) fn unit_list_span(stmts: &[Stmt], source: &str) -> crate::ast::SrcSpan {
    let mut spans = stmts.iter().filter_map(|stmt| match stmt {
        Stmt::SetLine(_, span) if span.is_some() => Some(*span),
        _ => None,
    });
    let Some(first) = spans.next() else {
        return crate::ast::SrcSpan::NONE;
    };
    let last = spans.next_back().unwrap_or(first);
    let (from, end) = (first.from as usize, last.to as usize);
    match crate::source_comments::statement_list_end(source, from, end) {
        Some(to) => crate::ast::SrcSpan::new(from, to),
        None => crate::ast::SrcSpan::NONE,
    }
}

/// Give the one `StrLiteral` of a quoted string written at `span` (`"abc"`,
/// `Q{...}`) the range inside the quote's delimiters, as in Rakudo: from after
/// the opening delimiter (past a `q` / `qq` / `Q` and its adverbs) to the
/// closing one. A quote with processors (word quotes) or more than one
/// segment is left alone.
// Cost: O(n), n = length of the quote's source.
pub(super) fn quoted_text(node: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    if node.class != super::RakuAstClass::QuotedString {
        return;
    }
    let words = node.fields.iter().any(|f| f.name == Some("processors"));
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("segments")) else {
        return;
    };
    let RakuAstFieldValue::List(items) = &mut field.value else {
        return;
    };
    let [only] = items.as_mut_slice() else {
        return;
    };
    let ValueView::RakuAst(text) = only.view() else {
        return;
    };
    if text.class != super::RakuAstClass::StrLiteral || text.origin.0.is_some() {
        return;
    }
    let inside = super::unit_source::with_text(span, |written| {
        if words {
            // A word quote keeps the raw text between its brackets.
            let (open, close) = word_brackets(written)?;
            let inner = written.get(open..written.len().checked_sub(close)?)?;
            return (super::infix_origins::leaf_text(text)? == inner).then(|| {
                crate::ast::SrcSpan::new(
                    span.from as usize + open,
                    span.from as usize + written.len() - close,
                )
            });
        }
        let (open, delimiter) = written
            .char_indices()
            .find(|(_, c)| !(c.is_alphanumeric() || matches!(c, ':' | '_')))?;
        let start = open + delimiter.len_utf8();
        let (close, _) = written.char_indices().last()?;
        (close >= start).then(|| {
            crate::ast::SrcSpan::new(span.from as usize + start, span.from as usize + close)
        })
    });
    if let Some(inside) = inside {
        let mut text = RakuAstNode::clone(text);
        set_span(&mut text, inside);
        *only = Value::rakuast(Box::new(text));
    }
}

/// The byte length of the opening bracket of a word subscript written
/// `<...>`, `<<...>>` or `«...»`, and its closing bracket.
// Cost: O(1).
pub(super) fn word_brackets_open(text: &str) -> Option<(usize, &'static str)> {
    if text.starts_with("<<") {
        Some((2, ">>"))
    } else if text.starts_with('<') {
        Some((1, ">"))
    } else if text.starts_with('\u{ab}') {
        Some(('\u{ab}'.len_utf8(), "\u{bb}"))
    } else {
        None
    }
}

/// Give a word quote (`QuotedString` over one `StrLiteral`) and its text the
/// range `inner` between the brackets, when the text is exactly what is
/// written there.
// Cost: O(n), n = length of `inner`.
pub(super) fn word_quote_inner(quote: &mut RakuAstNode, inner: crate::ast::SrcSpan) {
    let written = super::unit_source::with_text(inner, |t| Some(t.to_string()));
    let matches = quote
        .fields
        .iter()
        .find(|f| f.name == Some("segments"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => match items.as_slice() {
                [only] => match only.view() {
                    ValueView::RakuAst(text) => super::infix_origins::leaf_text(text),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        });
    if quote.class != super::RakuAstClass::QuotedString || written.is_none() || written != matches {
        return;
    }
    set_span(quote, inner);
    if let Some(field) = quote.fields.iter_mut().find(|f| f.name == Some("segments"))
        && let RakuAstFieldValue::List(items) = &mut field.value
        && let [only] = items.as_mut_slice()
        && let ValueView::RakuAst(text) = only.view()
    {
        let mut text = RakuAstNode::clone(text);
        set_span(&mut text, inner);
        *only = Value::rakuast(Box::new(text));
    }
}

/// The byte lengths of the opening and closing brackets of a word quote
/// written as `<...>`, `<<...>>` or `«...»`.
// Cost: O(1).
pub(super) fn word_brackets(written: &str) -> Option<(usize, usize)> {
    if written.len() >= 4 && written.starts_with("<<") && written.ends_with(">>") {
        Some((2, 2))
    } else if written.len() >= 2 && written.starts_with('<') && written.ends_with('>') {
        Some((1, 1))
    } else if written.starts_with('\u{ab}') && written.ends_with('\u{bb}') {
        Some(('\u{ab}'.len_utf8(), '\u{bb}'.len_utf8()))
    } else {
        None
    }
}
