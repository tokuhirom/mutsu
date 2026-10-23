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
    node.origin = super::NodeOrigin(Some(super::unit_source::origin_value(line, span)));
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
        node.origin = super::NodeOrigin(super::unit_source::span_origin_value(span));
    }
}

/// The range of a unit's top-level `StatementList`: from the start of its
/// first statement to the end of its last, the `;` terminators that directly
/// follow it included, as for a block's statements.
// Cost: O(s + k), s = top-level statements, k = terminators skipped.
pub(super) fn unit_list_span(stmts: &[Stmt], source: &str) -> crate::ast::SrcSpan {
    let mut spans = stmts.iter().filter_map(|stmt| match stmt {
        Stmt::SetLine(_, span) if span.is_some() => Some(*span),
        _ => None,
    });
    let Some(first) = spans.next() else {
        return crate::ast::SrcSpan::NONE;
    };
    let last = spans.next_back().unwrap_or(first);
    let end = last.to as usize;
    let skipped = source
        .as_bytes()
        .get(end..)
        .map_or(0, |tail| tail.iter().take_while(|&&b| b == b';').count());
    crate::ast::SrcSpan::new(first.from as usize, end + skipped)
}

/// Give the one `StrLiteral` of a quoted string written at `span` (`"abc"`,
/// `Q{...}`) the range inside the quote's delimiters, as in Rakudo: from after
/// the opening delimiter (past a `q` / `qq` / `Q` and its adverbs) to the
/// closing one. A quote with processors (word quotes) or more than one
/// segment is left alone.
// Cost: O(n), n = length of the quote's source.
pub(super) fn quoted_text(node: &mut RakuAstNode, span: crate::ast::SrcSpan) {
    if node.class != super::RakuAstClass::QuotedString
        || node.fields.iter().any(|f| f.name == Some("processors"))
    {
        return;
    }
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
