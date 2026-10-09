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
use crate::value::{Value, ValueView};

thread_local! {
    /// The line of the statement being lowered, for the call-site markers the
    /// parser attaches to calls (see [`current_line`]).
    static CURRENT_LINE: Cell<Option<i64>> = const { Cell::new(None) };
}

/// The hidden field's name.
pub(super) const FIELD: &str = "origin";

/// The `origin` field for a statement that began on `line`.
// Cost: O(1).
pub(super) fn field(line: i64) -> RakuAstField {
    RakuAstField {
        name: Some(FIELD),
        value: RakuAstFieldValue::Node(Value::int(line)),
    }
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
    field.name == Some(FIELD) || field.name == Some(END_INDEX)
}

/// The line `node`'s statement began on, when the node carries one.
// Cost: O(f), f = fields of `node`.
pub(super) fn line_of(node: &RakuAstNode) -> Option<i64> {
    let field = node.fields.iter().find(|f| is_origin(f))?;
    let RakuAstFieldValue::Node(value) = &field.value else {
        return None;
    };
    match value.view() {
        ValueView::Int(line) => Some(line),
        _ => None,
    }
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
