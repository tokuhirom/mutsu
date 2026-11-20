//! `Node.fileline` and `Origin::Source.original-file`: which file a RakuAST
//! node was read from, and on which line (#14916, #15531).

use crate::rakuast::RakuAstNode;
use crate::value::{RuntimeError, Value, ValueView};

/// `Node.fileline`: `"<file>:<line>"` where the node's origin begins. The unit
/// itself starts on line 1; a node built by hand has no origin and answers
/// `Nil`, as in rakudo.
// Cost: O(f), f = fields of the node.
pub(super) fn fileline(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let base = match target.view() {
        ValueView::RakuAst(_) => target.clone(),
        ValueView::Instance { attributes, .. } => attributes.rakuast_base()?,
        _ => return None,
    };
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    let line = if node.class == crate::rakuast::RakuAstClass::CompUnit {
        Some(1)
    } else {
        crate::rakuast::origin_line_of(node)
    };
    let Some(line) = line else {
        return Some(Ok(Value::NIL));
    };
    // The unit the node was parsed in (`EVAL_0` for a `Str.AST`), else the
    // unit running now.
    let file = origin_file(node)
        .or_else(|| compunit_file(node))
        .or_else(|| crate::unit_source_file::current().map(|file| file.resolve().to_string()))
        .unwrap_or_default();
    Some(Ok(Value::str(format!("{file}:{line}"))))
}

/// A CompUnit carries no origin of its own: it names the file its statements
/// were read from (`EVAL_0` for a unit compiled from a `Str.AST` tree).
// Cost: O(s), s = top-level statements before the first with an origin.
fn compunit_file(node: &RakuAstNode) -> Option<String> {
    use crate::rakuast::{RakuAstClass, RakuAstFieldValue};
    if node.class != RakuAstClass::CompUnit {
        return None;
    }
    let list = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some("statement-list"), RakuAstFieldValue::Node(v)) => Some(v.clone()),
        _ => None,
    })?;
    let ValueView::RakuAst(list) = list.view() else {
        return None;
    };
    origin_file(list).or_else(|| {
        list.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::List(items) => items.iter().find_map(|item| match item.view() {
                ValueView::RakuAst(stmt) => origin_file(stmt),
                _ => None,
            }),
            _ => None,
        })
    })
}

/// The `original-file` of the source a node's origin points into.
// Cost: O(1).
fn origin_file(node: &RakuAstNode) -> Option<String> {
    let origin = crate::rakuast::origin_value_of(node)?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let source = attributes.as_map().get("source")?.clone();
    source_original_file(&source)
}

/// `Origin::Source.original-file` of `source`, when it records one.
// Cost: O(1).
fn source_original_file(source: &Value) -> Option<String> {
    let ValueView::Instance { attributes, .. } = source.view() else {
        return None;
    };
    attributes
        .as_map()
        .get(crate::rakuast::unit_source::ORIGINAL_FILE_ATTR)
        .and_then(|v| v.as_str().map(str::to_string))
}

/// `Origin::Source.original-file`: the unit the source was parsed as
/// (`EVAL_0` for a `Str.AST`), `Nil` when it was not recorded.
// Cost: O(1).
pub(super) fn original_file(
    target: &Value,
    _args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(
        source_original_file(target).map_or(Value::NIL, Value::str)
    ))
}
