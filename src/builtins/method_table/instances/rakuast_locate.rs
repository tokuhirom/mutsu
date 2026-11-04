//! `RakuAST::Node.locate-node($pos, $to?)` (rakudo's `src/Raku/ast/base.rakumod`):
//! the deepest descendant whose origin covers a position (or the range
//! `$pos .. $to`), found through `visit-children`; `Nil` when the node itself
//! does not cover it. A range is half-open, so a node ending at `$pos` does not
//! cover it, and an empty or reversed range stands for the position itself.
//! With the `:key` adverb it answers the nearest enclosing statement-like node of
//! that deepest node (measured on rakudo 2026.09): a `Statement::*`, a
//! `StatementList`, a `Blockoid`, a `Sub` or a `Method`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_node::children;
use super::rakuast_visit::native_node;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::Node",
        name: "locate-node",
        arity: 1,
        handler: Handler::Interp(locate_node),
        flags: RowFlags::OWNER_ONLY,
        named: &["key"],
    },
    MethodRow {
        owner: "RakuAST::Node",
        name: "locate-node",
        arity: 2,
        handler: Handler::Interp(locate_node),
        flags: RowFlags::OWNER_ONLY,
        named: &["key"],
    },
];

/// The deepest nesting followed.
const MAX_DEPTH: usize = 4096;

// Cost: O(n), n = nodes on the path and their siblings, bounded by the tree.
fn locate_node(
    _interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let base = native_node(target)?;
    let key = named.get("key").is_some_and(|v| v.truthy());
    let int = |v: Option<&Value>| match v.map(Value::view) {
        Some(ValueView::Int(n)) => Some(n),
        _ => None,
    };
    let Some(pos) = int(args.first()).filter(|p| *p >= 0) else {
        return Some(Ok(Value::NIL));
    };
    let end = int(args.get(1)).map_or(pos + 1, |to| to.max(pos + 1));
    if !covers(&base, pos, end) && crate::rakuast::origin_range::of_value(&base).is_some() {
        return Some(Ok(Value::NIL));
    }
    if key {
        let mut path = vec![base.clone()];
        deepest_path(&base, pos, end, 0, &mut path);
        let found = path.iter().rev().find(|n| is_key_node(n)).cloned();
        return Some(Ok(found.unwrap_or(base)));
    }
    Some(Ok(deepest(&base, pos, end, 0).unwrap_or(base)))
}

/// Whether `node` is one of the statement-like nodes `:key` answers.
// Cost: O(1).
fn is_key_node(node: &Value) -> bool {
    let ValueView::RakuAst(native) = node.view() else {
        return false;
    };
    let name = native.class.printed_name();
    name.starts_with("RakuAST::Statement::")
        || matches!(
            name,
            "RakuAST::StatementList" | "RakuAST::Blockoid" | "RakuAST::Sub" | "RakuAST::Method"
        )
}

/// [`deepest`], keeping the nodes on the way down in `path`.
// Cost: O(n), n = nodes on the path and their siblings, bounded by the tree.
fn deepest_path(node: &Value, pos: i64, end: i64, depth: usize, path: &mut Vec<Value>) {
    if depth > MAX_DEPTH {
        return;
    }
    let ValueView::RakuAst(native) = node.view() else {
        return;
    };
    for child in children(native) {
        if covers(&child, pos, end) {
            path.push(child.clone());
            deepest_path(&child, pos, end, depth + 1, path);
            return;
        }
        if crate::rakuast::origin_range::of_value(&child).is_none() {
            let before = path.len();
            path.push(child.clone());
            deepest_path(&child, pos, end, depth + 1, path);
            if path.len() > before + 1 {
                return;
            }
            path.truncate(before);
        }
    }
}

/// Whether the origin of `node` covers `pos .. end`; a node without one does not.
fn covers(node: &Value, pos: i64, end: i64) -> bool {
    crate::rakuast::origin_range::of_value(node).is_some_and(|(from, to)| from <= pos && end <= to)
}

/// The deepest descendant of `node` covering `pos .. end`, looking through
/// children that carry no origin of their own.
fn deepest(node: &Value, pos: i64, end: i64, depth: usize) -> Option<Value> {
    if depth > MAX_DEPTH {
        return None;
    }
    let ValueView::RakuAst(native) = node.view() else {
        return None;
    };
    for child in children(native) {
        if covers(&child, pos, end) {
            return Some(deepest(&child, pos, end, depth + 1).unwrap_or(child));
        }
        if crate::rakuast::origin_range::of_value(&child).is_none()
            && let Some(found) = deepest(&child, pos, end, depth + 1)
        {
            return Some(found);
        }
    }
    None
}
