//! `RakuAST::Node.dump` and its parts (`dump-markers`, `dump-origin`,
//! `dump-extras`, `dump-children`), as rakudo 2026.11 defines them in
//! `src/Raku/ast/base.rakumod` and overrides them per class: one line per
//! node (indentation, the class name without `RakuAST::`, the markers, the
//! origin), then the node's extras, then its children one level deeper
//! (#18333).
//!
//! The dump is composed from the node's own answers (`sunk`,
//! `is-block-statement`, `origin.is-key`, `visit-children`), so it is as
//! exact as they are.

use crate::rakuast::{RakuAstClass, RakuAstFieldValue, RakuAstNode, node_accessor, origin_value_of, unit_source};
use crate::value::{Value, ValueView};

/// `$node.dump($indent)`.
// Cost: O(n + t), n = nodes of the tree, t = length of their origins' text.
pub(crate) fn dump(node: &RakuAstNode, indent: usize) -> String {
    let mut out = " ".repeat(indent);
    out.push_str(node.class.printed_name().trim_start_matches("RakuAST::"));
    let markers = markers(node);
    if !markers.is_empty() {
        out.push(' ');
        out.push_str(&markers);
    }
    let origin = origin(node);
    if !origin.is_empty() {
        out.push(' ');
        out.push_str(&origin);
    }
    out.push('\n');
    out.push_str(&extras(node, indent + 2));
    out.push_str(&children(node, indent + 2));
    out
}

/// `dump-markers`: a class's own marker (`【+】` for an `Infix`), else the
/// base markers: `⚓` for a sunk node, `▪` for a block statement and `𝄞` for
/// a statement-level origin.
// Cost: O(f), f = fields of the node.
pub(crate) fn markers(node: &RakuAstNode) -> String {
    let field = |name: &str| node_accessor(node, name).map(|v| v.to_string_value());
    let own = match node.class {
        RakuAstClass::Infix | RakuAstClass::Assignment => field("operator"),
        RakuAstClass::Name => field("canonicalize"),
        RakuAstClass::VarLexical | RakuAstClass::VarLexicalConstant => field("name"),
        RakuAstClass::VarDeclarationSimple => declared_name(node),
        RakuAstClass::WhateverCodeArgument => return "🔆".to_string(),
        _ => None,
    };
    if let Some(own) = own {
        return format!("【{own}】");
    }
    let flag = |name: &str| node_accessor(node, name).is_some_and(|v| v.truthy());
    let mut out = String::new();
    if flag("sunk") {
        out.push('⚓');
    }
    if flag("is-block-statement") {
        out.push('▪');
    }
    if origin_is_key(node) {
        out.push('𝄞');
    }
    out
}

/// `VarDeclaration::Simple.name`: its sigil, twigil and desigilname.
// Cost: O(f), f = fields of the node.
fn declared_name(node: &RakuAstNode) -> Option<String> {
    let part = |name: &str| node_accessor(node, name).map(|v| v.to_string_value());
    // A declaration that records its whole name (a parameter's target).
    let desigil = match node_accessor(node, "desigilname").as_ref().map(Value::view) {
        Some(ValueView::RakuAst(name)) => node_accessor(name, "canonicalize")?.to_string_value(),
        _ => return part("name"),
    };
    Some(format!(
        "{}{}{}",
        part("sigil").unwrap_or_default(),
        part("twigil").unwrap_or_default(),
        desigil
    ))
}

/// Whether the node's origin is a statement-level (key) one.
// Cost: O(1).
fn origin_is_key(node: &RakuAstNode) -> bool {
    let Some(origin) = origin_value_of(node) else {
        return false;
    };
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return false;
    };
    attributes
        .as_map()
        .get(unit_source::KEY_ATTR)
        .is_some_and(Value::truthy)
}

/// `Origin.is-key` of the origin instance `origin`.
// Cost: O(1).
pub(crate) fn is_key(origin: &Value) -> bool {
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return false;
    };
    attributes
        .as_map()
        .get(unit_source::KEY_ATTR)
        .is_some_and(Value::truthy)
}

/// `dump-origin`: `file:line` for a key origin, then ` ⎡text⎤` with the
/// source text escaped and cut to 49 characters and `…`.
// Cost: O(t), t = end offset of the origin.
pub(crate) fn origin(node: &RakuAstNode) -> String {
    let Some(origin) = origin_value_of(node) else {
        return String::new();
    };
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return String::new();
    };
    let map = attributes.as_map();
    let (Some(ValueView::Int(from)), Some(ValueView::Int(to))) = (
        map.get("from").map(Value::view),
        map.get("to").map(Value::view),
    ) else {
        return String::new();
    };
    let Some(source) = map.get("source") else {
        return String::new();
    };
    let mut out = String::new();
    if is_key(&origin)
        && let Some((line, _, file)) = location_of_pos(source, from)
    {
        out.push_str(&format!("{file}:{line}"));
    }
    let text: String = source_text(source)
        .chars()
        .skip(from.max(0) as usize)
        .take((to - from).max(0) as usize)
        .collect();
    let mut escaped = escape(&text);
    if escaped.chars().count() > 50 {
        escaped = escaped.chars().take(49).collect::<String>() + "…";
    }
    out.push_str(" ⎡");
    out.push_str(&escaped);
    out.push('⎤');
    out
}

/// The text of an `Origin::Source`.
// Cost: O(1).
fn source_text(source: &Value) -> String {
    let ValueView::Instance { attributes, .. } = source.view() else {
        return String::new();
    };
    attributes
        .as_map()
        .get(unit_source::ORIG_ATTR)
        .map(Value::to_string_value)
        .unwrap_or_default()
}

/// `Origin::Source.location-of-pos($pos)`: the 1-based line and column of the
/// character offset `pos`, and the unit's file name.
// Cost: O(log n), n = lines of the unit.
pub(crate) fn location_of_pos(source: &Value, pos: i64) -> Option<(i64, i64, String)> {
    let ValueView::Instance { attributes, .. } = source.view() else {
        return None;
    };
    let map = attributes.as_map();
    let file = map
        .get(unit_source::ORIGINAL_FILE_ATTR)
        .map(Value::to_string_value)
        .unwrap_or_default();
    let starts: Vec<i64> = match map.get(unit_source::LINE_STARTS_ATTR)?.view() {
        ValueView::Array(starts, _) => starts
            .iter()
            .filter_map(|s| match s.view() {
                ValueView::Int(i) => Some(i),
                _ => None,
            })
            .collect(),
        _ => return None,
    };
    let line = starts.partition_point(|&start| start <= pos).max(1);
    let column = pos - starts.get(line - 1).copied().unwrap_or(0) + 1;
    Some((line as i64, column, file))
}

/// `nqp::escape`: backslash escapes for `\`, `"` and the control characters
/// MoarVM names.
// Cost: O(t), t = length of the text.
fn escape(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{8}' => out.push_str("\\b"),
            '\u{c}' => out.push_str("\\f"),
            '\u{7}' => out.push_str("\\a"),
            '\u{1b}' => out.push_str("\\e"),
            _ => out.push(ch),
        }
    }
    out
}

/// `dump-extras($indent)`: a `QuotedString`'s postprocessors.
// Cost: O(p), p = postprocessors.
pub(crate) fn extras(node: &RakuAstNode, indent: usize) -> String {
    if node.class != RakuAstClass::QuotedString {
        return String::new();
    }
    let processors: Vec<String> = node
        .fields
        .iter()
        .find(|f| f.name == Some("processors"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => {
                Some(items.iter().map(Value::to_string_value).collect())
            }
            _ => None,
        })
        .unwrap_or_default();
    if processors.is_empty() {
        return String::new();
    }
    format!(
        "{}postprocessors: {}\n",
        " ".repeat(indent),
        processors.join(", ")
    )
}

/// `dump-children($indent)`: each child `visit-children` hands out, dumped at
/// `indent`.
// Cost: O(n + t) over the subtree, as `dump`.
pub(crate) fn children(node: &RakuAstNode, indent: usize) -> String {
    super::rakuast_node::children(node)
        .iter()
        .filter_map(|child| match child.view() {
            ValueView::RakuAst(child) => Some(dump(child, indent)),
            _ => None,
        })
        .collect()
}

use super::super::{Handler, MethodRow, RowFlags};
use crate::value::RuntimeError;

/// The node a RakuAST value or a user subclass instance of one stands for.
// Cost: O(1).
fn node_of(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::RakuAst(_) => Some(target.clone()),
        ValueView::Instance { attributes, .. } => attributes.rakuast_base(),
        _ => None,
    }
}

/// The optional `int $indent` argument (0 when absent).
// Cost: O(1).
fn indent_arg(args: &[Value]) -> Option<usize> {
    match args.first().map(Value::view) {
        None => Some(0),
        Some(ValueView::Int(i)) => Some(i.max(0) as usize),
        _ => None,
    }
}

/// A row whose handler renders the node with `$render`.
macro_rules! node_rows {
    ($($method:literal, $arity:literal => $handler:ident = |$node:ident, $indent:ident| $render:expr;)*) => {
        $(
            // Cost: O(n + t) over the node's subtree, as `dump`.
            fn $handler(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
                let base = node_of(target)?;
                let ValueView::RakuAst($node) = base.view() else {
                    return None;
                };
                let $indent = indent_arg(args)?;
                let _ = $indent;
                Some(Ok(Value::str($render)))
            }
        )*
        const NODE_ROWS: &[MethodRow] = &[
            $(MethodRow {
                owner: "RakuAST::Node",
                name: $method,
                arity: $arity,
                handler: Handler::Narrow($handler),
                flags: RowFlags::OWNER_ONLY,
                named: &[],
            },)*
        ];
    };
}

node_rows! {
    "dump", 0 => dump_row = |node, indent| dump(node, indent);
    "dump", 1 => dump_indented_row = |node, indent| dump(node, indent);
    "dump-markers", 0 => markers_row = |node, indent| markers(node);
    "dump-origin", 0 => origin_row = |node, indent| origin(node);
    "dump-extras", 1 => extras_row = |node, indent| extras(node, indent);
    "dump-children", 1 => children_row = |node, indent| children(node, indent);
}

/// `Origin.is-key`.
// Cost: O(1).
fn is_key_row(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(Value::truth(is_key(target))))
}

/// `Origin::Source.location-of-pos($pos)`: `(line, column, file)`.
// Cost: O(log n), n = lines of the unit.
fn location_of_pos_row(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let ValueView::Int(pos) = args.first()?.view() else {
        return None;
    };
    let (line, column, file) = location_of_pos(target, pos)?;
    Some(Ok(Value::array(vec![
        Value::int(line),
        Value::int(column),
        Value::str(file),
    ])))
}

pub(super) static ROWS: &[MethodRow] = &[
    NODE_ROWS[0],
    NODE_ROWS[1],
    NODE_ROWS[2],
    NODE_ROWS[3],
    NODE_ROWS[4],
    NODE_ROWS[5],
    MethodRow {
        owner: "RakuAST::Origin",
        name: "is-key",
        arity: 0,
        handler: Handler::Narrow(is_key_row),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Origin::Source",
        name: "location-of-pos",
        arity: 1,
        handler: Handler::Narrow(location_of_pos_row),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
];
