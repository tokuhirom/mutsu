//! A package node's type declared ahead of `EVAL` (ADR-17472).
//!
//! In rakudo a `RakuAST::Class` (`Role`, `Grammar`, `Package`, `Module`) has
//! its type object from the moment it is built, and `EVAL` composes that same
//! type. mutsu declares a package only when the lowered unit runs, so
//! `meta-object` of a package node declares it on demand, and the later `EVAL`
//! adopts the type instead of declaring a second one. Both sides meet on the
//! node's unit source (the `RakuAST::Origin::Source` instance every origin of
//! one `.AST` shares), keyed by the node's start offset:
//!
//! - `.AST` records the unit's layout there: which top-level statements are
//!   `use`-like (the prelude a declaration needs) and which declare a package;
//! - the type a node declared (ahead of `EVAL`, or by an `EVAL` of its unit)
//!   is recorded there too.
//!
//! The layout keeps offsets only, never nodes: a node's origin points back at
//! the source, so a stored tree would be a reference cycle.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The `[kind, from, to]` rows of a unit's top-level statements.
const LAYOUT_ATTR: &str = "__mutsu_unit_layout";
/// The types the unit's package nodes declared, keyed by start offset.
const DECLARED_ATTR: &str = "__mutsu_declared_ahead";

const PRELUDE: &str = "prelude";
const PACKAGE: &str = "package";

/// Whether `class` declares a package type.
// Cost: O(1).
pub(crate) fn is_package_class(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::Class
            | RakuAstClass::Role
            | RakuAstClass::Grammar
            | RakuAstClass::Package
            | RakuAstClass::Module
    )
}

/// The unit source and `[from, to]` of a node's origin.
// Cost: O(1).
fn origin_span(node: &RakuAstNode) -> Option<(Value, i64, i64)> {
    let origin = super::origin::origin_of(node)?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let attrs = attributes.as_map();
    let source = attrs.get("source")?.clone();
    let (ValueView::Int(from), ValueView::Int(to)) =
        (attrs.get("from")?.view(), attrs.get("to")?.view())
    else {
        return None;
    };
    Some((source, from, to))
}

/// The node a `Value` holds.
// Cost: O(1).
fn node_of(value: &Value) -> Option<&RakuAstNode> {
    match value.view() {
        ValueView::RakuAst(node) => Some(node),
        _ => None,
    }
}

/// Record the top-level layout of the unit `.AST` returned (`tree`, a
/// `StatementList`) on its source.
// Cost: O(s), s = top-level statements.
pub(crate) fn record_unit_layout(tree: &Value) {
    let Some(list) = node_of(tree) else {
        return;
    };
    if list.class != RakuAstClass::StatementList {
        return;
    }
    let Some((source, _, _)) = origin_span(list) else {
        return;
    };
    let mut rows = Vec::new();
    for field in &list.fields {
        let RakuAstFieldValue::Node(value) = &field.value else {
            continue;
        };
        let Some(statement) = node_of(value) else {
            continue;
        };
        let (kind, spanned) = match statement.class {
            RakuAstClass::StatementUse
            | RakuAstClass::StatementNeed
            | RakuAstClass::StatementImport
            | RakuAstClass::Pragma => (PRELUDE, statement),
            RakuAstClass::StatementExpression => {
                let Some(expression) = statement
                    .fields
                    .iter()
                    .find(|f| f.name == Some("expression"))
                    .and_then(|f| match &f.value {
                        RakuAstFieldValue::Node(v) => node_of(v),
                        _ => None,
                    })
                else {
                    continue;
                };
                if !is_package_class(expression.class) {
                    continue;
                }
                (PACKAGE, expression)
            }
            _ => continue,
        };
        let Some((_, from, to)) = origin_span(spanned) else {
            continue;
        };
        rows.push(Value::array(vec![
            Value::str_from(kind),
            Value::int(from),
            Value::int(to),
        ]));
    }
    if let ValueView::Instance { attributes, .. } = source.view() {
        attributes.insert(LAYOUT_ATTR.to_string(), Value::array(rows));
    }
}

/// The type the package node `node` declared, if any (ahead of `EVAL` or by
/// an `EVAL` of its unit).
// Cost: O(1) average (one hash probe).
pub(crate) fn declared_type(node: &RakuAstNode) -> Option<Value> {
    if !is_package_class(node.class) {
        return None;
    }
    let (source, from, _) = origin_span(node)?;
    let ValueView::Instance { attributes, .. } = source.view() else {
        return None;
    };
    let declared = attributes.as_map().get(DECLARED_ATTR)?.clone();
    let ValueView::Hash(map) = declared.view() else {
        return None;
    };
    map.get(from.to_string().as_str()).cloned()
}

/// Record `ty` as the type the package node `node` declared.
// Cost: O(d), d = types the unit declared (the table is copied on write).
pub(crate) fn record_declared_type(node: &RakuAstNode, ty: &Value) {
    let Some((source, from, _)) = origin_span(node) else {
        return;
    };
    let ValueView::Instance { attributes, .. } = source.view() else {
        return;
    };
    let mut map = match attributes.as_map().get(DECLARED_ATTR).map(Value::view) {
        Some(ValueView::Hash(map)) => (**map).clone(),
        _ => Default::default(),
    };
    map.insert(from.to_string(), ty.clone());
    attributes.insert(DECLARED_ATTR.to_string(), Value::hash_bare_values(map));
}

/// What declaring the package node `node` ahead of `EVAL` runs: the text of
/// the `use`-like statements before it in its unit (its prelude), joined.
/// `None` when the node is not a top-level declaration of a unit `.AST`
/// parsed (nested in another package or a routine, or built by hand).
// Cost: O(n), n = length of the unit's text.
pub(crate) fn prelude_text(node: &RakuAstNode) -> Option<String> {
    let (source, from, _) = origin_span(node)?;
    let ValueView::Instance { attributes, .. } = source.view() else {
        return None;
    };
    let attrs = attributes.as_map();
    let orig = attrs.get(super::unit_source::ORIG_ATTR)?.to_string_value();
    let layout = attrs.get(LAYOUT_ATTR)?.clone();
    drop(attrs);
    let rows = layout.as_list_items()?;
    let mut top_level = false;
    let mut prelude = String::new();
    for row in rows.iter() {
        let cells = row.as_list_items()?;
        let [kind, start, end] = cells else {
            continue;
        };
        let (ValueView::Int(start), ValueView::Int(end)) = (start.view(), end.view()) else {
            continue;
        };
        let kind = kind.to_string_value();
        if kind == PACKAGE && start == from {
            top_level = true;
        }
        if kind == PRELUDE && end <= from {
            prelude.push_str(&grapheme_slice(&orig, start, end));
            prelude.push_str(";\n");
        }
    }
    top_level.then_some(prelude)
}

/// The text between grapheme offsets `from` and `to` of `text` (an origin
/// counts graphemes, as rakudo's does).
// Cost: O(n), n = length of `text`.
fn grapheme_slice(text: &str, from: i64, to: i64) -> String {
    let cp = |g: i64| crate::value::match_helpers::grapheme_to_cp_index(text, g.max(0) as usize);
    let (start, end) = (cp(from), cp(to));
    text.chars()
        .skip(start)
        .take(end.saturating_sub(start))
        .collect()
}

/// The package declarations at the top level of the statement list `tree`.
// Cost: O(s), s = top-level statements.
pub(crate) fn toplevel_packages(tree: &RakuAstNode) -> Vec<RakuAstNode> {
    if tree.class != RakuAstClass::StatementList {
        return Vec::new();
    }
    tree.fields
        .iter()
        .filter_map(|field| match &field.value {
            RakuAstFieldValue::Node(value) => node_of(value),
            _ => None,
        })
        .filter(|statement| statement.class == RakuAstClass::StatementExpression)
        .filter_map(|statement| {
            statement
                .fields
                .iter()
                .find(|f| f.name == Some("expression"))
                .and_then(|f| match &f.value {
                    RakuAstFieldValue::Node(v) => node_of(v),
                    _ => None,
                })
        })
        .filter(|expression| is_package_class(expression.class))
        .cloned()
        .collect()
}

/// The name a package node declares, as written (`Foo`, `A::B`); `None` for
/// an anonymous one.
// Cost: O(n), n = length of the name.
pub(crate) fn package_name(node: &RakuAstNode) -> Option<String> {
    super::lower::call_name_str(node).ok()
}
