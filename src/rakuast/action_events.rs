//! The grammar actions a RakuAST tree stands for.
//!
//! `Str.AST(:actions($obj))` runs the actions object's methods for the
//! productions rakudo's grammar would have reported while parsing. mutsu's
//! parser does not call them, so the productions are read back from the tree
//! (a `use`, a `BEGIN`, a `constant`) and from the parser's comment log, each
//! with the character range it covers, in the order the grammar finishes them.

use super::{RakuAstClass as C, RakuAstField, RakuAstFieldValue, RakuAstNode, origin_range};
use crate::value::{Value, ValueView};

/// One production to report.
pub(crate) struct Event {
    /// The action method's name, `comment:sym<#>`.
    pub(crate) rule: &'static str,
    pub(crate) from: i64,
    pub(crate) to: i64,
    /// The `<module-name>` capture of a `use`.
    pub(crate) name: Option<String>,
}

/// The deepest nesting followed.
const MAX_DEPTH: usize = 4096;

/// The productions of `tree`, `comments` (character ranges) and the
/// `=finish` line of `source`, ordered as the grammar finishes them: by end,
/// the inner before the outer.
// Cost: O(n + c log c), n = nodes of the tree, c = events.
pub(crate) fn collect(
    tree: &Value,
    source: &str,
    comments: &[(i64, i64)],
    children: &dyn Fn(&RakuAstNode) -> Vec<Value>,
) -> Vec<Event> {
    let mut events = Vec::new();
    walk(tree, 0, children, &mut events);
    for &(from, to) in comments {
        events.push(Event {
            rule: "comment:sym<#>",
            from,
            to,
            name: None,
        });
    }
    if let (code, Some(_)) = crate::parser::split_finish_section(source) {
        let from = code.chars().count() as i64 + 1;
        events.push(Event {
            rule: "doc-block:sym<finish>",
            from,
            to: source.chars().count() as i64,
            name: None,
        });
    }
    events.sort_by_key(|e| (e.to, std::cmp::Reverse(e.from)));
    events
}

// Cost: O(size of the subtree).
fn walk(
    value: &Value,
    depth: usize,
    children: &dyn Fn(&RakuAstNode) -> Vec<Value>,
    out: &mut Vec<Event>,
) {
    let ValueView::RakuAst(node) = value.view() else {
        return;
    };
    if depth > MAX_DEPTH {
        return;
    }
    let rule = match node.class {
        C::StatementUse | C::Pragma => Some(("statement-control:sym<use>", module_name(node))),
        C::StatementPrefixPhaserBegin => Some(("statement-prefix:sym<BEGIN>", None)),
        C::VarDeclarationConstant => Some(("type-declarator:sym<constant>", None)),
        _ => None,
    };
    if let (Some((rule, name)), Some((from, to))) = (rule, origin_range::of(node)) {
        out.push(Event {
            rule,
            from,
            to,
            name,
        });
    }
    for child in children(node) {
        walk(&child, depth + 1, children, out);
    }
}

/// The module a `use` names: a pragma's `name`, or the parts of the
/// `module-name` joined by `::`.
// Cost: O(f).
fn module_name(node: &RakuAstNode) -> Option<String> {
    if node.class == C::Pragma {
        return node.fields.iter().find_map(|f| match (&f.name, &f.value) {
            (Some("name"), RakuAstFieldValue::Node(v)) => Some(v.to_string_value()),
            _ => None,
        });
    }
    let name = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some("module-name"), RakuAstFieldValue::Node(v)) => match v.view() {
            ValueView::RakuAst(n) => Some(n.clone()),
            _ => None,
        },
        _ => None,
    })?;
    let mut parts = Vec::new();
    for RakuAstField { value, .. } in &name.fields {
        match value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => parts.push(s.to_string()),
                _ => parts.push(super::name_parts::part_identifier(v)?),
            },
            RakuAstFieldValue::List(values) => {
                for v in values {
                    parts.push(super::name_parts::part_identifier(v)?);
                }
            }
            RakuAstFieldValue::Adverb(_) => return None,
        }
    }
    (!parts.is_empty()).then(|| parts.join("::"))
}
