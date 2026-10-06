//! Which routine names a RakuAST unit declares itself.
//!
//! The parser chooses what a call means with a scope it keeps while parsing: a
//! `sub take { }` in scope makes `take(1)` a call of that sub rather than the
//! builtin `take` statement, and a declared sub makes a bare `foo` a call with
//! a call-site marker. The node says only that a name is called, so lowering
//! re-derives the choice from the declarations the unit holds: a named `sub`, a
//! `my &name` and a `&name` parameter.
//!
//! The scan ignores scoping (a declaration anywhere in the unit counts), which
//! is the same approximation `shadowed_terms` makes for the math constants.

use std::cell::RefCell;
use std::collections::HashSet;

use super::lower::{call_name_str, leaf_str};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::ValueView;

thread_local! {
    static DECLARED: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
}

/// Record the routine names `root` declares, replacing the previous unit's.
// Cost: O(n), n = nodes in the unit.
pub(super) fn scan(root: &RakuAstNode) {
    let mut found = HashSet::new();
    visit(root, &mut found);
    DECLARED.with(|d| *d.borrow_mut() = found);
}

/// Whether the unit last passed to [`scan`] declares a routine named `name`.
// Cost: O(1).
pub(super) fn is_declared(name: &str) -> bool {
    DECLARED.with(|d| d.borrow().contains(name))
}

/// The routine name `node` itself declares (not its children): a named `sub`,
/// a `my &name`, a `&name` parameter.
// Cost: O(1).
pub(super) fn declared_by(node: &RakuAstNode) -> Option<String> {
    match node.class {
        RakuAstClass::Sub if node.fields.iter().any(|f| f.name == Some("name")) => {
            call_name_str(node).ok()
        }
        // `my &name` / `our &name`.
        RakuAstClass::VarDeclarationSimple if leaf_str(node, "sigil").is_ok_and(|s| s == "&") => {
            let field = node.fields.iter().find(|f| f.name == Some("desigilname"))?;
            let RakuAstFieldValue::Node(v) = &field.value else {
                return None;
            };
            let ValueView::RakuAst(name) = v.view() else {
                return None;
            };
            match name_parts::name_shape(name) {
                Some(NameShape::Identifier(name)) => Some(name),
                _ => None,
            }
        }
        // A `&name` parameter.
        RakuAstClass::ParameterTargetVar => leaf_str(node, "name")
            .ok()
            .and_then(|name| name.strip_prefix('&').map(str::to_string)),
        _ => None,
    }
}

fn visit(node: &RakuAstNode, found: &mut HashSet<String>) {
    if let Some(name) = declared_by(node) {
        found.insert(name);
    }
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(v) => visit_value(v, found),
            RakuAstFieldValue::List(items) => {
                for v in items {
                    visit_value(v, found);
                }
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
}

fn visit_value(v: &crate::value::Value, found: &mut HashSet<String>) {
    if let ValueView::RakuAst(child) = v.view() {
        visit(child, found);
    }
}
