//! Which math-constant term names a RakuAST unit redeclares.
//!
//! `Term::Name` for `pi`/`e`/`tau` is the setting constant unless the unit
//! declares a term of that name (`-> \e { }`, `constant e`, an enum member):
//! the parser makes the same choice at parse time, but the node does not say
//! which one it meant, so lowering re-derives it from the unit's declarations.

use std::cell::RefCell;
use std::collections::HashSet;

use super::lower::{leaf_str, named_child, named_child_or_positional};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::ValueView;

thread_local! {
    static SHADOWED: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    /// The sigilless terms the unit declares: `my \x`, `constant x`, a `\x`
    /// parameter. Unlike `SHADOWED`, only the declared name counts.
    static TERMS: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
}

/// Record the constant names `root` redeclares, replacing the previous unit's.
// Cost: O(n), n = nodes in the unit.
pub(super) fn scan(root: &RakuAstNode) {
    let mut found = HashSet::new();
    visit(root, false, &mut found);
    SHADOWED.with(|s| *s.borrow_mut() = found);
    let mut terms = HashSet::new();
    visit_terms(root, &mut terms);
    TERMS.with(|t| *t.borrow_mut() = terms);
}

/// Whether the unit last passed to [`scan`] declares a sigilless term `name`.
// Cost: O(1).
pub(super) fn is_declared_term(name: &str) -> bool {
    TERMS.with(|t| t.borrow().contains(name))
}

fn visit_terms(node: &RakuAstNode, found: &mut HashSet<String>) {
    let declared = match node.class {
        RakuAstClass::VarDeclarationTerm => named_child(node, "name").ok(),
        RakuAstClass::ParameterTargetTerm => named_child_or_positional(node).ok(),
        _ => None,
    };
    if let Some(name) = declared
        && let Some(NameShape::Identifier(name)) = name_parts::name_shape(name)
    {
        found.insert(name);
    }
    if node.class == RakuAstClass::VarDeclarationConstant
        && let Ok(name) = leaf_str(node, "name")
        && !name.starts_with(['$', '@', '%', '&'])
    {
        found.insert(name);
    }
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(v) => visit_terms_value(v, found),
            RakuAstFieldValue::List(items) => {
                for v in items {
                    visit_terms_value(v, found);
                }
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
}

fn visit_terms_value(v: &crate::value::Value, found: &mut HashSet<String>) {
    if let ValueView::RakuAst(child) = v.view() {
        visit_terms(child, found);
    }
}

/// Whether the unit last passed to [`scan`] declares a term named `name`.
// Cost: O(1).
pub(super) fn is_shadowed(name: &str) -> bool {
    SHADOWED.with(|s| s.borrow().contains(name))
}

fn is_term_declaration(class: RakuAstClass) -> bool {
    matches!(
        class,
        RakuAstClass::ParameterTargetTerm
            | RakuAstClass::VarDeclarationConstant
            | RakuAstClass::VarDeclarationTerm
            | RakuAstClass::TypeEnum
    )
}

fn visit(node: &RakuAstNode, in_decl: bool, found: &mut HashSet<String>) {
    let in_decl = in_decl || is_term_declaration(node.class);
    for field in &node.fields {
        match &field.value {
            RakuAstFieldValue::Node(v) => visit_value(v, in_decl, found),
            RakuAstFieldValue::List(items) => {
                for v in items {
                    visit_value(v, in_decl, found);
                }
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
}

fn visit_value(v: &crate::value::Value, in_decl: bool, found: &mut HashSet<String>) {
    match v.view() {
        ValueView::RakuAst(child) => visit(child, in_decl, found),
        ValueView::Str(s) if in_decl => {
            found.insert(s.to_string());
        }
        _ => {}
    }
}
