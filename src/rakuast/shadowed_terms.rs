//! Which math-constant term names a RakuAST unit redeclares.
//!
//! `Term::Name` for `pi`/`e`/`tau` is the setting constant unless the unit
//! declares a term of that name (`-> \e { }`, `constant e`, an enum member):
//! the parser makes the same choice at parse time, but the node does not say
//! which one it meant, so lowering re-derives it from the unit's declarations.

use std::cell::RefCell;
use std::collections::HashSet;

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::ValueView;

thread_local! {
    static SHADOWED: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
}

/// Record the constant names `root` redeclares, replacing the previous unit's.
// Cost: O(n), n = nodes in the unit.
pub(super) fn scan(root: &RakuAstNode) {
    let mut found = HashSet::new();
    visit(root, false, &mut found);
    SHADOWED.with(|s| *s.borrow_mut() = found);
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
        ValueView::RakuAst(child) => visit(&child, in_decl, found),
        ValueView::Str(s) if in_decl => {
            found.insert(s.to_string());
        }
        _ => {}
    }
}
