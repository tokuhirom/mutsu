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
    /// The names of the packages the unit declares (`class Nil`, `role X`,
    /// `grammar G`, `module M`, `package P`).
    static PACKAGES: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    /// The `True` / `False` terms each enclosing statement list redeclares
    /// (`constant True = 42`), innermost last. Unlike `SHADOWED` these are
    /// scoped: the redeclaration holds in its own block and what is nested in it.
    static KEYWORD_SCOPES: RefCell<Vec<HashSet<String>>> = const { RefCell::new(Vec::new()) };
}

/// Pops the scope [`enter_scope`] pushed.
pub(super) struct KeywordScope;

impl Drop for KeywordScope {
    fn drop(&mut self) {
        KEYWORD_SCOPES.with(|s| {
            s.borrow_mut().pop();
        });
    }
}

/// Start a statement list: a `True` / `False` it declares is visible to the
/// rest of it and to what it contains, as in the parser.
// Cost: O(1).
pub(super) fn enter_scope() -> KeywordScope {
    KEYWORD_SCOPES.with(|s| s.borrow_mut().push(HashSet::new()));
    KeywordScope
}

/// Record the `True` / `False` term the statement `stmt` declares, if it does.
// Cost: O(1).
pub(super) fn note_keyword_declaration(stmt: &RakuAstNode) {
    let target = if stmt.class == RakuAstClass::StatementExpression {
        match named_child(stmt, "expression") {
            Ok(inner) => inner,
            Err(_) => return,
        }
    } else {
        stmt
    };
    let name = match target.class {
        RakuAstClass::VarDeclarationConstant => leaf_str(target, "name").ok(),
        RakuAstClass::VarDeclarationTerm => named_child(target, "name")
            .ok()
            .and_then(name_parts::name_shape)
            .and_then(|shape| match shape {
                NameShape::Identifier(name) => Some(name),
                _ => None,
            }),
        _ => None,
    };
    if let Some(name) = name.filter(|n| n == "True" || n == "False") {
        KEYWORD_SCOPES.with(|s| {
            if let Some(scope) = s.borrow_mut().last_mut() {
                scope.insert(name);
            }
        });
    }
}

/// Whether an enclosing statement list redeclares the term `True` / `False`.
// Cost: O(s), s = open scopes.
pub(super) fn is_keyword_shadowed(name: &str) -> bool {
    KEYWORD_SCOPES.with(|s| s.borrow().iter().any(|scope| scope.contains(name)))
}

/// Record the names `root` declares that lowering resolves other names
/// against: the constants it redeclares, its sigilless terms, its packages and
/// its routines (`declared_routines`). One walk of the unit finds all four.
// Cost: O(n), n = nodes in the unit.
pub(super) fn scan(root: &RakuAstNode) {
    KEYWORD_SCOPES.with(|s| s.borrow_mut().clear());
    let mut found = Found::default();
    walk(root, false, &mut found);
    SHADOWED.with(|s| *s.borrow_mut() = found.shadowed);
    TERMS.with(|t| *t.borrow_mut() = found.terms);
    PACKAGES.with(|p| *p.borrow_mut() = found.packages);
    super::declared_routines::set(found.routines);
}

/// Whether the unit last passed to [`scan`] declares a package named `name`.
/// A declared `class Nil` makes `Nil` that type, not the setting's `Nil` term.
// Cost: O(1).
pub(super) fn declares_package(name: &str) -> bool {
    PACKAGES.with(|p| p.borrow().contains(name))
}

/// Whether the unit last passed to [`scan`] declares a sigilless term `name`.
// Cost: O(1).
pub(super) fn is_declared_term(name: &str) -> bool {
    TERMS.with(|t| t.borrow().contains(name))
}

/// Whether `name` is a sigilless value term the unit declares or the parser
/// scope it is lowered in knows (a term imported from a module).
// Cost: O(s), s = open parser scopes.
pub(super) fn is_value_term(name: &str) -> bool {
    is_declared_term(name) || crate::parser::is_user_declared_value_term(name)
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

#[derive(Default)]
struct Found {
    shadowed: HashSet<String>,
    terms: HashSet<String>,
    packages: HashSet<String>,
    routines: HashSet<String>,
}

fn walk(node: &RakuAstNode, in_decl: bool, found: &mut Found) {
    if super::passthrough::is_leaf(node) {
        let mut facts = super::passthrough::ScopeFacts::default();
        super::passthrough::scope_facts_of(node, &mut facts);
        found.shadowed.extend(facts.terms.iter().cloned());
        found.shadowed.extend(facts.shadowed);
        found.terms.extend(facts.terms);
        found.packages.extend(facts.packages);
        super::passthrough::declared_routines_of(node, &mut found.routines);
        return;
    }
    if let Some(name) = super::declared_routines::declared_by(node) {
        found.routines.insert(name);
    }
    if matches!(
        node.class,
        RakuAstClass::Class
            | RakuAstClass::Role
            | RakuAstClass::Grammar
            | RakuAstClass::Module
            | RakuAstClass::Package
    ) && let Ok(name) = named_child(node, "name")
        && let Some(NameShape::Identifier(name)) = name_parts::name_shape(name)
    {
        found.packages.insert(name);
    }
    let declared = match node.class {
        RakuAstClass::VarDeclarationTerm => named_child(node, "name").ok(),
        RakuAstClass::ParameterTargetTerm => named_child_or_positional(node).ok(),
        _ => None,
    };
    if let Some(name) = declared
        && let Some(NameShape::Identifier(name)) = name_parts::name_shape(name)
    {
        found.terms.insert(name);
    }
    if node.class == RakuAstClass::VarDeclarationConstant
        && let Ok(name) = leaf_str(node, "name")
    {
        // `constant term:<$box> = ...` declares the term `$box`, sigil and all.
        if let Some(term) = name
            .strip_prefix("term:<")
            .and_then(|rest| rest.strip_suffix('>'))
        {
            found.terms.insert(term.to_string());
        } else if !name.starts_with(['$', '@', '%', '&']) {
            found.terms.insert(name);
        }
    }
    let in_decl = in_decl || is_term_declaration(node.class);
    for field in &node.fields {
        // The initializer of `constant p = pi` / `my \p = pi` reads terms, it
        // declares none.
        let in_decl = in_decl
            && !(matches!(
                node.class,
                RakuAstClass::VarDeclarationConstant | RakuAstClass::VarDeclarationTerm
            ) && field.name == Some("initializer"));
        match &field.value {
            RakuAstFieldValue::Node(v) => walk_value(v, in_decl, found),
            RakuAstFieldValue::List(items) => {
                for v in items {
                    walk_value(v, in_decl, found);
                }
            }
            RakuAstFieldValue::Adverb(_) => {}
        }
    }
}

fn walk_value(v: &crate::value::Value, in_decl: bool, found: &mut Found) {
    match v.view() {
        ValueView::RakuAst(child) => walk(child, in_decl, found),
        ValueView::Str(s) if in_decl => {
            found.shadowed.insert(s.to_string());
        }
        _ => {}
    }
}
