//! Which math-constant term names a RakuAST unit redeclares.
//!
//! `Term::Name` for `pi`/`e`/`tau` is the setting constant unless the unit
//! declares a term of that name (`-> \e { }`, `constant e`, an enum member):
//! the parser makes the same choice at parse time, but the node does not say
//! which one it meant, so lowering re-derives it from the unit's declarations.

use std::collections::HashSet;

use super::lower::{leaf_str, named_child, named_child_or_positional};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Stmt;
use crate::ast::parsed::ParsedStmt;
use crate::ast_visit::Visit;
use crate::scope::{ListScope, ListStack, NameScope, UnitFacts, UnitGuard};
use crate::value::ValueView;

/// Opens a statement list's scope for the `True` / `False` it redeclares, which
/// holds in its own block and what is nested in it, as in the parser.
// Cost: O(1).
pub(super) fn enter_scope(names: &NameScope) -> ListScope<'_> {
    ListScope::enter(names, ListStack::Keywords)
}

/// Record the `True` / `False` term the statement `stmt` declares, if it does.
// Cost: O(1).
pub(super) fn note_keyword_declaration(names: &NameScope, stmt: &RakuAstNode) {
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
        names.note_in_list(ListStack::Keywords, name);
    }
}

/// Whether an enclosing statement list redeclares the term `True` / `False`.
// Cost: O(s), s = open scopes.
pub(super) fn is_keyword_shadowed(names: &NameScope, name: &str) -> bool {
    names.in_open_list(ListStack::Keywords, name)
}

/// Install, for the unit `root` while the guard lives, the names it declares
/// that lowering resolves other names against: the constants it redeclares,
/// its sigilless terms, its packages and its routines. One walk of the unit
/// finds all four.
// Cost: O(n), n = nodes in the unit.
pub(super) fn scan<'a>(names: &'a NameScope, root: &RakuAstNode) -> UnitGuard<'a> {
    let mut found = Found::default();
    walk(root, false, &mut found);
    UnitGuard::enter(
        names,
        UnitFacts {
            shadowed: found.shadowed,
            terms: found.terms,
            packages: found.packages,
            routines: found.routines,
        },
    )
}

/// [`scan`] for an executing unit, which is the parser's own statements: the
/// same four facts, read from the statements themselves.
// Cost: O(n), n = nodes in the unit.
#[cfg(test)]
pub(super) fn scan_stmts<'a>(names: &'a NameScope, stmts: &[Stmt]) -> UnitGuard<'a> {
    UnitGuard::enter(names, facts_of(stmts))
}

/// [`scan_stmts`] for a unit finalized in source order (ADR-15760 §7.2 (c)):
/// only its routines are installed up front (a routine is hoisted); each
/// top-level statement adds the rest as it is reached ([`declare_statement`]).
// Cost: O(n), n = nodes in the unit.
pub(super) fn scan_routines<'a>(names: &'a NameScope, stmts: &[Stmt]) -> UnitGuard<'a> {
    let routines = facts_of(stmts).routines;
    UnitGuard::enter(
        names,
        UnitFacts {
            routines,
            ..UnitFacts::default()
        },
    )
}

/// Add what the top-level statement `stmt` declares, other than routines, to
/// the installed unit facts.
// Cost: O(n), n = size of `stmt`.
pub(super) fn declare_statement(names: &NameScope, stmt: &Stmt) {
    names.extend_unit_in_order(facts_of(std::slice::from_ref(stmt)));
}

/// The facts the statements `stmts` declare.
// Cost: O(n), n = nodes of `stmts`.
fn facts_of(stmts: &[Stmt]) -> UnitFacts {
    let mut facts = Facts::default();
    crate::ast_visit::walk_stmts(&mut facts, stmts);
    let Facts {
        terms,
        shadowed,
        packages,
        routines,
    } = facts;
    let mut all_shadowed = terms.clone();
    all_shadowed.extend(shadowed);
    UnitFacts {
        shadowed: all_shadowed,
        terms,
        packages,
        routines,
    }
}

/// What the parser's statements declare that lowering resolves names against:
/// sigilless terms, the names a constant or an enum makes the unit's own, the
/// packages, and the routines (`sub`, `my &name`, a `&name` parameter).
#[derive(Default)]
struct Facts {
    terms: HashSet<String>,
    shadowed: HashSet<String>,
    packages: HashSet<String>,
    routines: HashSet<String>,
}

impl<'ast> Visit<'ast> for Facts {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Ast(node) => {
                if let ParsedStmt::SigillessDecl(source) = node.as_ref() {
                    self.terms.insert(source.name.clone());
                }
            }
            Stmt::SubDecl { name, .. } => {
                self.routines.insert(name.resolve());
            }
            // `my &name` declares a routine of the unit.
            Stmt::VarDecl { name, constant, .. } => {
                if let Some(code) = name.strip_prefix('&') {
                    self.routines.insert(code.to_string());
                }
                if let Some(sigil) = constant {
                    // `constant term:<$box> = ...` declares the term `$box`.
                    // A sigiled constant is the term `term:<$x>`, as the
                    // conversion spells it; the parser stores the bare name.
                    if matches!(sigil, crate::ast::ConstantSigil::Term) {
                        self.terms.insert(name.clone());
                    }
                    self.shadowed.insert(name.clone());
                }
            }
            Stmt::EnumDecl { name, variants, .. } => {
                self.shadowed.insert(name.resolve());
                self.shadowed
                    .extend(variants.iter().map(|(variant, _)| variant.clone()));
            }
            Stmt::ClassDecl { name, .. }
            | Stmt::RoleDecl { name, .. }
            | Stmt::Package { name, .. } => {
                self.packages.insert(name.resolve());
            }
            _ => {}
        }
        crate::ast_visit::walk_stmt(self, stmt);
    }

    fn visit_param(&mut self, param: &'ast crate::ast::ParamDef) {
        // `-> \p { ... }`: a sigilless parameter is a term of its body.
        if param.sigilless {
            self.terms.insert(param.name.clone());
        }
        // A `&name` parameter is a routine of the unit.
        if let Some(name) = param.name.strip_prefix('&') {
            self.routines.insert(name.to_string());
        }
        crate::ast_visit::walk_param(self, param);
    }
}

/// Whether the unit being lowered declares a package named `name`.
/// A declared `class Nil` makes `Nil` that type, not the setting's `Nil` term.
// Cost: O(k), k = length of `name`.
pub(super) fn declares_package(names: &NameScope, name: &str) -> bool {
    names.unit().packages.contains(name)
}

/// Whether the unit being lowered declares a sigilless term `name`.
// Cost: O(k), k = length of `name`.
pub(super) fn is_declared_term(names: &NameScope, name: &str) -> bool {
    names.unit().terms.contains(name)
}

/// Whether `name` is a sigilless value term the unit declares or the parser
/// scope it is lowered in knows (a term imported from a module).
// Cost: O(s), s = open parser scopes.
pub(super) fn is_value_term(names: &NameScope, name: &str) -> bool {
    is_declared_term(names, name) || crate::parser::is_user_declared_value_term(name)
}

/// Whether the unit being lowered declares a term named `name`.
// Cost: O(k), k = length of `name`.
pub(super) fn is_shadowed(names: &NameScope, name: &str) -> bool {
    names.unit().shadowed.contains(name)
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
