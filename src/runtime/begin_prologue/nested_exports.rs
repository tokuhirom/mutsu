//! Compile-time export of the `constant`, `enum` and lexical-class
//! declarations nested in code (#10543).
//!
//! Rakudo applies `is export` while it compiles the declaration, wherever the
//! declaration sits, so an importer sees
//!
//! ```raku
//! unit module PC;
//! sub g { constant kk is export = 5 }
//! sub h { enum EX is export <ex1 ex2> }
//! ```
//!
//! as exporting `kk`, `EX`, `ex1` and `ex2` although neither routine ever
//! runs. mutsu registers such a declaration where it textually sits, i.e.
//! only when the enclosing routine (or block) runs, which is after the
//! importer copied the export table, or never.
//!
//! [`lift_nested_exports`] gives each such declaration a compile-time
//! registration: a copy of it, in a `BEGIN` block placed just before the
//! statement of the enclosing declaration scope (the unit, or a package body)
//! that contains it. The BEGIN prologue (ADR-0134) then runs it with the
//! scope's other BEGIN-time effects, in source order, in the package the
//! declaration belongs to. The `BEGIN` block keeps the copy's own lexical name
//! out of the enclosing scope. The declaration itself stays in place and
//! re-registers the same symbols when its code runs.
//!
//! A declaration whose initializer names something its enclosing code
//! declares (a parameter, a `my` variable, another nested declaration that is
//! not lifted) is not lifted: at BEGIN time that name does not exist yet.
//! Rakudo would read its static (undefined) value instead.
//! TODO: give such a declaration the static-cell treatment the nested BEGIN
//! lift uses (`nested.rs`), instead of leaving it to run time.

use crate::ast::{PhaserKind, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_stmt};
use std::collections::HashSet;

/// Insert the compile-time registrations of every exported declaration nested
/// in code into `stmts` (a unit's top level) and into the package bodies it
/// declares, as described in the module docs.
// Cost: O(n), n = size of the AST under `stmts` (plus the copies).
pub(super) fn lift_nested_exports(stmts: &mut Vec<Stmt>) {
    // Every insertion: the index path of the declaration scope, the index of
    // the statement the copies go before, and the copies.
    let mut inserts: Vec<(Vec<usize>, Vec<Stmt>)> = Vec::new();
    let mut scopes: Vec<(Vec<usize>, &[Stmt])> = vec![(Vec::new(), stmts.as_slice())];
    while let Some((path, list)) = scopes.pop() {
        for (index, stmt) in list.iter().enumerate() {
            let mut at = path.clone();
            at.push(index);
            if let Some(body) = scope_body(stmt) {
                scopes.push((at, body));
                continue;
            }
            if !holds_code(stmt) {
                continue;
            }
            let copies = nested_exported_decls(stmt);
            if !copies.is_empty() {
                inserts.push((at, copies));
            }
        }
    }
    // Deepest and last first, so an insertion never shifts a path that is
    // still to be applied.
    inserts.sort_by(|a, b| b.0.cmp(&a.0));
    'insert: for (at, copies) in inserts {
        let Some((&index, scope_path)) = at.split_last() else {
            continue;
        };
        let mut list: &mut Vec<Stmt> = stmts;
        for &step in scope_path {
            // An insertion path only steps through scope bodies.
            let Some(body) = scope_body_mut(&mut list[step]) else {
                continue 'insert;
            };
            list = body;
        }
        list.insert(
            index,
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                body: copies,
                condition: None,
                end_index: None,
            },
        );
    }
}

/// The statement list of a declaration scope the lift descends into: a
/// brace-scoped package body, or a transparent statement group.
// Cost: O(1).
fn scope_body(stmt: &Stmt) -> Option<&[Stmt]> {
    match stmt {
        Stmt::Package {
            body,
            is_unit: false,
            ..
        }
        | Stmt::SyntheticBlock(body) => Some(body),
        _ => None,
    }
}

/// [`scope_body`], mutably.
// Cost: O(1).
fn scope_body_mut(stmt: &mut Stmt) -> Option<&mut Vec<Stmt>> {
    match stmt {
        Stmt::Package {
            body,
            is_unit: false,
            ..
        }
        | Stmt::SyntheticBlock(body) => Some(body),
        _ => None,
    }
}

/// Whether a statement of a declaration scope is code whose nested
/// declarations the lift looks for. A type body is a package of its own,
/// and a `BEGIN` already runs at compile time.
// Cost: O(1).
fn holds_code(stmt: &Stmt) -> bool {
    !matches!(
        stmt,
        Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::EnumDecl { .. }
            | Stmt::Package { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            }
    )
}

/// The copies of the exported declarations nested in `stmt`, in source order,
/// leaving out the ones that name something `stmt` declares.
// Cost: O(n), n = size of `stmt`'s subtree (plus the name sets of each
// declaration found).
fn nested_exported_decls(stmt: &Stmt) -> Vec<Stmt> {
    // Only what `stmt` nests: `stmt` itself registers where it stands.
    let mut finder = ExportedDecls::default();
    walk_stmt(&mut finder, stmt);
    if finder.found.is_empty() {
        return Vec::new();
    }
    let declared = names_of(stmt, is_declaration);
    let mut lifted: HashSet<String> = HashSet::new();
    let mut copies = Vec::new();
    for decl in finder.found {
        let own = names_of(&decl, is_declaration);
        let blocked = names_of(&decl, is_reference).into_iter().any(|name| {
            declared.contains(&name) && !own.contains(&name) && !lifted.contains(&name)
        });
        if blocked {
            continue;
        }
        lifted.extend(own);
        copies.push(decl);
    }
    copies
}

/// Collects the exported declarations nested in code: a `constant`, an
/// `enum`, and a lexical class (an `our` class is already installed at
/// compile time, by its nested type shell).
#[derive(Default)]
struct ExportedDecls {
    found: Vec<Stmt>,
}

impl Visit for ExportedDecls {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl {
                is_export: true,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(t, _)| t == "__constant") => {
                self.found.push(stmt.clone());
            }
            Stmt::EnumDecl {
                is_export: true, ..
            } => self.found.push(stmt.clone()),
            Stmt::ClassDecl {
                is_lexical: true,
                name_expr: None,
                custom_traits,
                ..
            } if custom_traits
                .iter()
                .any(|(t, _)| t == "__mutsu_export_type") =>
            {
                self.found.push(stmt.clone());
            }
            // Another package's body, or code that already runs at BEGIN time.
            Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::EnumDecl { .. }
            | Stmt::Package { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            } => {}
            _ => walk_stmt(self, stmt),
        }
    }
}

/// A name position that declares a name.
// Cost: O(1).
fn is_declaration(kind: NameKind) -> bool {
    matches!(
        kind,
        NameKind::VarDecl
            | NameKind::Param
            | NameKind::BlockParam
            | NameKind::SubDecl
            | NameKind::Decl
    )
}

/// A name position that refers to a declared name.
// Cost: O(1).
fn is_reference(kind: NameKind) -> bool {
    matches!(
        kind,
        NameKind::Var
            | NameKind::ArrayVar
            | NameKind::HashVar
            | NameKind::CodeVar
            | NameKind::Term
            | NameKind::Call
            | NameKind::UserRoutineCall
            | NameKind::Type
    )
}

/// The names reported at positions `keep` accepts anywhere in `stmt`, with
/// their sigils and twigils stripped, so a declaration and a reference of the
/// same name compare equal whatever form each position reports.
// Cost: O(n), n = size of `stmt`'s subtree.
fn names_of(stmt: &Stmt, keep: fn(NameKind) -> bool) -> HashSet<String> {
    struct Names {
        keep: fn(NameKind) -> bool,
        out: HashSet<String>,
    }
    impl Visit for Names {
        fn visit_name(&mut self, name: &str, kind: NameKind) {
            if (self.keep)(kind) {
                self.out.insert(
                    name.trim_start_matches(['$', '@', '%', '&', '!', '.', '*', '?', '^', ':'])
                        .to_string(),
                );
            }
        }
    }
    let mut names = Names {
        keep,
        out: HashSet::new(),
    };
    names.visit_stmt(stmt);
    names.out
}
