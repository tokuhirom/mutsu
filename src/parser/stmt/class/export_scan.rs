//! The `is export` declarations of an inline package body (`module M { }`,
//! `class C { }`), read with the typed AST visitor (ADR-0137).
//!
//! Rakudo's `is export` trait exports from any depth of the package's lexical
//! region: a routine declared in a nested block, a routine body, a nested
//! `module` or a nested `class` lands in the outer package's export list too
//! (`module M { sub f { sub g is export { } } }; import M; g()` works), and two
//! non-`multi` declarations of the same symbol anywhere in it clash. So every
//! scan here searches every position of the body.

use crate::ast::Stmt;
use crate::ast_visit::{Visit, walk_stmt, walk_stmts};
use crate::parser::stmt::simple::InlineModuleExportSpec;
use std::collections::HashSet;

/// The exported `sub`s, `token`s and `rule`s of a package body, so `import`
/// can register them at parse time.
// Cost: O(n), n = size of the AST of `stmts`.
pub(crate) fn extract_exported_subs(stmts: &[Stmt]) -> Vec<InlineModuleExportSpec> {
    let mut scan = ExportedSubs(Vec::new());
    walk_stmts(&mut scan, stmts);
    scan.0
}

struct ExportedSubs(Vec<InlineModuleExportSpec>);

impl<'ast> Visit<'ast> for ExportedSubs {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::SubDecl {
                name,
                is_export: true,
                precedence_trait,
                associativity,
                ..
            } => {
                self.0.push((
                    name.to_string(),
                    precedence_trait.clone(),
                    associativity.clone(),
                ));
            }
            // `token foo is export` / `rule foo is export` export a Regex under
            // `&foo`, so they are importable names just like an exported sub.
            Stmt::TokenDecl {
                name,
                is_export: true,
                ..
            }
            | Stmt::RuleDecl {
                name,
                is_export: true,
                ..
            } => self.0.push((name.to_string(), None, None)),
            _ => {}
        }
        walk_stmt(self, stmt);
    }
}

/// The `is export` *operator methods* (`method prefix:<~> is export`,
/// `method infix:<as> is export`, ...) of a class/role body. `import
/// ClassName` exposes them as operator subs, so the parser must learn the new
/// operator symbols (e.g. `as` becomes a known infix) when the `import`
/// statement is parsed.
// Cost: O(n), n = size of the AST of `stmts`.
pub(crate) fn extract_exported_operator_methods(stmts: &[Stmt]) -> Vec<InlineModuleExportSpec> {
    let mut scan = ExportedOperatorMethods(Vec::new());
    walk_stmts(&mut scan, stmts);
    scan.0
}

struct ExportedOperatorMethods(Vec<InlineModuleExportSpec>);

impl<'ast> Visit<'ast> for ExportedOperatorMethods {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::MethodDecl {
            name,
            is_export: true,
            ..
        } = stmt
        {
            let resolved = name.resolve();
            if is_operator_categorical_name(&resolved) {
                self.0.push((resolved.to_string(), None, None));
            }
        }
        walk_stmt(self, stmt);
    }
}

/// True for a categorical operator declaration name (`prefix:<...>`,
/// `infix:<...>`, `postfix:<...>`, `circumfix:<...>`, `postcircumfix:<...>`).
fn is_operator_categorical_name(name: &str) -> bool {
    const CATEGORIES: &[&str] = &[
        "prefix:",
        "postfix:",
        "infix:",
        "circumfix:",
        "postcircumfix:",
    ];
    CATEGORIES.iter().any(|c| name.starts_with(c)) && name.ends_with('>')
}

/// The first symbol exported more than once from a package body. In Raku two
/// `is export` declarations of the same symbol within one package raise
/// X::Export::NameClash at compile time.
// Cost: O(n), n = size of the AST of `stmts`.
pub(crate) fn find_export_name_clash(stmts: &[Stmt]) -> Option<String> {
    let mut scan = ExportNameClash {
        seen: HashSet::new(),
        clash: None,
    };
    walk_stmts(&mut scan, stmts);
    scan.clash
}

struct ExportNameClash {
    seen: HashSet<String>,
    clash: Option<String>,
}

impl<'ast> Visit<'ast> for ExportNameClash {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.clash.is_some() {
            return;
        }
        // `multi` candidates legitimately share a name, so only a non-multi
        // (`only`) exported sub can clash.
        if let Stmt::SubDecl {
            name,
            is_export: true,
            multi: false,
            ..
        } = stmt
            && !self.seen.insert(name.to_string())
        {
            self.clash = Some(name.to_string());
            return;
        }
        walk_stmt(self, stmt);
    }
}
