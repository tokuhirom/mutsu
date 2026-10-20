use crate::ast::{PackageRuntimeDecl, Stmt};

/// One enclosing package a moved phaser re-enters, outermost first.
#[derive(Clone)]
pub(in crate::runtime::begin_prologue) struct Enclosing {
    pub(super) name: crate::symbol::Symbol,
    pub(super) decl: PackageRuntimeDecl,
    pub(super) lexicals: Vec<String>,
    pub(super) our_names: Vec<String>,
}

impl Enclosing {
    /// The package a class or brace-scoped package declaration declares, with
    /// the `my` lexicals its body shares through the package's static store.
    /// `None` for any other statement, a `unit` declarator, and a class whose
    /// name is computed.
    // Cost: O(n), n = size of the body's top-level statement list.
    pub(in crate::runtime::begin_prologue) fn of_decl(stmt: &Stmt) -> Option<(Enclosing, &[Stmt])> {
        let (name, decl, body) = match stmt {
            Stmt::ClassDecl {
                name,
                name_expr: None,
                is_unit: false,
                is_lexical,
                decl_id,
                body,
                ..
            } => (
                *name,
                PackageRuntimeDecl::Class {
                    is_lexical: *is_lexical,
                    decl_id: *decl_id,
                },
                body,
            ),
            Stmt::Package {
                name,
                is_unit: false,
                body,
                ..
            } => (*name, PackageRuntimeDecl::Package, body),
            _ => return None,
        };
        let mut lexicals: Vec<String> = crate::compiler::Compiler::package_body_lexical_names(body)
            .into_iter()
            .collect();
        lexicals.sort();
        let mut our_names: Vec<String> = crate::compiler::Compiler::package_body_our_names(body)
            .into_iter()
            .collect();
        our_names.sort();
        Some((
            Enclosing {
                name,
                decl,
                lexicals,
                our_names,
            },
            body,
        ))
    }

    /// The `my` lexicals the package's static store holds.
    pub(in crate::runtime::begin_prologue) fn lexicals(&self) -> &[String] {
        &self.lexicals
    }

    /// `body`, run inside this package: [`Stmt::PackageRuntimeBody`] re-enters
    /// the package and binds its lexicals from the store.
    pub(in crate::runtime::begin_prologue) fn run_in(&self, body: Vec<Stmt>) -> Vec<Stmt> {
        vec![Stmt::PackageRuntimeBody {
            name: self.name,
            body,
            lexicals: self.lexicals.clone(),
            our_names: self.our_names.clone(),
            decl: self.decl,
        }]
    }
}
