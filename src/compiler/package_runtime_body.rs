//! Compiling a [`Stmt::PackageRuntimeBody`]: the run-time part of a class or
//! package body whose declaration the BEGIN prologue moved ahead (ADR-0134
//! §7, #10332).

use super::*;
use crate::ast::PackageRuntimeDecl;

/// The env name of `$?CLASS`, which the run-time part of a class body binds
/// to the class (see `Interpreter::bind_package_body_lexicals`).
pub(crate) const CLASS_LEXICAL: &str = "?CLASS";

impl Compiler {
    /// Re-enter the declared package and run its body's run-time statements.
    /// This is the same `PackageScope` a `package Foo { ... }` body runs in:
    /// the package is current, so the body's lexicals resolve through the
    /// package's static store as they do from its methods and subs.
    pub(super) fn compile_package_runtime_body(
        &mut self,
        name: Symbol,
        body: &[Stmt],
        lexicals: &[String],
        decl: PackageRuntimeDecl,
    ) {
        let qualified_name = match decl {
            PackageRuntimeDecl::Class {
                is_lexical,
                decl_id,
            } => self.qualified_class_decl_name(&name.resolve(), is_lexical, decl_id),
            PackageRuntimeDecl::Package => self.qualify_package_name(&name.resolve()),
        };
        let name_idx = self.code.add_constant(Value::str(qualified_name.clone()));
        // `$?CLASS` is a lexical of a class body too.
        let mut bound: Vec<&str> = lexicals.iter().map(String::as_str).collect();
        if matches!(decl, PackageRuntimeDecl::Class { .. }) {
            bound.push(CLASS_LEXICAL);
        }
        let lexicals_idx = if bound.is_empty() {
            crate::opcode::NO_PACKAGE_LEXICALS
        } else {
            self.code.add_constant(Value::str(bound.join("\n")))
        };
        let pkg_idx = self.code.emit(OpCode::PackageScope {
            name_idx,
            body_end: 0,
            lexicals_idx,
        });
        let saved_package = std::mem::replace(&mut self.current_package, qualified_name);
        let saved_in_unit = std::mem::replace(&mut self.in_unit_package, false);
        let saved_package_kind = self.current_package_kind.take();
        let saved_lexicals = std::mem::replace(
            &mut self.package_body_lexicals,
            lexicals.iter().cloned().collect(),
        );
        // The body's own lexicals shadow a same-named outer lexical: hide the
        // outer one's slot while the body compiles.
        let shadowed: Vec<(String, u32)> = lexicals
            .iter()
            .filter_map(|name| self.local_map.remove_entry(name))
            .collect();
        for s in body {
            // A class body registered in place swallows the error of an `EVAL`
            // or `BEGIN` statement so the class still registers
            // (`class C { EVAL q[has $.w] }`, `register_class_decl`). The
            // statements that stay behind when the prologue declares the class
            // ahead keep that.
            if matches!(decl, PackageRuntimeDecl::Class { .. })
                && crate::opcode::is_swallowable_class_body_stmt(s)
            {
                let swallowed = Stmt::Expr(Expr::Try {
                    body: vec![s.clone()],
                    catch: None,
                });
                self.compile_stmt(&swallowed);
            } else {
                self.compile_stmt(s);
            }
        }
        self.local_map.extend(shadowed);
        self.package_body_lexicals = saved_lexicals;
        self.current_package = saved_package;
        self.in_unit_package = saved_in_unit;
        self.current_package_kind = saved_package_kind;
        self.code.patch_body_end(pkg_idx);
    }

    /// The `my` lexicals a class or package body declares at its top level,
    /// for [`Compiler::package_body_lexicals`].
    pub(crate) fn package_body_lexical_names(body: &[Stmt]) -> HashSet<String> {
        let mut names = HashSet::new();
        for stmt in crate::ast::scope_members(body) {
            match stmt {
                Stmt::VarDecl {
                    name,
                    is_our: false,
                    is_dynamic: false,
                    custom_traits,
                    ..
                } if !name.starts_with('&')
                    && !custom_traits.iter().any(|(t, _)| t == "__constant") =>
                {
                    names.insert(name.clone());
                }
                _ => {}
            }
        }
        names
    }

    /// The names a package body declares `our` at its own top level, which
    /// stay package variables there even when an enclosing lexical of the
    /// same name is in scope.
    // Cost: O(n), n = the body's top-level statements.
    pub(crate) fn package_body_our_names(body: &[Stmt]) -> HashSet<String> {
        crate::ast::scope_members(body)
            .filter_map(|stmt| match stmt {
                Stmt::VarDecl {
                    name, is_our: true, ..
                } => Some(name.clone()),
                _ => None,
            })
            .collect()
    }
}
