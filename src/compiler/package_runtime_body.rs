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
        our_names: &[String],
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
            decls_idx: u32::MAX,
            own_slots_idx: u32::MAX,
            slots_idx: u32::MAX,
        });
        let locals_from = self.code.locals.len();
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
            .chain(our_names)
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
                self.compile_stmt_discarding_value(s);
            }
        }
        self.local_map.extend(shadowed);
        let declared: Vec<&str> = crate::ast::scope_members(body)
            .filter_map(|stmt| match stmt {
                Stmt::VarDecl { name, .. } => Some(name.as_str()),
                _ => None,
            })
            .collect();
        self.set_package_scope_own_slots(pkg_idx, locals_from, declared);
        self.package_body_lexicals = saved_lexicals;
        self.current_package = saved_package;
        self.in_unit_package = saved_in_unit;
        self.current_package_kind = saved_package_kind;
        self.code.patch_body_end(pkg_idx);
    }

    /// Record on the `PackageScope` at `pkg_idx` the local slots its body
    /// binds itself (`own_slots_idx`): every slot allocated since
    /// `locals_from`, and every slot named by one of `declared`, the names the
    /// body's own scope declares (a body `my`/`our` reuses a same-named outer
    /// slot). Its exit restores at least these (#9171).
    // Cost: O(r + d * L), r = slots allocated by the body, d = declared names,
    // L = locals. Once per compiled package body.
    pub(super) fn set_package_scope_own_slots<'a>(
        &mut self,
        pkg_idx: usize,
        locals_from: usize,
        declared: impl IntoIterator<Item = &'a str>,
    ) {
        let mut own: Vec<usize> = (locals_from..self.code.locals.len()).collect();
        for name in declared {
            own.extend(
                self.code.locals[..locals_from]
                    .iter()
                    .enumerate()
                    .filter(|(_, local)| local.as_str() == name)
                    .map(|(slot, _)| slot),
            );
        }
        if own.is_empty() {
            return;
        }
        let idx = self.code.add_constant(Value::array(
            own.into_iter()
                .map(|slot| Value::int(slot as i64))
                .collect::<Vec<_>>(),
        ));
        if let OpCode::PackageScope { own_slots_idx, .. } = &mut self.code.ops[pkg_idx] {
            *own_slots_idx = idx;
        }
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
