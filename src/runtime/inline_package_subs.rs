//! The CHECK-time prepass that installs a package's routines before the
//! package body runs (#10504).
//!
//! Rakudo installs an `our sub` declared in a package when the compunit is
//! compiled, wherever the package or the sub sits: at the top of the unit, in
//! a never-run branch, inside an uncalled routine, or in a nested block of the
//! package body. [`InlinePackageSubCollector`] finds those declarations and
//! [`Interpreter::preregister_inline_package_subs`] registers them.

use super::*;
use crate::ast_visit::{Visit, walk_stmt, walk_stmts};
use crate::runtime::meta_ns::MetaNs;

impl Interpreter {
    /// Register the routines declared by an inline package before CHECK
    /// phasers run.  Raku makes an inline package's interface available during
    /// compilation, but keeps the package body's procedural statements in
    /// their normal runtime order.  `reorder_phasers` therefore places a
    /// top-level `module` after CHECK; without this declaration-only pass an
    /// import from CHECK sees neither the package's routines nor its exports.
    ///
    /// This deliberately compiles only routine bodies.  Executing the package
    /// body here would change observable ordering (for example, a `say` in the
    /// module must still happen after CHECK), while the routine registry and
    /// export table are the compile-time interface needed by `import`.
    pub(crate) fn preregister_inline_package_subs(
        &mut self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut collector = InlinePackageSubCollector::default();
        crate::ast_visit::walk_stmts(&mut collector, stmts);
        let declarations = collector.out;

        // Protos first: a candidate's `our`-scope check reads `proto_subs`.
        for InlinePackageDecl { package, stmt, .. } in &declarations {
            let Stmt::ProtoDecl {
                name,
                params,
                param_defs,
                return_type,
                body,
                is_method,
                is_our,
                ..
            } = stmt
            else {
                continue;
            };
            // A `proto method` is a METHOD-level proto (the class method table
            // owns it), never a package proto sub -- same exclusion the
            // `RegisterProtoSub` opcode makes.
            if *is_method {
                continue;
            }
            let name_str = name.resolve();
            let saved_package = self.current_package();
            self.set_current_package(package.clone());
            let result = self.register_proto_decl(
                &name_str,
                params,
                param_defs,
                return_type.as_ref(),
                body,
                *is_our,
                None,
                false,
            );
            // Tell the in-sequence `RegisterProtoSub` that this key's proto is
            // its OWN prepass registration and not a redeclaration -- the same
            // protocol `__mutsu_inline_package_sub_preregistered` already uses
            // for the candidates. Only set when the prepass actually installed
            // one, so a genuine duplicate `our proto` in the body still errors.
            if result.is_ok() {
                self.env.insert(
                    MetaNs::InlinePackageProto.owned_key_pair_for_strs(package, &name_str),
                    Value::TRUE,
                );
            }
            self.set_current_package(saved_package);
            // A declaration-only prepass must not turn a body-level problem
            // into a hard failure: leave any error to the in-sequence
            // registration, which has the real compiled routine.
            drop(result);
        }

        for InlinePackageDecl {
            package,
            stmt,
            nested,
            outer_packages,
        } in declarations
        {
            let Stmt::SubDecl {
                name,
                params,
                param_defs,
                return_type,
                associativity,
                signature_alternates,
                body,
                multi,
                is_rw,
                is_raw,
                is_export,
                export_tags,
                is_test_assertion,
                supersede,
                custom_traits,
                ..
            } = stmt
            else {
                // A `ProtoDecl` from the same collector; handled in the pass above.
                continue;
            };

            // Declaration-only registration cannot safely apply a user trait:
            // its argument may need the package body (or a preceding `use`) to
            // have run. Leave those declarations to the normal package-body
            // registration pass. Built-in/internal traits below are fully
            // represented by the metadata passed to the registrar.
            if custom_traits.iter().any(|(trait_name, _)| {
                !trait_name.starts_with("__")
                    && trait_name != "default"
                    && !trait_name.starts_with("DEPRECATED")
                    && trait_name != "hidden-from-USAGE"
            }) {
                continue;
            }

            let saved_package = self.current_package();
            self.set_current_package(package.clone());

            let site_fingerprint = crate::ast::sub_registration_fingerprint(
                &params,
                &param_defs,
                &body,
                return_type.as_ref(),
                multi,
                is_rw,
                is_raw,
            );

            let metadata = crate::opcode::compiled_routine_metadata(
                &params,
                &param_defs,
                &body,
                return_type.as_ref(),
                is_rw,
                is_raw,
            );
            let compiled =
                self.compile_forward_declared_sub(name, &params, &param_defs, &body, is_rw, is_raw);
            let traits: Vec<(String, Option<crate::opcode::DeclTraitArg>)> = custom_traits
                .iter()
                .filter(|(trait_name, _)| {
                    trait_name.starts_with("__")
                        || *trait_name == "default"
                        || trait_name.starts_with("DEPRECATED")
                        || *trait_name == "hidden-from-USAGE"
                })
                .map(|(trait_name, _)| (trait_name.clone(), None))
                .collect();
            let result = (|| {
                let result = self.register_compiled_sub_decl(
                    &name.resolve(),
                    &params,
                    &param_defs,
                    return_type.as_ref(),
                    associativity.as_ref(),
                    &[],
                    multi,
                    is_rw,
                    is_raw,
                    is_test_assertion,
                    supersede,
                    &traits,
                    Some(site_fingerprint),
                    &metadata,
                    Some(&compiled),
                );

                if matches!(
                    &result,
                    Ok(crate::runtime::registration_sub::SubRegisterOutcome::Installed)
                ) {
                    if is_export && !self.suppress_exports {
                        // Rakudo's `is export` installs the symbol into the
                        // EXPORT package of every package lexically enclosing
                        // the declaration, not just the innermost one.
                        self.register_exported_sub(
                            package.clone(),
                            name.resolve(),
                            export_tags.clone(),
                        );
                        for outer in &outer_packages {
                            self.export_nested_sub_into(
                                outer,
                                &package,
                                &name.resolve(),
                                &export_tags,
                            );
                        }
                    }
                    if multi && !self.suppress_exports {
                        self.refresh_exported_multi_family(&name.resolve());
                    }
                    for (alt_params, alt_param_defs) in &signature_alternates {
                        let alt_metadata = crate::opcode::compiled_routine_metadata(
                            alt_params,
                            alt_param_defs,
                            &body,
                            return_type.as_ref(),
                            is_rw,
                            is_raw,
                        );
                        let alt_compiled = self.compile_forward_declared_sub(
                            name,
                            alt_params,
                            alt_param_defs,
                            &body,
                            is_rw,
                            is_raw,
                        );
                        self.register_sub_alternate_decl(
                            &name.resolve(),
                            alt_params,
                            alt_param_defs,
                            return_type.as_ref(),
                            associativity.as_ref(),
                            &[],
                            multi,
                            is_rw,
                            is_raw,
                            is_test_assertion,
                            supersede,
                            &traits,
                            Some(&alt_metadata),
                            Some(&alt_compiled),
                        )?;
                    }
                }
                result
            })();

            let installed = matches!(
                &result,
                Ok(crate::runtime::registration_sub::SubRegisterOutcome::Installed)
            );
            self.set_current_package(saved_package);
            // A routine nested below the package body's own statement list
            // closes over a block (or routine) that has not run yet, so the
            // in-sequence `RegisterDecl` must still install it, with the
            // declaring activation's bindings: no "already installed" marker.
            if installed && !nested {
                self.env_mut().insert(
                    MetaNs::InlinePackageSub.owned_key_from_parts(&[
                        &package,
                        &name.resolve(),
                        &site_fingerprint.to_string(),
                    ]),
                    Value::TRUE,
                );
            }
            result?;
        }
        Ok(())
    }
}

impl Interpreter {
    /// Export the routine `name`, registered in package `source`, from the
    /// lexically enclosing package `outer` too, under `tags` (and `ALL`).
    ///
    /// Rakudo's `is export` installs the symbol into the EXPORT package of
    /// every package enclosing the declaration, so `module M { module N {
    /// sub g is export { } } }; import M` imports `g` (#10543). `import`
    /// finds the routine through `outer`'s `EXPORT::ALL` alias, since no
    /// `outer::name` routine exists.
    ///
    /// Cost: O(f), f = registered routines (the scan for `name`'s multi
    /// candidates).
    fn export_nested_sub_into(&mut self, outer: &str, source: &str, name: &str, tags: &[String]) {
        let mut tags = tags.to_vec();
        if tags.is_empty() {
            tags.push("DEFAULT".to_string());
        }
        let name_sym = Symbol::intern(name);
        let single = crate::qualified::qualified(Symbol::intern(source), name_sym);
        let prefix = format!("{}/", single.as_str());
        let entries: Vec<(Option<String>, Arc<FunctionDef>)> = self
            .registry()
            .functions
            .iter()
            .filter_map(|(key, def)| {
                if *key == single {
                    return Some((None, def.clone()));
                }
                key.as_str()
                    .strip_prefix(&prefix)
                    .map(|suffix| (Some(suffix.to_string()), def.clone()))
            })
            .collect();
        let export_pkg =
            crate::qualified::qualified(Symbol::intern(outer), Symbol::intern("EXPORT"));
        for tag in tags
            .iter()
            .map(String::as_str)
            .chain(std::iter::once("ALL"))
        {
            let tag_pkg = crate::qualified::qualified(export_pkg, Symbol::intern(tag));
            for (suffix, def) in &entries {
                let member = match suffix {
                    Some(suffix) => Symbol::intern(&format!("{name}/{suffix}")),
                    None => name_sym,
                };
                self.registry_mut()
                    .functions_mut()
                    .entry(crate::qualified::qualified(tag_pkg, member))
                    .or_insert_with(|| def.clone());
            }
        }
        let entry = crate::runtime::cow_table_mut(&mut self.exported_subs)
            .entry(outer.to_string())
            .or_default()
            .entry(name.to_string())
            .or_default();
        entry.extend(tags);
    }
}

/// One routine declaration the prepass registers.
struct InlinePackageDecl {
    /// The fully-qualified package the routine is installed in.
    package: String,
    /// The `Stmt::SubDecl` / `Stmt::ProtoDecl`.
    stmt: Stmt,
    /// Declared below the package body's own statement list -- in a nested
    /// block, a branch, a loop or a routine body -- rather than directly in it
    /// (through `SyntheticBlock`s and nested packages only).
    nested: bool,
    /// The packages lexically enclosing `package`, outermost first. An
    /// exported routine is exported from each of them too.
    outer_packages: Vec<String>,
}

/// Finds the routines [`Interpreter::preregister_inline_package_subs`]
/// installs.
///
/// Directly in a brace-scoped package body, every `sub` / `proto` is collected
/// (a plain `sub` there is callable from the whole body, before its textual
/// position). Below that -- in a nested block, a branch, a loop, a routine
/// body, or a package that itself sits in one -- only an `our` or an exported
/// routine is: a lexical one is visible to its own block alone, and that block
/// runs it in sequence, but `is export` puts the routine into the package's
/// EXPORT stash at compile time, wherever it sits (#10543).
///
/// An `enum` body is never entered. A `class` body inside a package is
/// entered for its exported routines only: Rakudo exports them from the class
/// and from every package enclosing it.
#[derive(Default)]
struct InlinePackageSubCollector {
    /// The enclosing brace-scoped packages, outermost first; empty outside
    /// every package.
    packages: Vec<String>,
    /// Below the statement list of the unit or of the enclosing package.
    nested: bool,
    /// Inside a class body: only exported routines are collected.
    exports_only: bool,
    out: Vec<InlinePackageDecl>,
}

impl InlinePackageSubCollector {
    // Cost: O(n), n = size of `body`'s subtree.
    fn in_package(&mut self, name: Symbol, body: &[Stmt], exports_only: bool) {
        let name = name.resolve();
        let package = if let Some(absolute) = name.strip_prefix("GLOBAL::") {
            absolute.to_string()
        } else {
            match self.packages.last() {
                Some(parent) => {
                    crate::qualified::qualified(Symbol::intern(parent), Symbol::intern(&name))
                        .resolve()
                }
                None => name,
            }
        };
        self.packages.push(package);
        let saved = std::mem::replace(&mut self.exports_only, exports_only);
        walk_stmts(self, body);
        self.exports_only = saved;
        self.packages.pop();
    }

    fn collect(&mut self, stmt: &Stmt, is_our: bool, is_export: bool) {
        let Some((package, outer)) = self.packages.split_last() else {
            return;
        };
        if self.exports_only && !is_export {
            return;
        }
        if self.nested && !is_our && !is_export {
            return;
        }
        self.out.push(InlinePackageDecl {
            package: package.clone(),
            stmt: stmt.clone(),
            // A class body's routine is registered by the class itself.
            nested: self.nested || self.exports_only,
            outer_packages: outer.to_vec(),
        });
    }

    // Cost: O(n), n = size of `stmt`'s subtree.
    fn walk_nested(&mut self, stmt: &Stmt) {
        let saved = std::mem::replace(&mut self.nested, true);
        walk_stmt(self, stmt);
        self.nested = saved;
    }
}

impl<'ast> Visit<'ast> for InlinePackageSubCollector {
    // Cost: O(n), n = size of `stmt`'s subtree.
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Package {
                name,
                body,
                is_unit: false,
                is_my: false,
                ..
            } => self.in_package(*name, body, false),
            // The run-time half of a package body the BEGIN prologue split
            // off (ADR-0134 §7): its bare statements, at the package body's
            // own level. The declaration half is visited where the prologue
            // keeps it.
            Stmt::PackageRuntimeBody {
                name,
                body,
                decl: crate::ast::PackageRuntimeDecl::Package,
                ..
            } => self.in_package(*name, body, false),
            // A named, package-scoped class (at the top of the unit or inside
            // a package): only its exported routines, which the enclosing
            // packages export too.
            Stmt::ClassDecl {
                name,
                body,
                name_expr: None,
                is_lexical: false,
                is_unit: false,
                ..
            } => {
                let saved = std::mem::replace(&mut self.nested, false);
                self.in_package(*name, body, true);
                self.nested = saved;
            }
            // The run-time half of a package-scoped class body the BEGIN
            // prologue split off: a routine nested in one of its bare
            // statements (`class K { if True { sub g is export { } } }`) lives
            // only here, and is exported from the class like a direct one.
            Stmt::PackageRuntimeBody {
                name,
                body,
                decl:
                    crate::ast::PackageRuntimeDecl::Class {
                        is_lexical: false, ..
                    },
                ..
            } => {
                let saved = std::mem::replace(&mut self.nested, false);
                self.in_package(*name, body, true);
                self.nested = saved;
            }
            // A `my package`, a `unit package` and any other class-like body
            // are packages this pass does not install into.
            Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::EnumDecl { .. }
            | Stmt::AugmentClass { .. } => {}
            Stmt::SyntheticBlock(inner) => walk_stmts(self, inner),
            Stmt::SubDecl {
                custom_traits,
                is_export,
                ..
            } => {
                let is_our = custom_traits.iter().any(|(t, _)| t == "__our_scoped");
                self.collect(stmt, is_our, *is_export);
                self.walk_nested(stmt);
            }
            Stmt::ProtoDecl {
                is_our, is_export, ..
            } => {
                // A class body's proto is left to the class registration:
                // the prepass's proto marker would make it skip its own.
                if !self.exports_only {
                    self.collect(stmt, *is_our, *is_export);
                }
                self.walk_nested(stmt);
            }
            _ => self.walk_nested(stmt),
        }
    }
}
