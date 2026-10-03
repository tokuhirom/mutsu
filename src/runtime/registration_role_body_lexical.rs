//! The eager lexical-type pass of role declaration: `my class`/`my
//! grammar`/`my enum`/exported `my constant`… declared directly in a role
//! body are registered when the role is declared, not only at composition.

use super::*;

impl Interpreter {
    /// Register every lexical TYPE declaration (`my class`, `my grammar`, `my
    /// role`, `my token`/`rule`/`regex`, `my enum`) declared directly in a
    /// role body when the role is declared, rather than waiting for a later
    /// composition — the type-declaration counterpart of
    /// [`Self::register_role_body_exported_subs`], for the same reason.
    ///
    /// A `my class`/`my grammar` in a role body is, like a plain `sub`, a
    /// lexical declaration of the role's own compilation unit: Rakudo makes
    /// it resolvable the moment the role loads, with no class ever composing
    /// it. `Date::Calendar::Strftime`'s private `_strftime` helper parses
    /// with a `my grammar prt-format {...}` and builds results with a `my
    /// class re-format {...}`, both declared alongside it in the role body —
    /// without this, `prt-format`/`re-format` stayed undeclared (resolving
    /// as a bare `Str`, `No such method 'parse'...`) for exactly the same
    /// reason `_strftime` itself used to be unresolvable (#8950).
    ///
    /// In a **parameterized** role a declaration is run here only when it
    /// does not mention any of the role's parameters
    /// ([`stmt_mentions_role_params`]). A declaration that does (`role
    /// R[::T] { my class C { has T $x } }`) cannot be evaluated before
    /// composition binds `T` to a concrete type — re-evaluating the body with
    /// real bindings is exactly what `run_composed_role_deferred_body` does
    /// at every composition site, and this eager pass must not pre-empt that
    /// with an unbound `T`. One that does not (`unit role V6[::K]; my class
    /// Foo is export { }`, `my constant X is export = 5`) means the same thing
    /// under every parameterization, so, as in a non-parameterized role, it
    /// is declared once here — importable the moment the role loads (#10244)
    /// — and a later composition re-running it is idempotent. Rakudo agrees:
    /// `role R[::T] { my class C {} }` has one `R::C` shared by `R[Int]` and
    /// `R[Str]`; because it already exists when a composition re-runs the
    /// body, the composition-time `R::C[Int]` renaming (reserved for classes
    /// the composition itself creates) leaves it alone.
    ///
    /// Rakudo goes further and declares even a parameter-mentioning
    /// declaration eagerly, *generically* over the unbound `T`; mutsu has no
    /// generic-type representation for that, so such a declaration stays
    /// composition-only.
    ///
    /// Only declarative statement kinds are eagerly run — never a plain
    /// statement or expression, whose side effects must not replay both here
    /// and at composition.
    pub(crate) fn register_role_body_lexical_types(
        &mut self,
        role_name: &str,
        deferred_body_ops: &[crate::opcode::DeferredBodyOp],
        type_params: &[String],
    ) -> Result<(), RuntimeError> {
        let parameterized = !type_params.is_empty();
        let saved_package = self.current_package().to_string();
        self.set_current_package(role_name.to_string());
        // `use`/`need` statements seen so far in the body: they are BEGIN-time,
        // so a nested type declared after one may name the module's role.
        let mut used_modules: Vec<String> = Vec::new();
        for op in deferred_body_ops {
            if let Stmt::Use { module, .. } | Stmt::Need { module } = &op.raw {
                used_modules.push(module.clone());
            }
            let eager = match &op.raw {
                // An exported `my constant`/`my $x` is a compile-time
                // declaration too (#9981).
                Stmt::VarDecl {
                    is_export: true, ..
                }
                | Stmt::EnumDecl { .. }
                | Stmt::ClassDecl { .. }
                | Stmt::RoleDecl { .. }
                | Stmt::TokenDecl { .. }
                | Stmt::SubsetDecl { .. } => {
                    !parameterized || !stmt_mentions_role_params(&op.raw, type_params)
                }
                // A sigilless or bind declaration arrives as the group
                // `role_body_plan` keeps whole.
                Stmt::SyntheticBlock(inner)
                    if crate::ast::scope_members(inner).any(|s| {
                        matches!(
                            s,
                            Stmt::VarDecl {
                                is_export: true,
                                ..
                            }
                        )
                    }) =>
                {
                    !parameterized || !stmt_mentions_role_params(&op.raw, type_params)
                }
                _ => false,
            };
            if !eager {
                continue;
            }
            let loaded = nested_type_parent_names(&op.raw).try_for_each(|parent| {
                self.load_role_body_module_for_parent(&used_modules, &parent)
            });
            if let Err(error) =
                loaded.and_then(|()| self.run_block_raw(std::slice::from_ref(&op.raw)))
            {
                self.set_current_package(saved_package);
                return Err(error);
            }
            // A lexical `my class C is export` carries its tags on an internal
            // marker (see `class_decl`); publish it the way an exported subset is.
            if let Stmt::ClassDecl {
                name,
                custom_traits,
                ..
            } = &op.raw
                && let Some((_, tags)) = custom_traits
                    .iter()
                    .find(|(t, _)| t == "__mutsu_export_type")
                && !self.suppress_exports
            {
                let tags = match tags {
                    Some(Expr::ArrayLiteral(items)) => items
                        .iter()
                        .filter_map(|e| match e {
                            Expr::Literal(v) => Some(v.to_string_value()),
                            _ => None,
                        })
                        .collect(),
                    _ => vec!["DEFAULT".to_string()],
                };
                let (pkg, short) = match crate::qualified::package_parent(*name) {
                    Some(parent) => (
                        parent.resolve().to_string(),
                        crate::qualified::unqualified_part(*name)
                            .resolve()
                            .to_string(),
                    ),
                    None => (role_name.to_string(), name.resolve().to_string()),
                };
                self.register_exported_var(pkg, short, tags);
            }
        }
        self.set_current_package(saved_package);
        Ok(())
    }
}

/// Whether `stmt` mentions any of a parameterized role's parameters
/// (`params`: the bare names of `::T` captures and `$n` value parameters).
///
/// Conservative by construction: every identifier the typed AST visitor
/// (ADR-0137) reports — variable and type names, type constraints (`T:D`,
/// `Array[T]`), source text compiled later — is inspected, and one containing
/// a parameter's name as a whole word counts as a mention. A false positive
/// only defers a declaration to composition (the behavior before #10244).
/// String literals are data, not mentions: `my constant X is export = "T"`
/// does not depend on `T`.
///
/// Cost: O(n), n = size of `stmt`'s AST (times the parameter count).
fn stmt_mentions_role_params(stmt: &Stmt, params: &[String]) -> bool {
    use crate::ast_visit::{NameKind, Visit, contains_word, walk_expr, walk_stmt};

    struct Mentions<'a> {
        params: Vec<&'a str>,
        found: bool,
    }
    impl<'ast> Visit<'ast> for Mentions<'_> {
        fn visit_stmt(&mut self, stmt: &'ast Stmt) {
            if !self.found {
                walk_stmt(self, stmt);
            }
        }
        fn visit_expr(&mut self, expr: &'ast Expr) {
            if !self.found {
                walk_expr(self, expr);
            }
        }
        fn visit_name(&mut self, name: &str, _kind: NameKind) {
            self.found |= self.params.iter().any(|p| contains_word(name, p));
        }
    }

    let mut m = Mentions {
        params: params
            .iter()
            .map(|p| p.as_str())
            .filter(|p| !p.is_empty())
            .collect(),
        found: false,
    };
    m.visit_stmt(stmt);
    m.found
}

/// The `is`/`does` parents a nested class/role declaration names, header
/// clauses and body `also does` alike.
// Cost: O(p + b), p = header parents, b = top-level body statements.
fn nested_type_parent_names(stmt: &Stmt) -> impl Iterator<Item = String> + '_ {
    let (header, body): (Vec<&String>, &[Stmt]) = match stmt {
        Stmt::ClassDecl {
            parents,
            does_parents,
            body,
            ..
        } => (parents.iter().chain(does_parents).collect(), body),
        Stmt::RoleDecl { body, .. } => (Vec::new(), body),
        _ => (Vec::new(), &[]),
    };
    header
        .into_iter()
        .cloned()
        .chain(crate::ast::scope_members(body).filter_map(|s| match s {
            Stmt::DoesDecl { name, .. } => Some(name.resolve()),
            _ => None,
        }))
}
