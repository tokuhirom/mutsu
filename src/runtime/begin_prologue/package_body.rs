//! Splitting a package body the BEGIN prologue moves ahead (ADR-0134 §7,
//! slice 1 residue, #10332).
//!
//! Rakudo composes a `class` (or a `module`/`package`) at BEGIN time, but it
//! runs the bare statements of the body at run time, in the body's source
//! position:
//!
//! ```raku
//! say 1; class B { say 2 }; BEGIN say 3    # 3 1 2
//! ```
//!
//! So a declaration the prologue takes is split in two. The prologue keeps
//! the declaration with its BEGIN-time part: every declarator (attributes,
//! methods, subs, nested types, `use`, phasers) and the static half of each
//! variable declaration. The run-time part (the bare statements, and the
//! initializers as assignments) stays at the declaration's position as a
//! [`Stmt::PackageRuntimeBody`], which re-enters the package to run them.
//! The two parts share the body's lexicals through the package's static
//! store, the way the body's methods already do.

use crate::ast::{Expr, PackageRuntimeDecl, Stmt};

/// Split a declaration the prologue takes into its BEGIN-time declaration and
/// its run-time part. A declaration with no run-time part comes back
/// unchanged, with `None`.
pub(super) fn split_package_decl(stmt: Stmt) -> (Stmt, Option<Stmt>) {
    match stmt {
        Stmt::ClassDecl {
            name_expr: None,
            is_unit: false,
            ..
        }
        | Stmt::Package { is_unit: false, .. } => {
            let mut stmt = stmt;
            let (name, body, decl) = match &mut stmt {
                Stmt::ClassDecl {
                    name,
                    body,
                    custom_traits,
                    is_lexical,
                    decl_id,
                    ..
                } => {
                    if custom_traits.iter().any(|(t, _)| t == "__hoisted") {
                        return (stmt, None);
                    }
                    let decl = PackageRuntimeDecl::Class {
                        is_lexical: *is_lexical,
                        decl_id: *decl_id,
                    };
                    (*name, body, decl)
                }
                Stmt::Package { name, body, .. } => (*name, body, PackageRuntimeDecl::Package),
                _ => unreachable!(),
            };
            if !splittable_body(body) {
                return (stmt, None);
            }
            let runtime = match split_body(std::mem::take(body)) {
                Ok((decls, runtime)) => {
                    *body = decls;
                    runtime
                }
                Err(unchanged) => {
                    *body = unchanged;
                    return (stmt, None);
                }
            };
            let mut lexicals: Vec<String> =
                crate::compiler::Compiler::package_body_lexical_names(body)
                    .into_iter()
                    .collect();
            lexicals.sort();
            let runtime = Stmt::PackageRuntimeBody {
                name,
                body: runtime,
                lexicals,
                decl,
            };
            (stmt, Some(runtime))
        }
        // An exported type (`class C is export { }`) is the declaration plus
        // its `__MUTSU_EXPORT_TYPE__` marker.
        Stmt::SyntheticBlock(inner) => {
            let mut runtime = Vec::new();
            let inner = inner
                .into_iter()
                .map(|member| {
                    let (member, member_runtime) = split_package_decl(member);
                    runtime.extend(member_runtime);
                    member
                })
                .collect();
            let runtime = match runtime.len() {
                0 => None,
                1 => runtime.pop(),
                _ => Some(Stmt::SyntheticBlock(runtime)),
            };
            (Stmt::SyntheticBlock(inner), runtime)
        }
        other => (other, None),
    }
}

/// A stub body (`class A { ... }`) is a declaration only.
fn splittable_body(body: &[Stmt]) -> bool {
    !body.iter().filter(|s| !s.is_marker()).all(
        |s| matches!(s, Stmt::Expr(Expr::Call { name, .. }) if is_internal_call(&name.resolve())),
    )
}

/// Split a body into its BEGIN-time statements and its run-time ones. When
/// nothing in it runs at run time, the body comes back untouched as `Err`.
fn split_body(body: Vec<Stmt>) -> Result<(Vec<Stmt>, Vec<Stmt>), Vec<Stmt>> {
    if !body.iter().any(has_runtime_part) {
        return Err(body);
    }
    let mut decls = Vec::with_capacity(body.len());
    let mut runtime = Vec::new();
    for stmt in body {
        split_body_stmt(stmt, &mut decls, &mut runtime);
    }
    Ok((decls, runtime))
}

fn split_body_stmt(stmt: Stmt, decls: &mut Vec<Stmt>, runtime: &mut Vec<Stmt>) {
    match stmt {
        // The line marker goes to both halves, so each statement keeps its line.
        Stmt::SetLine(_) => {
            runtime.push(stmt.clone());
            decls.push(stmt);
        }
        Stmt::VarDecl { .. } if splittable_var_decl(&stmt) => {
            match crate::runtime::phasers::split_var_decl(&stmt) {
                Some((static_decl, assign)) => {
                    decls.push(static_decl);
                    runtime.extend(assign);
                }
                None => decls.push(stmt),
            }
        }
        // A group declaration `my ($a, @b);` splits member by member.
        Stmt::SyntheticBlock(inner) if crate::ast::is_group_declaration(&inner) => {
            for member in inner {
                // The source-form record goes with the group it described.
                if !matches!(member, Stmt::SourceForm(_)) {
                    split_body_stmt(member, decls, runtime);
                }
            }
        }
        Stmt::ClassDecl { .. } | Stmt::Package { .. } | Stmt::SyntheticBlock(_) => {
            let (decl, decl_runtime) = split_package_decl(stmt);
            decls.push(decl);
            runtime.extend(decl_runtime);
        }
        _ if is_runtime_stmt(&stmt) => runtime.push(stmt),
        _ => decls.push(stmt),
    }
}

/// Whether `stmt` contributes anything to a body's run-time part.
fn has_runtime_part(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::SourceForm(_) => false,
        Stmt::VarDecl { custom_traits, .. } => {
            splittable_var_decl(stmt)
                && custom_traits
                    .iter()
                    .any(|(t, _)| t == "__has_initializer" || t == "__scalar_bind")
        }
        Stmt::ClassDecl { .. } | Stmt::Package { .. } => {
            matches!(split_package_decl(stmt.clone()), (_, Some(_)))
        }
        // Mirrors `split_body_stmt`: a group declaration splits member by
        // member, and any other block only splits the types it declares.
        Stmt::SyntheticBlock(inner) if crate::ast::is_group_declaration(inner) => {
            inner.iter().any(has_runtime_part)
        }
        Stmt::SyntheticBlock(inner) => inner.iter().any(|s| {
            matches!(s, Stmt::ClassDecl { .. } | Stmt::Package { .. }) && has_runtime_part(s)
        }),
        _ => is_runtime_stmt(stmt),
    }
}

/// A plain `my`/`our` variable whose initializer runs at run time. A
/// `state`, dynamic or exported variable, and a code variable (which a
/// method may call as a class-scoped routine), keep their declaration whole.
fn splittable_var_decl(stmt: &Stmt) -> bool {
    let Stmt::VarDecl {
        name,
        is_state,
        is_dynamic,
        is_export,
        custom_traits,
        ..
    } = stmt
    else {
        return false;
    };
    !is_state
        && !is_dynamic
        && !is_export
        && !name.starts_with('&')
        && !custom_traits.iter().any(|(t, _)| t == "__constant")
}

/// A statement that runs at run time in Rakudo and declares nothing.
fn is_runtime_stmt(stmt: &Stmt) -> bool {
    match stmt {
        // An anonymous method (`method { $!x }`) is validated against the
        // class's attributes while the class composes.
        Stmt::Expr(Expr::AnonSubParams { params, .. }) => {
            params.first().map(String::as_str) != Some("self")
        }
        Stmt::Expr(Expr::Call { name, .. }) => !is_internal_call(&name.resolve()),
        Stmt::Expr(_)
        | Stmt::Say(_)
        | Stmt::Put(_)
        | Stmt::Print(_)
        | Stmt::Note(_)
        | Stmt::Call { .. }
        | Stmt::Assign { .. }
        | Stmt::For { .. }
        | Stmt::If { .. }
        | Stmt::While { .. }
        | Stmt::Loop { .. }
        | Stmt::Given { .. }
        | Stmt::Block(_)
        | Stmt::Die(_)
        | Stmt::Fail(_) => true,
        _ => false,
    }
}

/// A call the parser synthesizes as a marker (`__MUTSU_EXPORT_TYPE__`,
/// `__mutsu_stub_die`, ...), which is part of a declaration.
fn is_internal_call(name: &str) -> bool {
    name.starts_with("__mutsu") || name.starts_with("__MUTSU")
}
