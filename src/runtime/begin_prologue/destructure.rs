//! The BEGIN-prologue split of a destructuring declaration
//! (`my ($a, $b) = RHS`, ADR-0134).
//!
//! The parser expands the declaration into a `SyntheticBlock`: the plain
//! targets predeclared with their default value, the staging temp holding the
//! right-hand side, then each target declared again from its element of the
//! temp. A plain `my $x = 5` splits into a static declaration in the prologue
//! and an assignment at its position (`phasers::split_var_decl`); this is the
//! same split for the expanded form. The predeclarations move to the prologue,
//! and each re-declaration of a target becomes an assignment, so the targets
//! keep the containers the prologue declared.
//!
//! The containers matter because a class the prologue composes (one that does
//! a role) is compiled and registered there. A method of it that writes `$a`
//! makes the declaration of `$a` give the variable a shared cell, and the
//! method captures that cell at registration. A re-declaration at the old
//! position would replace the cell with a fresh container, which the method
//! never sees.

use crate::ast::{AssignOp, Stmt};

/// Split the destructuring declaration `stmt`: push its static half onto
/// `prologue` and return the run-time half. `None`, with `prologue` untouched,
/// for any other statement and for a destructuring declaration with a target
/// the split cannot express as an assignment (a bind, a trait, a `state` or
/// `our` target).
// Cost: O(s), s = statements of the expansion.
pub(super) fn split(stmt: &Stmt, prologue: &mut Vec<Stmt>) -> Option<Stmt> {
    let Stmt::SyntheticBlock(inner) = stmt else {
        return None;
    };
    if !crate::ast::signature_decl::is_destructure_block(inner) {
        return None;
    }
    let predecl_count = inner.iter().take_while(|s| is_target_decl(s)).count();
    let mut statics = Vec::with_capacity(predecl_count);
    for member in &inner[..predecl_count] {
        match crate::runtime::phasers::split_var_decl(member) {
            Some((static_decl, None)) if plain_target(member) => statics.push(static_decl),
            _ => return None,
        }
    }
    let targets: Vec<&str> = inner[..predecl_count]
        .iter()
        .filter_map(|s| match s {
            Stmt::VarDecl { name, .. } => Some(name.as_str()),
            _ => None,
        })
        .collect();
    let mut runtime = Vec::with_capacity(inner.len() - predecl_count);
    for member in &inner[predecl_count..] {
        match member {
            Stmt::VarDecl { name, expr, .. } if targets.contains(&name.as_str()) => {
                if !plain_target(member) {
                    return None;
                }
                runtime.push(Stmt::Assign {
                    name: name.clone(),
                    expr: expr.clone(),
                    op: AssignOp::Assign,
                    target_is_sigilless: false,
                });
            }
            other => runtime.push(other.clone()),
        }
    }
    prologue.extend(statics);
    Some(Stmt::SyntheticBlock(runtime))
}

/// A target declaration of the expansion: any declaration but the staging
/// temp's.
// Cost: O(1).
fn is_target_decl(stmt: &Stmt) -> bool {
    matches!(stmt, Stmt::VarDecl { name, .. } if !name.contains("__destructure_tmp__"))
}

/// A target an assignment can stand in for: a plain `my` with no bind, no
/// user trait and no sigilless term.
// Cost: O(t), t = the declaration's traits.
fn plain_target(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::VarDecl {
            is_state: false,
            is_our: false,
            bind: None,
            constant: None,
            custom_traits,
            marks,
            ..
        } if custom_traits.iter().all(|(t, _)| t.starts_with("__"))
            && marks.term == crate::ast::var_marks::TermMark::No
    )
}
