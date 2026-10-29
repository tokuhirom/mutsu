//! The lowering of `ParsedStmt::WillDecl`: the declaration followed by one
//! phaser per `will <phaser> { ... }` trait (ADR-15334).

use crate::ast::{Expr, PhaserKind, Stmt};

/// Wrap a statement with one or more `will <phaser> { ... }` phasers.
/// For each phaser, the body is wrapped in a `given <topic>` block so that
/// `$_` is correctly set to the variable value for the duration of the block,
/// and restored to the outer `$_` when the block exits.
///
/// For compile-time phasers (Begin, Check, Init), `$_` is bound to the
/// variable's initial expression rather than the variable reference.
/// This ensures the correct type is available even before runtime
/// initialization (e.g. `@a` has type `Array`, not `Nil`, at BEGIN time).
// Cost: O(n), n = size of the declaration and the phaser bodies.
pub(super) fn wrap(
    stmt: Stmt,
    var_name: &str,
    will_phasers: Vec<(PhaserKind, Vec<Stmt>)>,
) -> Stmt {
    // Extract the initialization expression from the VarDecl (if any) so
    // compile-time phasers (BEGIN/CHECK/INIT) can bind $_ to the container.
    let init_expr = if let Stmt::VarDecl { expr, .. } = &stmt {
        Some(expr.clone())
    } else {
        None
    };
    let mut stmts = vec![stmt];
    for (kind, body) in will_phasers {
        // For compile-time phasers, bind $_ to the initial value/container so
        // it has the correct type (e.g. Array for @a, Hash for %h).
        // For runtime phasers, bind $_ to the variable itself so the body
        // sees the current runtime value.
        let topic_expr = match &kind {
            PhaserKind::Begin | PhaserKind::Check | PhaserKind::Init => init_expr
                .clone()
                .unwrap_or_else(|| Expr::Var(var_name.to_string())),
            _ => Expr::Var(var_name.to_string()),
        };
        // Wrap the user-supplied body in `given <topic> { <body> }` so that
        // $_ is properly scoped (saved and restored when the block exits).
        let phaser_body = vec![Stmt::Given {
            topic: topic_expr,
            body,
            is_statement_modifier: false,
            with_kind: None,
        }];
        stmts.push(Stmt::Phaser {
            spans: Default::default(),
            kind,
            body: phaser_body,
            condition: None,
            end_index: None,
            bare: false,
        });
    }
    Stmt::SyntheticBlock(stmts)
}

