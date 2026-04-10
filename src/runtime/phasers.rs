use crate::ast::{AssignOp, Expr, PhaserKind, Stmt};
use std::sync::atomic::{AtomicUsize, Ordering};

static PHASER_TEMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn next_temp_name() -> String {
    let n = PHASER_TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    // Use name without $ prefix to match parser convention.
    // VarDecl names and Var references use bare names (e.g. "x" not "$x").
    format!("__phaser_result_{}", n)
}

/// Top-level entry point for phaser reordering.
///
/// Within each block, reorders statements so that:
/// 1. VarDecls (bare declarations, hoisted)
/// 2. BEGIN bodies (forward order)
/// 3. CHECK bodies (reverse order)
/// 4. INIT bodies (forward order)
/// 5. Rest of statements (original order, with VarDecl initializers as assigns)
///
/// Additionally:
/// - INIT/CHECK inside "transparent" blocks (bare blocks with no VarDecls)
///   are lifted to the parent scope before reordering.
/// - INIT/CHECK PhaserExpr (rvalue) inside closures are extracted to the
///   enclosing scope so they run once, not per-call.
pub(crate) fn reorder_phasers(stmts: &mut Vec<Stmt>) {
    reorder_recursive(stmts);
}

fn reorder_recursive(stmts: &mut Vec<Stmt>) {
    // Flatten SyntheticBlocks so VarDecls get hoisted properly.
    flatten_synthetic_blocks(stmts);

    // Lift BEGIN/INIT/CHECK from transparent child blocks/closures to this level.
    let mut lifted_begin: Vec<Stmt> = Vec::new();
    let mut lifted_check: Vec<Stmt> = Vec::new();
    let mut lifted_init: Vec<Stmt> = Vec::new();
    lift_phasers(
        stmts,
        &mut lifted_begin,
        &mut lifted_check,
        &mut lifted_init,
    );

    // Per-block reordering at this level.
    reorder_at_level(stmts, lifted_begin, lifted_check, lifted_init);

    // Recurse into child statements.
    for stmt in stmts.iter_mut() {
        recurse_into_stmt(stmt);
    }
}

/// Flatten SyntheticBlock wrappers (e.g. from `my $x ~= "o"`).
/// Only flattens blocks that don't contain MarkReadonly, since those
/// need to stay as atomic units (e.g. `my $x := 42`).
fn flatten_synthetic_blocks(stmts: &mut Vec<Stmt>) {
    let old = std::mem::take(stmts);
    for stmt in old {
        if let Stmt::SyntheticBlock(ref inner) = stmt {
            let has_mark_readonly = inner
                .iter()
                .any(|s| matches!(s, Stmt::MarkReadonly(_) | Stmt::MarkSigillessReadonly(_)));
            // Keep SyntheticBlocks with bound array markers intact so the compiler
            // can detect `:=` bind context for `@` variables.
            let has_bound_array = inner.iter().any(|s| {
                matches!(s,
                    Stmt::Expr(Expr::Call { name, .. })
                    if name.resolve() == "__mutsu_record_bound_array_len"
                )
            });
            let has_mark_bind = inner.iter().any(|s| matches!(s, Stmt::MarkBind));
            if has_mark_readonly || has_bound_array || has_mark_bind {
                stmts.push(stmt);
            } else if let Stmt::SyntheticBlock(inner) = stmt {
                stmts.extend(inner);
            }
        } else {
            stmts.push(stmt);
        }
    }
}

/// Lift BEGIN/INIT/CHECK phasers from child blocks/closures to the current level.
/// "Transparent" blocks are bare blocks (Stmt::Block) without VarDecls.
/// BEGIN/INIT/CHECK phasers are extracted from:
/// 1. Transparent blocks (statement-level phasers)
/// 2. Closures/lambdas in expressions (PhaserExpr rvalues)
fn lift_phasers(
    stmts: &mut [Stmt],
    begin: &mut Vec<Stmt>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    for stmt in stmts.iter_mut() {
        lift_phasers_from_stmt(stmt, begin, check, init);
    }
}

fn lift_phasers_from_stmt(
    stmt: &mut Stmt,
    begin: &mut Vec<Stmt>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    match stmt {
        // Transparent blocks: extract BEGIN/INIT/CHECK phasers
        Stmt::Block(body) => {
            let has_var_decls = body.iter().any(|s| matches!(s, Stmt::VarDecl { .. }));
            if !has_var_decls {
                // Extract BEGIN/INIT/CHECK from this block
                extract_phasers_from_stmts(body, begin, check, init);
                // Also recurse deeper into transparent sub-blocks
                lift_phasers(body, begin, check, init);
            }
        }
        // Expression statements: extract PhaserExpr from closures
        Stmt::Expr(expr)
        | Stmt::Return(expr)
        | Stmt::Die(expr)
        | Stmt::Fail(expr)
        | Stmt::Take(expr) => {
            lift_phasers_from_expr(expr, begin, check, init);
        }
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
            lift_phasers_from_expr(expr, begin, check, init);
        }
        Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
            for e in exprs.iter_mut() {
                lift_phasers_from_expr(e, begin, check, init);
            }
        }
        Stmt::Call { args, .. } => {
            for arg in args.iter_mut() {
                match arg {
                    crate::ast::CallArg::Positional(e)
                    | crate::ast::CallArg::Slip(e)
                    | crate::ast::CallArg::Invocant(e) => {
                        lift_phasers_from_expr(e, begin, check, init);
                    }
                    crate::ast::CallArg::Named { value, .. } => {
                        if let Some(e) = value {
                            lift_phasers_from_expr(e, begin, check, init);
                        }
                    }
                }
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            lift_phasers_from_expr(cond, begin, check, init);
            // Lift from if/else branches - BEGIN/INIT/CHECK are global
            lift_phasers_from_closure_stmts(then_branch, begin, check, init);
            lift_phasers_from_closure_stmts(else_branch, begin, check, init);
        }
        Stmt::For { iterable, body, .. } => {
            lift_phasers_from_expr(iterable, begin, check, init);
            // Lift BEGIN/INIT/CHECK from loop body - they should run once
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::While { cond, body, .. } => {
            lift_phasers_from_expr(cond, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::When { cond, body } => {
            lift_phasers_from_expr(cond, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::Loop { body, .. } | Stmt::React { body } => {
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::Given { topic, body } => {
            lift_phasers_from_expr(topic, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::Whenever { supply, body, .. } => {
            lift_phasers_from_expr(supply, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::Label { stmt: inner, .. } => {
            lift_phasers_from_stmt(inner, begin, check, init);
        }
        _ => {}
    }
}

/// Extract INIT/CHECK statement-level phasers from a stmt list.
/// Replaces extracted phasers with a temp variable expression so the phaser's
/// return value is available in expression context (e.g. string interpolation).
fn extract_phasers_from_stmts(
    stmts: &mut [Stmt],
    _begin: &mut Vec<Stmt>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    for stmt in stmts.iter_mut() {
        if matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Check | PhaserKind::Init,
                ..
            }
        ) {
            let temp_name = next_temp_name();
            let old = std::mem::replace(stmt, Stmt::Expr(Expr::Var(temp_name.clone())));
            if let Stmt::Phaser { kind, body } = old {
                let var_decl = Stmt::VarDecl {
                    name: temp_name.clone(),
                    expr: Expr::Literal(crate::value::Value::Nil),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: vec![],
                    custom_traits: vec![],
                    where_constraint: None,
                };
                let assign = Stmt::Assign {
                    name: temp_name,
                    expr: Expr::DoBlock { body, label: None },
                    op: AssignOp::Assign,
                };
                match kind {
                    PhaserKind::Check => {
                        check.push(var_decl);
                        check.push(assign);
                    }
                    PhaserKind::Init => {
                        init.push(var_decl);
                        init.push(assign);
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}

/// Extract BEGIN statement-level phasers from a stmt list inside DoStmt blocks.
/// This is separate from extract_phasers_from_stmts because BEGIN should only
/// be extracted from DoStmt contexts (e.g. string interpolation), not from
/// general blocks which may contain local sub declarations needed by the BEGIN body.
fn extract_begin_from_stmts(stmts: &mut [Stmt], begin: &mut Vec<Stmt>) {
    for stmt in stmts.iter_mut() {
        if matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            }
        ) {
            let temp_name = next_temp_name();
            let old = std::mem::replace(stmt, Stmt::Expr(Expr::Var(temp_name.clone())));
            if let Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
            } = old
            {
                let var_decl = Stmt::VarDecl {
                    name: temp_name.clone(),
                    expr: Expr::Literal(crate::value::Value::Nil),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: vec![],
                    custom_traits: vec![],
                    where_constraint: None,
                };
                let assign = Stmt::Assign {
                    name: temp_name,
                    expr: Expr::DoBlock { body, label: None },
                    op: AssignOp::Assign,
                };
                begin.push(var_decl);
                begin.push(assign);
            }
        }
    }
}

/// Extract PhaserExpr { Check | Init } from expressions (including closures).
fn lift_phasers_from_expr(
    expr: &mut Expr,
    begin: &mut Vec<Stmt>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    // Handle PhaserExpr at this node
    if matches!(
        expr,
        Expr::PhaserExpr {
            kind: PhaserKind::Begin | PhaserKind::Check | PhaserKind::Init,
            ..
        }
    ) {
        let temp_name = next_temp_name();
        let old = std::mem::replace(expr, Expr::Var(temp_name.clone()));
        if let Expr::PhaserExpr { kind, body } = old {
            let var_decl = Stmt::VarDecl {
                name: temp_name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: vec![],
                custom_traits: vec![],
                where_constraint: None,
            };
            let assign = Stmt::Assign {
                name: temp_name,
                expr: Expr::DoBlock { body, label: None },
                op: AssignOp::Assign,
            };
            match kind {
                PhaserKind::Begin => {
                    begin.push(var_decl);
                    begin.push(assign);
                }
                PhaserKind::Check => {
                    check.push(var_decl);
                    check.push(assign);
                }
                PhaserKind::Init => {
                    init.push(var_decl);
                    init.push(assign);
                }
                _ => unreachable!(),
            }
        }
        return;
    }

    // Recurse into sub-expressions
    match expr {
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            lift_phasers_from_expr(left, begin, check, init);
            lift_phasers_from_expr(right, begin, check, init);
        }
        Expr::Unary { expr: inner, .. } | Expr::PostfixOp { expr: inner, .. } => {
            lift_phasers_from_expr(inner, begin, check, init);
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            lift_phasers_from_expr(target, begin, check, init);
            for a in args.iter_mut() {
                lift_phasers_from_expr(a, begin, check, init);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            lift_phasers_from_expr(target, begin, check, init);
            lift_phasers_from_expr(name_expr, begin, check, init);
            for a in args.iter_mut() {
                lift_phasers_from_expr(a, begin, check, init);
            }
        }
        Expr::Call { args, .. } => {
            for a in args.iter_mut() {
                lift_phasers_from_expr(a, begin, check, init);
            }
        }
        Expr::CallOn { target, args } => {
            lift_phasers_from_expr(target, begin, check, init);
            for a in args.iter_mut() {
                lift_phasers_from_expr(a, begin, check, init);
            }
        }
        Expr::Index { target, index } => {
            lift_phasers_from_expr(target, begin, check, init);
            lift_phasers_from_expr(index, begin, check, init);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            lift_phasers_from_expr(cond, begin, check, init);
            lift_phasers_from_expr(then_expr, begin, check, init);
            lift_phasers_from_expr(else_expr, begin, check, init);
        }
        Expr::AssignExpr { expr: inner, .. }
        | Expr::PositionalPair(inner)
        | Expr::ZenSlice(inner)
        | Expr::Eager(inner)
        | Expr::Reduction { expr: inner, .. }
        | Expr::IndirectCodeLookup { package: inner, .. }
        | Expr::SymbolicDeref { expr: inner, .. } => {
            lift_phasers_from_expr(inner, begin, check, init);
        }
        Expr::Exists { target, arg, .. } => {
            lift_phasers_from_expr(target, begin, check, init);
            if let Some(a) = arg {
                lift_phasers_from_expr(a, begin, check, init);
            }
        }
        Expr::ArrayLiteral(es)
        | Expr::BracketArray(es, _)
        | Expr::StringInterpolation(es)
        | Expr::CaptureLiteral(es) => {
            for e in es.iter_mut() {
                lift_phasers_from_expr(e, begin, check, init);
            }
        }
        // Recurse into closures — extract PhaserExpr from them
        Expr::Block(stmts)
        | Expr::AnonSub { body: stmts, .. }
        | Expr::AnonSubParams { body: stmts, .. }
        | Expr::Gather(stmts)
        | Expr::DoBlock { body: stmts, .. } => {
            lift_phasers_from_closure_stmts(stmts, begin, check, init);
        }
        Expr::Lambda { body, .. } => {
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Expr::Try { body, catch } => {
            lift_phasers_from_closure_stmts(body, begin, check, init);
            if let Some(c) = catch {
                lift_phasers_from_closure_stmts(c, begin, check, init);
            }
        }
        Expr::InfixFunc { left, right, .. } => {
            lift_phasers_from_expr(left, begin, check, init);
            for e in right.iter_mut() {
                lift_phasers_from_expr(e, begin, check, init);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs.iter_mut() {
                if let Some(e) = v {
                    lift_phasers_from_expr(e, begin, check, init);
                }
            }
        }
        Expr::PhaserExpr { body, .. } => {
            // Non-BEGIN/CHECK/INIT PhaserExpr (e.g. END) — don't extract, just recurse
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Expr::DoStmt(inner_stmt) => {
            // For DoStmt (e.g. string interpolation blocks), also extract
            // BEGIN phasers from the inner block. This is safe because DoStmt
            // blocks are simple expression wrappers, unlike general blocks that
            // may contain local sub declarations needed by BEGIN.
            if let Stmt::Block(body) = inner_stmt.as_mut() {
                extract_begin_from_stmts(body, begin);
            }
            lift_phasers_from_stmt(inner_stmt, begin, check, init);
        }
        _ => {}
    }
}

/// Extract BEGIN/INIT/CHECK phasers from inside closure bodies.
/// Both statement-level phasers and PhaserExpr are extracted.
fn lift_phasers_from_closure_stmts(
    stmts: &mut [Stmt],
    begin: &mut Vec<Stmt>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    // Extract statement-level BEGIN/INIT/CHECK
    extract_phasers_from_stmts(stmts, begin, check, init);
    // Recurse into each statement for PhaserExpr
    for stmt in stmts.iter_mut() {
        lift_phasers_from_closure_stmt(stmt, begin, check, init);
    }
}

fn lift_phasers_from_closure_stmt(
    stmt: &mut Stmt,
    begin: &mut Vec<Stmt>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    match stmt {
        Stmt::Expr(expr)
        | Stmt::Return(expr)
        | Stmt::Die(expr)
        | Stmt::Fail(expr)
        | Stmt::Take(expr) => {
            lift_phasers_from_expr(expr, begin, check, init);
        }
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
            lift_phasers_from_expr(expr, begin, check, init);
        }
        Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
            for e in exprs.iter_mut() {
                lift_phasers_from_expr(e, begin, check, init);
            }
        }
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::Loop { body, .. }
        | Stmt::React { body } => {
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            lift_phasers_from_expr(cond, begin, check, init);
            lift_phasers_from_closure_stmts(then_branch, begin, check, init);
            lift_phasers_from_closure_stmts(else_branch, begin, check, init);
        }
        Stmt::While { cond, body, .. } | Stmt::When { cond, body } => {
            lift_phasers_from_expr(cond, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::For { iterable, body, .. } => {
            lift_phasers_from_expr(iterable, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::Given { topic, body } => {
            lift_phasers_from_expr(topic, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::Whenever { supply, body, .. } => {
            lift_phasers_from_expr(supply, begin, check, init);
            lift_phasers_from_closure_stmts(body, begin, check, init);
        }
        Stmt::Label { stmt: inner, .. } => {
            lift_phasers_from_closure_stmt(inner, begin, check, init);
        }
        Stmt::Call { args, .. } => {
            for arg in args.iter_mut() {
                match arg {
                    crate::ast::CallArg::Positional(e)
                    | crate::ast::CallArg::Slip(e)
                    | crate::ast::CallArg::Invocant(e) => {
                        lift_phasers_from_expr(e, begin, check, init);
                    }
                    crate::ast::CallArg::Named { value, .. } => {
                        if let Some(e) = value {
                            lift_phasers_from_expr(e, begin, check, init);
                        }
                    }
                }
            }
        }
        _ => {}
    }
}

// ── Per-block reordering ───────────────────────────────────────────

/// Reorder statements at a single block level.
/// Hoists VarDecls, then BEGIN (forward), CHECK (reverse), INIT (forward), rest.
fn reorder_at_level(
    stmts: &mut Vec<Stmt>,
    extra_begin: Vec<Stmt>,
    extra_check: Vec<Stmt>,
    extra_init: Vec<Stmt>,
) {
    let mut var_decls: Vec<Stmt> = Vec::new();
    let mut use_stmts: Vec<Stmt> = Vec::new();
    let mut begin: Vec<Stmt> = Vec::new();
    let mut check: Vec<Vec<Stmt>> = Vec::new();
    let mut init: Vec<Vec<Stmt>> = Vec::new();
    let mut rest: Vec<Stmt> = Vec::new();

    let has_phasers = stmts.iter().any(|s| {
        matches!(
            s,
            Stmt::Phaser {
                kind: PhaserKind::Check | PhaserKind::Init,
                ..
            }
        )
    }) || !extra_begin.is_empty()
        || !extra_check.is_empty()
        || !extra_init.is_empty()
        || stmts.iter().any(stmt_has_phaser_expr);

    if !has_phasers {
        return;
    }

    for stmt in stmts.drain(..) {
        if let Stmt::Phaser { kind, body } = &stmt {
            match kind {
                PhaserKind::Begin => {
                    // BEGIN phasers are kept as whole Stmt::Phaser nodes
                    // (not extracted into raw body stmts) to preserve compiler
                    // slot allocation for array/hash container assignments.
                    // They are placed in a separate bucket so they run before
                    // CHECK blocks (BEGIN runs first in forward order, then
                    // CHECK runs in reverse order).
                    begin.push(stmt);
                    continue;
                }
                PhaserKind::Check => {
                    check.push(body.clone());
                    continue;
                }
                PhaserKind::Init => {
                    init.push(body.clone());
                    continue;
                }
                _ => {}
            }
        }

        // Hoist VarDecl (bare declarations) so that CHECK/INIT blocks
        // can reference variables declared later in source order.
        if let Stmt::VarDecl {
            name,
            expr: init_expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic,
            is_export,
            export_tags,
            custom_traits,
            where_constraint,
        } = &stmt
        {
            let has_init = !matches!(init_expr, Expr::Literal(crate::value::Value::Nil));
            var_decls.push(Stmt::VarDecl {
                name: name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: type_constraint.clone(),
                is_state: *is_state,
                is_our: *is_our,
                is_dynamic: *is_dynamic,
                is_export: *is_export,
                export_tags: export_tags.clone(),
                custom_traits: custom_traits.clone(),
                where_constraint: where_constraint.clone(),
            });
            if has_init {
                rest.push(Stmt::Assign {
                    name: name.clone(),
                    expr: init_expr.clone(),
                    op: AssignOp::Assign,
                });
            }
            continue;
        }

        // Hoist `use` statements before CHECK/INIT blocks, since `use` is
        // a compile-time directive in Raku and its imports must be available
        // to CHECK blocks.
        if matches!(&stmt, Stmt::Use { .. }) {
            use_stmts.push(stmt);
            continue;
        }

        rest.push(stmt);
    }

    // Reconstruct: VarDecls first, then `use` (compile-time imports),
    // then BEGIN (forward), CHECK (reverse), INIT (forward), then rest.
    stmts.extend(var_decls);
    stmts.extend(use_stmts);
    stmts.extend(begin);
    // Extra BEGIN from lifted phasers (e.g. inside string interpolation blocks).
    stmts.extend(extra_begin);
    for body in check.iter().rev() {
        stmts.extend(body.iter().cloned());
    }
    // Extra CHECK from lifted phasers.
    // Each phaser is a VarDecl+Assign pair. Reverse by pairs for CHECK order.
    // TODO: For multiple CHECK PhaserExprs, pairs should be reversed.
    // For now, just extend in forward order (correct for single CHECK).
    stmts.extend(extra_check);
    for body in &init {
        stmts.extend(body.iter().cloned());
    }
    stmts.extend(extra_init);
    stmts.extend(rest);
}

fn stmt_has_phaser_expr(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(e)
        | Stmt::Return(e)
        | Stmt::VarDecl { expr: e, .. }
        | Stmt::Assign { expr: e, .. } => expr_has_phaser_expr(e),
        _ => false,
    }
}

fn expr_has_phaser_expr(expr: &Expr) -> bool {
    matches!(expr, Expr::PhaserExpr { .. })
}

// ── Recursion into child blocks ────────────────────────────────────

fn recurse_into_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::ClassDecl { body, .. }
        | Stmt::RoleDecl { body, .. }
        | Stmt::Subtest { body, .. }
        | Stmt::Package { body, .. } => {
            reorder_recursive(body);
        }
        Stmt::Phaser { body, .. } => {
            reorder_recursive(body);
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            recurse_into_expr(cond);
            reorder_recursive(then_branch);
            reorder_recursive(else_branch);
        }
        Stmt::While { cond, body, .. } | Stmt::When { cond, body } => {
            recurse_into_expr(cond);
            reorder_recursive(body);
        }
        Stmt::For { iterable, body, .. } => {
            recurse_into_expr(iterable);
            reorder_recursive(body);
        }
        Stmt::Loop { body, .. } | Stmt::React { body } => {
            reorder_recursive(body);
        }
        Stmt::Given { topic, body } => {
            recurse_into_expr(topic);
            reorder_recursive(body);
        }
        Stmt::Whenever { supply, body, .. } => {
            recurse_into_expr(supply);
            reorder_recursive(body);
        }
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
            recurse_into_expr(e);
        }
        Stmt::VarDecl { expr: e, .. } | Stmt::Assign { expr: e, .. } => {
            recurse_into_expr(e);
        }
        Stmt::SubDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::ProtoDecl { body, .. } => {
            reorder_recursive(body);
        }
        Stmt::Label { stmt: inner, .. } => {
            recurse_into_stmt(inner);
        }
        _ => {}
    }
}

fn recurse_into_expr(expr: &mut Expr) {
    match expr {
        Expr::Block(stmts)
        | Expr::AnonSub { body: stmts, .. }
        | Expr::AnonSubParams { body: stmts, .. }
        | Expr::Gather(stmts)
        | Expr::DoBlock { body: stmts, .. } => {
            reorder_recursive(stmts);
        }
        Expr::Lambda { body, .. } => {
            reorder_recursive(body);
        }
        Expr::Try { body, catch } => {
            reorder_recursive(body);
            if let Some(c) = catch {
                reorder_recursive(c);
            }
        }
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            recurse_into_expr(left);
            recurse_into_expr(right);
        }
        Expr::Unary { expr: inner, .. } | Expr::PostfixOp { expr: inner, .. } => {
            recurse_into_expr(inner);
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            recurse_into_expr(target);
            for a in args.iter_mut() {
                recurse_into_expr(a);
            }
        }
        Expr::Call { args, .. } => {
            for a in args.iter_mut() {
                recurse_into_expr(a);
            }
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            recurse_into_expr(cond);
            recurse_into_expr(then_expr);
            recurse_into_expr(else_expr);
        }
        Expr::PhaserExpr { body, .. } => {
            reorder_recursive(body);
        }
        _ => {}
    }
}
