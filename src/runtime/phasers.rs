mod lift;

use crate::ast::{AssignOp, Expr, PhaserKind, Stmt};
use crate::value::Value;
use crate::value::ValueMap;
use lift::{lift_phasers, recurse_into_stmt};
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
///
/// Returns the length of the unit's BEGIN prologue (ADR-0134), which is left at
/// the head of `stmts`.
pub(crate) fn reorder_phasers(stmts: &mut Vec<Stmt>) -> usize {
    crate::runtime::begin_prologue::lift_nested_exports(stmts);
    reorder_recursive(stmts, true, false)
}

/// EVAL-specific phaser reordering.  In addition to the standard
/// `reorder_phasers`, this also lifts BEGIN phasers from closure bodies
/// that are direct children of the EVAL'd code.  This is needed because
/// EVAL'd code like `$x = { ... BEGIN { ... } ... }` should run the
/// BEGIN at EVAL compile time, not when the closure is called.
///
/// Unlike the general case, closures in EVAL cannot reference locally
/// declared subs from a parent scope (the EVAL scope is already the
/// outermost scope), so lifting BEGIN from them is safe.
pub(crate) fn reorder_phasers_for_eval(stmts: &mut Vec<Stmt>) {
    crate::runtime::begin_prologue::lift_nested_exports(stmts);
    reorder_recursive(stmts, true, true);
    // Second pass: lift BEGIN from closure bodies to the top level.
    let mut extra_begin: Vec<Stmt> = Vec::new();
    for stmt in stmts.iter_mut() {
        lift_begin_from_eval_stmt(stmt, &mut extra_begin);
    }
    if !extra_begin.is_empty() {
        // Find the position of the first non-VarDecl, non-Use statement
        // to insert BEGIN blocks after declarations but before code.
        let insert_pos = stmts
            .iter()
            .position(|s| {
                !matches!(
                    s,
                    Stmt::VarDecl { .. }
                        | Stmt::Use { .. }
                        | Stmt::Phaser {
                            kind: PhaserKind::Begin,
                            ..
                        }
                )
            })
            .unwrap_or(stmts.len());
        // Insert lifted BEGIN blocks as Stmt::Phaser nodes (not raw body
        // stmts) to match reorder_at_level's BEGIN handling.
        for (i, s) in extra_begin.into_iter().enumerate() {
            stmts.insert(insert_pos + i, s);
        }
        // Re-run reorder to properly sort the newly inserted phasers
        reorder_recursive(stmts, true, true);
    }
}

/// Lift BEGIN phasers from a closure that is a top-level statement's whole
/// value in EVAL'd code. Only that one expression is looked at, through any
/// un-expanded WhateverCurry wrappers (ADR-0033): a spine, not a tree walk.
fn lift_begin_from_eval_stmt(stmt: &mut Stmt, begin: &mut Vec<Stmt>) {
    let (Stmt::Assign { expr, .. } | Stmt::VarDecl { expr, .. } | Stmt::Expr(expr)) = stmt else {
        return;
    };
    let mut expr = expr;
    while let Expr::WhateverCurry(inner) = expr {
        expr = inner;
    }
    if let Expr::Block(body)
    | Expr::AnonSub { body, .. }
    | Expr::AnonSubParams { body, .. }
    | Expr::Lambda { body, .. } = expr
    {
        lift::extract_begin_from_stmts(body, begin);
    }
}

/// Returns the length of the BEGIN prologue left at the head of `stmts` (zero
/// below the top level).
fn reorder_recursive(stmts: &mut Vec<Stmt>, is_top: bool, is_eval: bool) -> usize {
    // At a compilation unit's top level, the BEGIN-time effects run first, in
    // source order (ADR-0134). The prologue is taken out before the per-level
    // reordering below, which then only sees the run-time remainder, so no
    // bucketing can move a BEGIN above a declaration it observes. It is taken
    // before the flattening, so a desugared multi-statement declaration
    // (`my ($a, $b) = f()`) reaches the partition as the one unit it is.
    let mut prologue = if is_top {
        crate::runtime::begin_prologue::take_unit_prologue(stmts, is_eval)
    } else {
        Vec::new()
    };

    // Flatten SyntheticBlocks so VarDecls get hoisted properly.
    flatten_synthetic_blocks(stmts);
    // The phasers of a routine the prologue took are the unit's as much as
    // those of one left in the remainder (#10552), so they are lifted to the
    // remainder's level too. The slots they leave behind are read by the
    // routine, so they are declared ahead of it, at the head of the prologue.
    let mut lifted = Lifted::default();
    lift_phasers(
        &mut prologue,
        &mut lifted.begin,
        &mut lifted.check,
        &mut lifted.init,
    );
    let slots = lifted.take_slot_decls();
    if !slots.is_empty() {
        prologue.splice(0..0, slots);
    }
    for stmt in prologue.iter_mut() {
        recurse_into_stmt(stmt);
    }
    reorder_level_and_children(stmts, is_top, lifted);
    let prologue_len = prologue.len();
    if prologue_len > 0 {
        prologue.append(stmts);
        *stmts = prologue;
    }
    prologue_len
}

/// The phasers lifted to one level, each as its slot's declaration followed by
/// the assignment of its body's value.
#[derive(Default)]
struct Lifted {
    begin: Vec<Stmt>,
    check: Vec<Stmt>,
    init: Vec<Stmt>,
}

impl Lifted {
    /// Take the slot declarations out, leaving only the assignments.
    fn take_slot_decls(&mut self) -> Vec<Stmt> {
        let mut decls = Vec::new();
        for list in [&mut self.begin, &mut self.check, &mut self.init] {
            let (slot_decls, assigns): (Vec<Stmt>, Vec<Stmt>) = std::mem::take(list)
                .into_iter()
                .partition(|s| matches!(s, Stmt::VarDecl { .. }));
            decls.extend(slot_decls);
            *list = assigns;
        }
        decls
    }
}

fn reorder_level_and_children(stmts: &mut Vec<Stmt>, is_top: bool, mut lifted: Lifted) {
    // Lift BEGIN/INIT/CHECK from transparent child blocks/closures to this level.
    lift_phasers(
        stmts,
        &mut lifted.begin,
        &mut lifted.check,
        &mut lifted.init,
    );

    // Per-block reordering at this level.
    reorder_at_level(stmts, lifted.begin, lifted.check, lifted.init, is_top);

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
                .any(|s| matches!(s, Stmt::MarkReadonly(..) | Stmt::MarkSigillessReadonly(_)));
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

// ── Per-block reordering ───────────────────────────────────────────

/// Reorder statements at a single block level.
/// Hoists VarDecls, then BEGIN (forward), CHECK (reverse), INIT (forward), rest.
fn reorder_at_level(
    stmts: &mut Vec<Stmt>,
    extra_begin: Vec<Stmt>,
    extra_check: Vec<Stmt>,
    extra_init: Vec<Stmt>,
    is_top: bool,
) {
    let mut var_decls: Vec<Stmt> = Vec::new();
    let mut use_stmts: Vec<Stmt> = Vec::new();
    let mut begin: Vec<Stmt> = Vec::new();
    let mut check: Vec<Vec<Stmt>> = Vec::new();
    let mut init: Vec<Vec<Stmt>> = Vec::new();
    let mut rest: Vec<Stmt> = Vec::new();
    // Constants that textually precede a CHECK and have nothing but other such
    // constants before them: compile-time values a CHECK must see.
    let mut early_consts: Vec<Stmt> = Vec::new();
    let last_check_idx = stmts.iter().rposition(|s| {
        matches!(
            s,
            Stmt::Phaser {
                kind: PhaserKind::Check,
                ..
            }
        )
    });

    // A statement-level BEGIN phaser in a *nested* block must also trigger
    // reordering so the BEGIN is hoisted ahead of the block's plain code — its
    // side effects have to be visible to reads that textually precede it (Raku
    // runs BEGIN at compile time). Top-level BEGINs are handled separately by
    // `run_toplevel_begin_phasers` before this pass, so a bare top-level BEGIN
    // is deliberately left in place here (it either already ran at compile time
    // or is non-hoistable and must run at its original source position).
    //
    // These are the phaser conditions that already triggered reordering before
    // nested-BEGIN hoisting was added; when any hold, BEGINs keep their old
    // behavior (all hoisted into the `begin` bucket, ahead of CHECK/INIT).
    let has_other_phasers = stmts.iter().any(|s| {
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

    // A nested BEGIN is hoisted only to make its compile-time side effects
    // visible to a *read that textually precedes it* — Raku runs BEGIN at
    // compile time. It must NOT be hoisted above a declaration, assignment, or
    // variable initializer it might depend on (those share the source/compile
    // ordering, e.g. `class Foo; BEGIN {...Foo...}`, `has $.a; BEGIN
    // {...set_build...}`, `my $x = 42; BEGIN {...}; constant c = $x`), and there
    // is no point hoisting when every read already follows it (a read-after
    // BEGIN works in place). `first_barrier_idx` is the first such non-hoistable
    // statement; `first_read_idx` is the first read. A BEGIN hoists iff it sits
    // after a read and before the first barrier. Top-level BEGINs are handled
    // separately by `run_toplevel_begin_phasers` and left in place here. Other
    // phasers (CHECK/INIT) are never barriers — BEGINs always run before them.
    let first_barrier_idx = stmts
        .iter()
        .position(|s| !stmt_is_hoist_safe(s) && !matches!(s, Stmt::Phaser { .. }))
        .unwrap_or(usize::MAX);
    let first_read_idx = stmts.iter().position(is_read_stmt).unwrap_or(usize::MAX);
    let nested_begin_hoistable = |idx: usize| first_read_idx < idx && idx < first_barrier_idx;
    let has_nested_begin = !is_top
        && !has_other_phasers
        && stmts
            .iter()
            .enumerate()
            .any(|(i, s)| is_begin_phaser(s) && nested_begin_hoistable(i));

    if !has_other_phasers && !has_nested_begin {
        return;
    }

    for (idx, stmt) in stmts.drain(..).enumerate() {
        if let Stmt::Phaser { kind, body, .. } = &stmt {
            match kind {
                PhaserKind::Begin => {
                    // BEGIN phasers are kept as whole Stmt::Phaser nodes
                    // (not extracted into raw body stmts) to preserve compiler
                    // slot allocation for array/hash container assignments.
                    // They are placed in a separate bucket so they run before
                    // CHECK blocks (BEGIN runs first in forward order, then
                    // CHECK runs in reverse order). With CHECK/INIT present (or
                    // at top level) all BEGINs hoist as before; a bare-BEGIN
                    // nested block only hoists a BEGIN that sits after a read and
                    // before the first barrier, leaving the rest in place.
                    let hoist = is_top || has_other_phasers || nested_begin_hoistable(idx);
                    if hoist {
                        begin.push(stmt);
                    } else {
                        rest.push(stmt);
                    }
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
        //
        // A `constant` declaration is NOT split this way: the compiler marks
        // a constant's local slot readonly unconditionally at the end of
        // compiling its `Stmt::VarDecl` (`is_constant_decl` does not check
        // whether an initializer is present), so a bare hoisted `constant E;`
        // would already be readonly by the time the split-out `E = ...`
        // assign ran, and a plain `Stmt::Assign` (unlike the VarDecl's own
        // `MarkVarDeclContext`-guarded store) does not bypass the readonly
        // check — "Cannot assign to a readonly variable" for e.g.
        // `constant E = BEGIN 5`, whose nested PhaserExpr is exactly what
        // triggers this split. Compiling the constant as one unsplit
        // declaration (its normal, unhoisted path) keeps the mark-readonly
        // strictly after its one real store, like a phaser-free `constant`.
        let is_constant_decl = matches!(&stmt, Stmt::VarDecl { custom_traits, .. } if custom_traits.iter().any(|(t, _)| t == "__constant"));
        if is_constant_decl {
            // A constant is a compile-time value, so a later CHECK sees it. Keep
            // source order by hoisting only while nothing that stays in `rest`
            // and could feed the initializer (a class, an assignment, ...) precedes it.
            let before_check = last_check_idx.is_some_and(|c| idx < c);
            if before_check && rest.iter().all(stmt_is_hoist_safe) {
                early_consts.push(stmt);
            } else {
                rest.push(stmt);
            }
            continue;
        }
        if let Some((static_decl, assign)) = split_var_decl(&stmt) {
            var_decls.push(static_decl);
            rest.extend(assign);
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
    stmts.extend(early_consts);
    for body in check.iter().rev() {
        stmts.push(Stmt::Phaser {
            kind: PhaserKind::Check,
            body: body.clone(),
            condition: None,
            end_index: None,
        });
    }
    // Extra CHECK from lifted phasers.
    // Each phaser is a VarDecl+Assign pair. They must remain as raw statements
    // (not wrapped in Stmt::Phaser) so that the EVAL second-pass insert_pos
    // logic correctly places BEGIN before CHECK via VarDecl matching.
    // The DoBlock body carries a "__mutsu_check_phaser__" sentinel label so
    // the compiler emits CheckPhaserStart/CheckPhaserEnd, ensuring errors
    // are wrapped in X::Comp::BeginTime.
    // TODO: For multiple CHECK PhaserExprs, pairs should be reversed.
    // For now, just extend in forward order (correct for single CHECK).
    stmts.extend(extra_check);
    // Keep statement-level INIT bodies wrapped as phasers.  Compiling an INIT
    // node runs its body inline just as the old raw-body expansion did, but
    // retaining the node preserves compile-time checks that are specific to a
    // phaser body (notably that it cannot take placeholder parameters).
    // Expanding the body here made `INIT { $^x }` look like an ordinary
    // mainline `$^x` to the compiler after reordering, bypassing
    // `emit_block_placeholder_die`.
    for body in &init {
        stmts.push(Stmt::Phaser {
            kind: PhaserKind::Init,
            body: body.clone(),
            condition: None,
            end_index: None,
        });
    }
    stmts.extend(extra_init);
    stmts.extend(rest);
}

/// True if `stmt` is a statement-level `BEGIN` phaser.
fn is_begin_phaser(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Phaser {
            kind: PhaserKind::Begin,
            ..
        }
    )
}

/// True if `stmt` is a "read": a statement that observes program state and
/// would miss a later BEGIN's compile-time side effects if the BEGIN ran after
/// it. Used to decide whether hoisting a nested BEGIN above it is worthwhile.
fn is_read_stmt(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Say(_) | Stmt::Put(_) | Stmt::Print(_) | Stmt::Note(_) => true,
        Stmt::Call { .. } => true,
        Stmt::Expr(e) => !matches!(e, Expr::AssignExpr { .. }) && !is_mixin_expr(e),
        _ => false,
    }
}

/// True if `expr` is a `does`/`but` role-mixin application (`$x does Role`,
/// `@a but Role`). Such an expression mutates its target container as a
/// compile-time declaration effect, so it acts as a hoisting barrier for a
/// following BEGIN rather than as a pure read.
fn is_mixin_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Binary { op: crate::token_kind::TokenKind::Ident(op), .. }
            if op == "does" || op == "but"
    )
}

/// True if a nested `BEGIN` phaser may be hoisted above `stmt`. Only pure reads
/// and bare (uninitialized) declarations qualify: a BEGIN runs at compile time
/// and its effects should be visible to such reads that textually precede it.
/// Anything that declares a compile-time entity (`class`/`sub`/`has`/...),
/// assigns, or initializes a variable is a *barrier* — those are part of the
/// same source/compile-time ordering the BEGIN must not jump over.
fn stmt_is_hoist_safe(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Say(_) | Stmt::Put(_) | Stmt::Print(_) | Stmt::Note(_) => true,
        // A bare routine call (including test-function listops like `is`/`ok`)
        // is a runtime read the BEGIN may run before.
        Stmt::Call { .. } => true,
        Stmt::SetLine(_) => true,
        Stmt::Use { .. } => true,
        // A bare declaration (no initializer) creates a container but performs
        // no runtime work, so a BEGIN may safely run before it.
        Stmt::VarDecl { expr, .. } => is_empty_vardecl_init(expr),
        // A plain expression statement is a read unless it is an assignment or a
        // `does`/`but` role mixin. A declaration-level mixin (`my @a does R1`,
        // desugared to `VarDecl @a; @a does R1`) applies the role to the
        // container as a compile-time effect that a following BEGIN must see, so
        // it is a barrier the BEGIN may not hoist above.
        Stmt::Expr(e) => !matches!(e, Expr::AssignExpr { .. }) && !is_mixin_expr(e),
        _ => false,
    }
}

/// True if a `VarDecl` initializer is the synthesized empty/absent default
/// (`my $x;` / `my @a;` / `my %h;`), i.e. the declaration has no real init.
fn is_empty_vardecl_init(expr: &Expr) -> bool {
    use crate::value::ValueView;
    match expr {
        Expr::Literal(v) if v.is_nil() => true,
        Expr::Literal(v) => matches!(v.view(), ValueView::Array(items, _) if items.is_empty()),
        Expr::Hash(items, _) => items.is_empty(),
        _ => false,
    }
}

/// True if a statement declares a variable, either directly (`VarDecl`) or
/// nested inside a `SyntheticBlock` (as produced by `will <phaser>` traits).
fn stmt_declares_var(stmt: &Stmt) -> bool {
    crate::ast::scope_members(std::slice::from_ref(stmt)).any(|s| matches!(s, Stmt::VarDecl { .. }))
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

/// Marks a declaration whose initializer reads its static cell (ADR-0134).
pub(crate) const BEGIN_STATIC_TRAIT: &str = "__begin_static";

/// Marks the static half of a `:D`-typed scalar declaration (`my Int:D $x =
/// 3`). Its container holds the nominal type object (`Int`), which the `:D`
/// constraint itself rejects, so the compiler stores the value first and
/// registers the constraint after it, the order an `is default` trait already
/// uses. The run-time assignment that follows is checked as usual.
pub(crate) const BEGIN_STATIC_DEFINITE_TRAIT: &str = "__begin_static_definite";

/// The nominal type a `:D` scalar declaration's static half holds, for a
/// constraint whose base is a plain type name (`Int:D`, `Foo::Bar:D`).
fn static_definite_type(name: &str, type_constraint: Option<&str>) -> Option<String> {
    if name.starts_with(['@', '%', '&']) {
        return None;
    }
    let base = type_constraint?.strip_suffix(":D")?;
    // A plain (possibly package-qualified) type name, not a parameterized
    // or otherwise composite constraint.
    let plain = base.starts_with(|c: char| c.is_alphabetic() || c == '_')
        && base
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | ':'));
    plain.then(|| base.to_string())
}

/// Split a `VarDecl` into its *static* declaration and the run-time assignment
/// of its initializer, if it has one. The static half is the container holding
/// what an uninitialized declaration of its sigil holds; it is what a BEGIN-time
/// effect observes (ADR-0134 §2.1.2), while the assignment stays at the
/// declaration's source position. `None` for any other statement, and for a
/// `constant`, whose initializer is itself a BEGIN-time effect.
pub(crate) fn split_var_decl(stmt: &Stmt) -> Option<(Stmt, Option<Stmt>)> {
    let Stmt::VarDecl {
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
    } = stmt
    else {
        return None;
    };
    if custom_traits.iter().any(|(t, _)| t == "__constant") {
        return None;
    }
    // A declaration a lifted BEGIN gave a static cell (ADR-0134 slice 2) is
    // already its static half: its initializer reads the static value.
    if custom_traits.iter().any(|(t, _)| t == BEGIN_STATIC_TRAIT) {
        return Some((stmt.clone(), None));
    }
    // A bare `my @a;`/`my %h;` (no explicit initializer) still parses with a
    // sigil-based default literal (`Literal(Array([]))` / `Literal(Hash({}))`),
    // not `Literal(NIL)` — so testing the initializer expression against a NIL
    // literal wrongly treats those as "has an initializer" and splices a
    // spurious `@a = []` reset after the static declaration, clobbering any
    // mutation a BEGIN performed on the array/hash in between. The parser
    // already marks a real explicit initializer with the `__has_initializer`
    // custom trait (checked the same way at e.g. `compiler/stmt.rs`'s
    // `has_init` sites) — use that instead of guessing from the expression shape.
    let has_init = custom_traits
        .iter()
        .any(|(n, _)| n == "__has_initializer" || n == "__scalar_bind");
    // The static declaration's value (before any real initializer runs) must
    // match what an uninitialized declaration of this sigil actually holds:
    // `Nil` for a scalar, but an EMPTY container for `@`/`%` — never a raw
    // `Nil` literal. Compiling a `@`-sigil VarDecl with a `Literal(NIL)`
    // initializer goes through the same path as an explicit `@a = Nil`
    // assignment, which itemizes the Nil into a ONE-ELEMENT array `[(Any)]`
    // instead of leaving the array empty (S02-types/assigning-refs.t semantics
    // for `@a = Nil` are correct there — they just don't apply to "no
    // initializer at all").
    // A native type the static half cannot give a zero to (a NativeCall
    // `ulong`, say) keeps its declaration whole: it holds no Nil.
    if let Some(tc) = type_constraint.as_deref()
        && tc.starts_with(|c: char| c.is_ascii_lowercase())
        && crate::runtime::Interpreter::native_scalar_default(tc).is_none()
        && !name.starts_with(['@', '%'])
    {
        return None;
    }
    let static_default = match name.as_bytes().first() {
        Some(b'@') => Expr::Literal(Value::real_array(Vec::new())),
        Some(b'%') => Expr::Literal(Value::hash_with_data(Value::hash_arc(ValueMap::default()))),
        // A native scalar (`my int $x`) holds its zero, never Nil.
        _ => Expr::Literal(
            type_constraint
                .as_deref()
                .and_then(crate::runtime::Interpreter::native_scalar_default)
                .unwrap_or(Value::NIL),
        ),
    };
    // A `:D` scalar's static state is its nominal type object, as in Rakudo
    // (`my Int:D $x = 3; BEGIN say $x.raku` prints `Int`). It is not an
    // initializer, so it carries no `__has_initializer` mark.
    let (static_default, static_traits) =
        match static_definite_type(name, type_constraint.as_deref())
            .filter(|_| has_init && !*is_our)
        {
            Some(nominal) => {
                let mut traits: Vec<_> = custom_traits
                    .iter()
                    .filter(|(t, _)| t != "__has_initializer" && t != "__scalar_bind")
                    .cloned()
                    .collect();
                traits.push((BEGIN_STATIC_DEFINITE_TRAIT.to_string(), None));
                (Expr::BareWord(nominal), traits)
            }
            None => (static_default, custom_traits.clone()),
        };
    let static_decl = Stmt::VarDecl {
        name: name.clone(),
        expr: static_default,
        type_constraint: type_constraint.clone(),
        is_state: *is_state,
        is_our: *is_our,
        is_dynamic: *is_dynamic,
        is_export: *is_export,
        export_tags: export_tags.clone(),
        custom_traits: static_traits,
        where_constraint: where_constraint.clone(),
    };
    let assign = has_init.then(|| Stmt::Assign {
        name: name.clone(),
        expr: init_expr.clone(),
        op: AssignOp::Assign,
        target_is_sigilless: false,
    });
    Some((static_decl, assign))
}
