//! The unit-level BEGIN prologue (ADR-0134, slice 1).
//!
//! Rakudo runs a `BEGIN` block as soon as the parser reaches its end, before
//! any run-time code of the compilation unit. The block sees every lexical in
//! its *static* state: declared, but with no run-time initializer applied.
//! mutsu parses a whole unit before running it, so it gets the same order by
//! moving the unit's BEGIN-time effects to the head of the unit, in source
//! order. The effects then run first, in the unit's own frame, and share its
//! lexical slots. No second environment exists to keep in sync.
//!
//! [`take_unit_prologue`] performs that split on a unit's top-level statement
//! list. Everything up to and including the last top-level statement-form
//! `BEGIN` is partitioned into two parts:
//!
//! - the **prologue**, which holds the `BEGIN` phasers themselves, the
//!   declarations a BEGIN can observe (`use`, routines, packages, types,
//!   `constant`), and the *static* half of each variable declaration;
//! - the **run-time remainder**: every other statement, plus the initializer
//!   of each split variable declaration as an assignment at its original
//!   position.
//!
//! Statements after the last `BEGIN` are left as they are. No BEGIN observes
//! them, so moving them would only change run-time order without gaining
//! anything. Slice 3 widens the prologue to every `use` and `constant`, which
//! do not need a `BEGIN` to be BEGIN-time effects.
//!
//! Known residue of this slice: a class or module body is a declaration and
//! moves whole, so a bare run-time statement *inside* such a body (`class A {
//! say 2 }`) runs with the prologue rather than in its source position. Rakudo
//! composes the class at BEGIN time but runs that statement at run time.
//! Splitting a package body is slice 2's static-cell machinery.

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::value::ValueView;

/// Split `stmts` (one compilation unit's top level) into its BEGIN prologue and
/// run-time remainder, as described in the module docs. The prologue is
/// returned, and `stmts` is left holding the remainder. If the unit has no
/// top-level statement-form `BEGIN`, the prologue is empty and `stmts` is
/// untouched.
pub(crate) fn take_unit_prologue(stmts: &mut Vec<Stmt>) -> Vec<Stmt> {
    let Some(last_begin) = stmts.iter().rposition(is_begin_phaser) else {
        return Vec::new();
    };
    let tail = stmts.split_off(last_begin + 1);
    let mut prologue = Vec::new();
    let mut rest = Vec::new();
    for stmt in std::mem::take(stmts) {
        partition_stmt(stmt, &mut prologue, &mut rest);
    }
    rest.extend(tail);
    *stmts = rest;
    prologue
}

/// Put a compilation unit's BEGIN prologue at its head. This is the whole of
/// the reordering a module's top level gets; the mainline and EVAL get it as
/// part of `phasers::reorder_phasers`. Returns the prologue's length.
pub(crate) fn order_unit(stmts: &mut Vec<Stmt>) -> usize {
    let mut prologue = take_unit_prologue(stmts);
    let prologue_len = prologue.len();
    if prologue_len > 0 {
        prologue.append(stmts);
        *stmts = prologue;
    }
    prologue_len
}

fn partition_stmt(stmt: Stmt, prologue: &mut Vec<Stmt>, rest: &mut Vec<Stmt>) {
    if is_begin_time_stmt(&stmt) {
        // Carry the line marker with the statement, so an error raised in the
        // prologue still names the statement's own line.
        if let Some(line @ Stmt::SetLine(_)) = rest.last() {
            prologue.push(line.clone());
        }
        prologue.push(stmt);
        return;
    }
    // A `require` of a statically named module installs a stub package under
    // that name at BEGIN time, even though the load itself happens at run time.
    // That stub is Rakudo's `package Foo {}`, so the prologue declares one.
    // The real load then fills it in.
    for name in static_require_targets(&stmt) {
        prologue.push(Stmt::Package {
            name: crate::symbol::Symbol::intern(&name),
            body: Vec::new(),
            kind: crate::ast::PackageKind::Package,
            is_unit: false,
            is_my: false,
        });
    }
    if let Some((static_decl, assign)) = crate::runtime::phasers::split_var_decl(&stmt) {
        prologue.push(static_decl);
        rest.extend(assign);
        return;
    }
    // A group declaration `my ($a, @b);` arrives as a `SyntheticBlock` of plain
    // declarations. It splits member by member.
    if let Stmt::SyntheticBlock(inner) = &stmt
        && !inner.is_empty()
        && inner.iter().all(|s| matches!(s, Stmt::VarDecl { .. }))
    {
        let Stmt::SyntheticBlock(inner) = stmt else {
            unreachable!()
        };
        for member in inner {
            partition_stmt(member, prologue, rest);
        }
        return;
    }
    rest.push(stmt);
}

/// The statically named targets of the `require` expressions in a run-time
/// statement, excluding file paths. Only the statement's own expression tree
/// is searched: a `require` inside a nested block or closure belongs to that
/// scope, which this slice does not reorder (ADR-0134 slice 2).
fn static_require_targets(stmt: &Stmt) -> Vec<String> {
    let mut out = Vec::new();
    match stmt {
        Stmt::Expr(e) | Stmt::VarDecl { expr: e, .. } | Stmt::Assign { expr: e, .. } => {
            collect_static_requires(e, &mut out)
        }
        _ => {}
    }
    out
}

fn collect_static_requires(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Call { name, args } => {
            if name.resolve() == "require"
                && let Some(Expr::Literal(target)) = args.first()
                && let ValueView::Package(module) = target.view()
            {
                out.push(module.resolve());
            }
            for arg in args {
                collect_static_requires(arg, out);
            }
        }
        Expr::Grouped(inner)
        | Expr::Unary { expr: inner, .. }
        | Expr::PostfixOp { expr: inner, .. }
        | Expr::AssignExpr { expr: inner, .. } => collect_static_requires(inner, out),
        Expr::Binary { left, right, .. } => {
            collect_static_requires(left, out);
            collect_static_requires(right, out);
        }
        Expr::MethodCall { target, args, .. } => {
            collect_static_requires(target, out);
            for arg in args {
                collect_static_requires(arg, out);
            }
        }
        Expr::ArrayLiteral(items) => {
            for item in items {
                collect_static_requires(item, out);
            }
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_static_requires(cond, out);
            collect_static_requires(then_expr, out);
            collect_static_requires(else_expr, out);
        }
        _ => {}
    }
}

fn is_begin_phaser(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::Phaser {
            kind: PhaserKind::Begin,
            ..
        }
    )
}

/// A statement whose whole effect happens at BEGIN time in Rakudo: the `BEGIN`
/// phaser itself, a module load, and every declarator a BEGIN can observe.
fn is_begin_time_stmt(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Phaser { kind, .. } => *kind == PhaserKind::Begin,
        Stmt::VarDecl { custom_traits, .. } => custom_traits.iter().any(|(t, _)| t == "__constant"),
        // A `unit module Foo;` is a body-less marker that puts every following
        // top-level statement into `Foo`; it has to precede them in the prologue
        // just as it does in the source.
        Stmt::Package { .. }
        | Stmt::Use { .. }
        | Stmt::No { .. }
        | Stmt::Need { .. }
        | Stmt::Import { .. }
        | Stmt::SubDecl { .. }
        | Stmt::ProtoDecl { .. }
        | Stmt::TokenDecl { .. }
        | Stmt::RuleDecl { .. }
        | Stmt::ProtoToken { .. }
        | Stmt::ClassDecl { .. }
        | Stmt::RoleDecl { .. }
        | Stmt::EnumDecl { .. }
        | Stmt::SubsetDecl { .. }
        | Stmt::AugmentClass { .. } => true,
        _ => false,
    }
}

impl crate::runtime::Interpreter {
    /// Run only a unit's BEGIN prologue. Used when a post-parse check rejects
    /// the unit: in Rakudo the BEGIN-time effects already ran while the unit
    /// was being parsed, before that check could report (ADR-0134 §2.1.5).
    pub(crate) fn run_begin_prologue_only(
        &mut self,
        prologue: &[Stmt],
    ) -> Result<(), crate::value::RuntimeError> {
        let mut compiler = crate::compiler::Compiler::new();
        compiler.set_current_package(self.current_package());
        compiler.is_mainline = true;
        let (code, compiled_fns) = compiler.compile(prologue);
        self.run_top(&code, &compiled_fns).map(|_| ())
    }
}
