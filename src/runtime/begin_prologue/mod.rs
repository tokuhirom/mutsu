//! The unit-level BEGIN prologue (ADR-0134, slices 1 and 2).
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
//!
//! BEGINs nested inside a top-level statement, and value-form BEGINs, are
//! lifted into the prologue by [`nested`] ahead of that statement.

mod nested;

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::value::ValueView;
use std::collections::HashSet;

/// Split `stmts` (one compilation unit's top level) into its BEGIN prologue and
/// run-time remainder, as described in the module docs. The prologue is
/// returned, and `stmts` is left holding the remainder. If the unit has no
/// top-level statement-form `BEGIN`, the prologue is empty and `stmts` is
/// untouched.
pub(crate) fn take_unit_prologue(stmts: &mut Vec<Stmt>) -> Vec<Stmt> {
    // Lift the BEGINs nested in each top-level statement first (slice 2):
    // each lifted effect joins the prologue just ahead of its statement.
    let unit_names = unit_lexical_names(stmts);
    let mut lifted = nested::Lifted::default();
    let mut effects: Vec<Vec<Stmt>> = Vec::with_capacity(stmts.len());
    for stmt in stmts.iter_mut() {
        let before = lifted.effects.len();
        nested::lift_in_stmt(stmt, &unit_names, &mut lifted);
        effects.push(lifted.effects.split_off(before));
    }
    let decls = lifted.decls;
    // `use` and `constant` are BEGIN-time effects on their own (slice 3), so
    // the prologue reaches the last of them too.
    let last_effect = stmts.iter().rposition(is_begin_time_effect);
    let last_lifted = effects.iter().rposition(|e| !e.is_empty());
    let Some(last) = last_effect.max(last_lifted) else {
        return Vec::new();
    };
    let tail = stmts.split_off(last + 1);
    let mut prologue = decls;
    let mut rest = Vec::new();
    for (stmt, stmt_effects) in std::mem::take(stmts).into_iter().zip(effects) {
        prologue.extend(stmt_effects);
        partition_stmt(stmt, &mut prologue, &mut rest);
    }
    rest.extend(tail);
    *stmts = rest;
    prologue
}

/// The lexical names a unit declares at its top level, in `VarDecl` naming
/// (`x`, `@a`, `&f`). A lifted BEGIN may read these, because the prologue runs
/// in the unit's frame.
fn unit_lexical_names(stmts: &[Stmt]) -> HashSet<String> {
    let mut names = HashSet::new();
    for stmt in stmts {
        match stmt {
            Stmt::VarDecl { name, .. } => {
                names.insert(name.clone());
            }
            Stmt::SyntheticBlock(inner) => names.extend(unit_lexical_names(inner)),
            Stmt::SubDecl { name, .. } => {
                names.insert(format!("&{}", name.resolve()));
            }
            _ => {}
        }
    }
    names
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
    let stmt = match stmt {
        Stmt::Use {
            module,
            arg,
            tags,
            condition: Some(condition),
        } => {
            prologue.extend(if_condition_check(*condition));
            Stmt::Use {
                module,
                arg,
                tags,
                condition: Some(Box::new(Expr::Var(IF_CONDITION_SLOT.to_string()))),
            }
        }
        other => other,
    };
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

/// A statement that is itself a BEGIN-time effect rather than a declaration a
/// BEGIN may observe: a `BEGIN`, a module load, or a `constant`.
fn is_begin_time_effect(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Phaser { kind, .. } => *kind == PhaserKind::Begin,
        Stmt::VarDecl { custom_traits, .. } => custom_traits.iter().any(|(t, _)| t == "__constant"),
        Stmt::Use { .. } | Stmt::No { .. } | Stmt::Need { .. } | Stmt::Import { .. } => true,
        _ => false,
    }
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

/// The unit-level slot a conditional `use` reads its evaluated `:if` value from.
const IF_CONDITION_SLOT: &str = "__begin_use_if";

/// `use Foo:if(EXPR)` under the `if` pragma evaluates `EXPR` as a BEGIN-time
/// effect (ADR-0134 §2.1.6). Running in the prologue, it sees lexicals in
/// their static state, so a condition that only a run-time assignment would
/// define is undefined here. That is the rakudo `if` module's compile error.
/// Each conditional `use` stores its value in the same slot just before the
/// `use` reads it, so one slot serves them all.
fn if_condition_check(condition: Expr) -> Vec<Stmt> {
    let slot = || Expr::Var(IF_CONDITION_SLOT.to_string());
    vec![
        Stmt::VarDecl {
            name: IF_CONDITION_SLOT.to_string(),
            expr: condition,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: vec![],
            custom_traits: vec![("__has_initializer".to_string(), None)],
            where_constraint: None,
        },
        Stmt::If {
            cond: Expr::Unary {
                op: crate::token_kind::TokenKind::Bang,
                expr: Box::new(Expr::MethodCall {
                    target: Box::new(slot()),
                    name: crate::symbol::Symbol::intern("defined"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                }),
            },
            then_branch: vec![Stmt::Die(Expr::Literal(crate::value::Value::str(
                "Did not provide compile-time-value for :if adverb in use statement".to_string(),
            )))],
            else_branch: vec![],
            binding_var: None,
            is_statement_modifier: true,
            is_unless: false,
            with_kind: None,
        },
    ]
}
