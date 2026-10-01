//! The unit-level BEGIN prologue (ADR-0134, slices 1 to 3).
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
//! list. Everything up to and including the last top-level BEGIN-time effect
//! is partitioned into two parts:
//!
//! - the **prologue**, which holds the `BEGIN` phasers themselves, the
//!   declarations a BEGIN can observe (`use`, routines, packages, types,
//!   `constant`), and the *static* half of each variable declaration;
//! - the **run-time remainder**: every other statement, plus the initializer
//!   of each split variable declaration as an assignment at its original
//!   position.
//!
//! A `use` (other than a positional pragma such as `use strict`) and a
//! `constant` are BEGIN-time effects too (slice 3), so the bound reaches the
//! last of them as well. Statements after the last effect are left as they
//! are. No effect observes them, so moving them would only change run-time
//! order without gaining anything.
//!
//! A class or package declaration moves without its body's run-time
//! statements: those stay at the declaration's position ([`package_body`]).
//! Rakudo composes the class at BEGIN time but runs `class A { say 2 }`'s
//! `say` at run time.
//!
//! BEGINs nested inside a top-level statement, and value-form BEGINs, are
//! lifted into the prologue by [`nested`] ahead of that statement.

mod nested;
mod package_body;

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{Visit, walk_expr};
use crate::value::ValueView;
use std::collections::HashSet;

/// Split `stmts` (one compilation unit's top level) into its BEGIN prologue and
/// run-time remainder, as described in the module docs. The prologue is
/// returned, and `stmts` is left holding the remainder. If the unit has no
/// BEGIN-time effect, the prologue is empty and `stmts` is
/// untouched.
pub(crate) fn take_unit_prologue(stmts: &mut Vec<Stmt>) -> Vec<Stmt> {
    // Lift the BEGINs nested in each top-level statement first (slice 2):
    // each lifted effect joins the prologue just ahead of its statement.
    let unit_names = unit_lexical_names(stmts);
    let mut lifted = nested::Lifted::default();
    let mut effects: Vec<Vec<Stmt>> = Vec::with_capacity(stmts.len());
    // The compile-time composition of the `our` types declared in each
    // statement's code (#10494) is a BEGIN-time effect too. It is collected
    // after the lift, so a type declared in a lifted BEGIN registers there
    // alone.
    let mut type_shells: Vec<Option<Stmt>> = Vec::with_capacity(stmts.len());
    let mut composes_role = false;
    for stmt in stmts.iter_mut() {
        let before = lifted.effects.len();
        nested::lift_in_stmt(stmt, &unit_names, &mut lifted);
        effects.push(lifted.effects.split_off(before));
        let shells = crate::compiler::nested_type_decls(stmt);
        composes_role |= shells
            .iter()
            .any(|shell| crate::compiler::nested_decl_composes_role(&shell.decl));
        type_shells.push((!shells.is_empty()).then_some(Stmt::NestedTypeShells(shells)));
    }
    // Only a composition runs user code (a role body), so only then does it
    // matter where among the unit's statements the shells run.
    // TODO: place every unit's shells here and drop the compiler's head-of-
    // unit fallback (`hoist_nested_type_decl_shells`). Extending the
    // prologue's bound over every unit with a nested type exposes partition
    // bugs that make that unsafe for now (#10524).
    if !composes_role {
        type_shells.iter_mut().for_each(|shells| *shells = None);
    }
    let decls = lifted.decls;
    // A `use` and a `constant` are BEGIN-time effects on their own (slice 3),
    // so the prologue reaches the last of them too.
    let last_effect = stmts.iter().rposition(is_begin_time_effect);
    let last_lifted = effects.iter().rposition(|e| !e.is_empty());
    let last_shell = type_shells.iter().rposition(Option::is_some);
    let Some(last) = last_effect.max(last_lifted).max(last_shell) else {
        return Vec::new();
    };
    let tail = stmts.split_off(last + 1);
    let mut prologue = decls;
    let mut rest = Vec::new();
    for ((stmt, stmt_effects), shells) in std::mem::take(stmts)
        .into_iter()
        .zip(effects)
        .zip(type_shells)
    {
        prologue.extend(stmt_effects);
        partition_stmt(stmt, &mut prologue, &mut rest);
        // After the statement's own declaration part: a class whose methods
        // declare the nested types is registered by then.
        prologue.extend(shells);
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
    if is_begin_time_stmt(&stmt) && !is_positional_pragma(&stmt) {
        // Carry the line marker with the statement, so an error raised in the
        // prologue still names the statement's own line.
        if let Some(line @ Stmt::SetLine(_)) = rest.last() {
            prologue.push(line.clone());
        }
        // A class or package body keeps its run-time statements in place.
        let (stmt, runtime) = package_body::split_package_decl(stmt);
        prologue.push(stmt);
        rest.extend(runtime);
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
    let stmt = match stmt {
        Stmt::SyntheticBlock(inner) if is_will_begin_group(&inner) => {
            partition_will_begin(inner, prologue, rest);
            return;
        }
        other => other,
    };
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

// Cost: O(n), n = size of the expression tree (closures excluded).
fn collect_static_requires(expr: &Expr, out: &mut Vec<String>) {
    let mut scan = StaticRequires(out);
    scan.visit_expr(expr);
}

/// The walk of [`collect_static_requires`] (ADR-0137 visitor).
struct StaticRequires<'a>(&'a mut Vec<String>);

impl Visit for StaticRequires<'_> {
    // A statement reached from an expression sits in a nested block, closure
    // or `do`, whose `require` belongs to that scope (see
    // [`static_require_targets`]).
    fn visit_stmt(&mut self, _stmt: &Stmt) {}

    // A parameter default belongs to its closure's scope too.
    fn visit_param(&mut self, _param: &crate::ast::ParamDef) {}

    fn visit_expr(&mut self, expr: &Expr) {
        if let Expr::Call { name, args } = expr
            && name.resolve() == "require"
            && let Some(Expr::Literal(target)) = args.first()
            && let ValueView::Package(module) = target.view()
        {
            self.0.push(module.resolve());
        }
        walk_expr(self, expr);
    }
}

/// A statement that extends the prologue's bound: a `BEGIN`, a `constant`, and
/// every module load (`use`, `need`, `import`) except a positional pragma
/// (ADR-0134 §2.1.1). A conditional `use` (`use Foo:if(EXPR)`) is one of them;
/// its condition is evaluated in the prologue too.
fn is_begin_time_effect(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Phaser { kind, .. } => *kind == PhaserKind::Begin,
        Stmt::Use { .. } | Stmt::Need { .. } | Stmt::Import { .. } => !is_positional_pragma(stmt),
        Stmt::VarDecl { custom_traits, .. } => custom_traits.iter().any(|(t, _)| t == "__constant"),
        Stmt::SyntheticBlock(inner) => is_will_begin_group(inner),
        _ => false,
    }
}

/// A lexical pragma that mutsu applies as run-time state at the statement's
/// own position (`use strict`, `no strict`, `use fatal`, `use soft`, ...). It
/// stays where it is: moving it into the prologue would switch the mode on for
/// the run-time statements that precede it. Every lowercase pragma counts,
/// except the ones a later BEGIN-time effect depends on: `use lib` extends the
/// search path the prologue's loads resolve against, and `use if` enables the
/// `:if` adverb of a later conditional `use`.
fn is_positional_pragma(stmt: &Stmt) -> bool {
    let module = match stmt {
        Stmt::Use { module, .. } | Stmt::No { module, .. } | Stmt::Need { module } => module,
        _ => return false,
    };
    module.starts_with(|c: char| c.is_ascii_lowercase()) && !matches!(module.as_str(), "lib" | "if")
}

/// `my $x will begin { ... }` parses to a `SyntheticBlock` of the declaration
/// followed by its trait phasers. Its `begin` phaser is a BEGIN-time effect.
fn is_will_begin_group(inner: &[Stmt]) -> bool {
    matches!(inner.first(), Some(Stmt::VarDecl { .. }))
        && inner
            .iter()
            .skip(1)
            .all(|s| matches!(s, Stmt::Phaser { .. }))
        && inner.iter().any(|s| {
            matches!(
                s,
                Stmt::Phaser {
                    kind: PhaserKind::Begin,
                    ..
                }
            )
        })
}

/// Split a `will begin` group: the declaration's static half and each `begin`
/// phaser go to the prologue, in source order, with `$_` bound to the declared
/// container. The initializer and the other trait phasers stay at run time.
fn partition_will_begin(inner: Vec<Stmt>, prologue: &mut Vec<Stmt>, rest: &mut Vec<Stmt>) {
    let mut members = inner.into_iter();
    let Some(decl) = members.next() else {
        return;
    };
    let name = match &decl {
        Stmt::VarDecl { name, .. } => name.clone(),
        _ => {
            rest.push(decl);
            rest.extend(members);
            return;
        }
    };
    partition_stmt(decl, prologue, rest);
    let mut later = Vec::new();
    for member in members {
        match member {
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                mut body,
                condition,
                end_index,
            } => {
                if let [Stmt::Given { topic, .. }] = body.as_mut_slice() {
                    *topic = Expr::Var(name.clone());
                }
                prologue.push(Stmt::Phaser {
                    kind: PhaserKind::Begin,
                    body,
                    condition,
                    end_index,
                });
            }
            other => later.push(other),
        }
    }
    rest.extend(later);
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
        // An exported type declaration (`class C is export { }`) arrives as the
        // declaration followed by its `__MUTSU_EXPORT_TYPE__` marker; the pair is
        // one declaration.
        Stmt::SyntheticBlock(inner) => {
            !inner.is_empty()
                && inner.iter().all(|s| {
                    is_begin_time_stmt(s)
                        || matches!(s, Stmt::Expr(Expr::Call { name, .. })
                            if name.resolve() == "__MUTSU_EXPORT_TYPE__")
                })
                && inner.iter().any(is_begin_time_stmt)
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn requires(src: &str) -> Vec<String> {
        let stmts = crate::parse_dispatch::parse_source(src)
            .map(|(stmts, _)| stmts)
            .unwrap();
        stmts.iter().flat_map(static_require_targets).collect()
    }

    #[test]
    fn a_require_anywhere_in_the_statement_expression_is_found() {
        assert_eq!(requires("my $x = (require Foo);"), vec!["Foo"]);
        assert_eq!(requires("my %h = a => (require Foo);"), vec!["Foo"]);
        assert_eq!(requires("f(:x(require Foo));"), vec!["Foo"]);
    }

    #[test]
    fn a_require_in_a_nested_block_or_closure_belongs_to_that_scope() {
        assert!(requires("my $c = { require Foo };").is_empty());
        assert!(requires("my $c = -> $x = (require Foo) { };").is_empty());
        assert!(requires("my $x = do { require Foo };").is_empty());
    }
}
