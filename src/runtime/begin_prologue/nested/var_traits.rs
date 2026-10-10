//! A user variable trait as a BEGIN-time effect (ADR-0134 §5, #12278).
//!
//! In Raku a `trait_mod:<is>(Variable ...)` handler runs while the declaring
//! block is being compiled, so a handler that reads state a `BEGIN` left behind
//! sees the state in effect at the declaration. mutsu applies the trait when
//! the declaration executes, which is after every `BEGIN` of the unit has run.
//!
//! The declaration `my $x is foo(1);` of a nested scope is lifted the way a
//! `BEGIN` is: the trait application becomes a prologue effect over `$x`'s
//! static cell, ahead of every later `BEGIN`, and the declaration itself keeps
//! only the static half, starting from the cell on each entry.

use super::cell_ast::read_var;
use super::Walker;
use crate::ast::{Expr, PhaserKind, Stmt};
use crate::runtime::phasers::APPLY_VAR_TRAIT_CALL;

/// The synthetic trait that sets (or, given Nil, clears) the declaration site
/// the lifted traits that follow file their `Variable.block` phasers under.
pub(crate) const VAR_TRAIT_SITE: &str = "__site";

/// The prefix of the synthetic trait that replays one kind of phaser (the
/// suffix) filed under the site given as its argument.
pub(crate) const VAR_TRAIT_REPLAY: &str = "__replay_phasers:";

/// The marker of a lifted declaration whose block's ENTER queue may seed the
/// variable: the declaration stashes the slot's value before it runs and puts
/// a defined one back after (`VAR_TRAIT_SEED_STASH` / `_RESTORE`).
pub(crate) const VAR_TRAIT_SEEDED: &str = "__seeded_decl";
pub(crate) const VAR_TRAIT_SEED_STASH: &str = "__seed_stash";
pub(crate) const VAR_TRAIT_SEED_RESTORE: &str = "__seed_restore";

/// The block phasers a lifted declaration's scope gets, with the kind name
/// `Variable.block.add_phaser` files each under.
const REPLAYED_KINDS: [(PhaserKind, &str); 9] = [
    (PhaserKind::Enter, "ENTER"),
    (PhaserKind::Leave, "LEAVE"),
    (PhaserKind::Keep, "KEEP"),
    (PhaserKind::Undo, "UNDO"),
    (PhaserKind::First, "FIRST"),
    (PhaserKind::Next, "NEXT"),
    (PhaserKind::Last, "LAST"),
    (PhaserKind::Pre, "PRE"),
    (PhaserKind::Post, "POST"),
];

/// Traits the VM applies itself, or that mark the declaration for something
/// other than a `trait_mod:<is>` handler. They are never lifted.
const BUILTIN_VARIABLE_TRAITS: &[&str] = &[
    "default", "rw", "readonly", "required", "raw", "copy", "built", "dynamic", "export", "buf",
    "blob", "leaf", "nodal", "pure",
];

/// Whether `name` is a trait only a user `trait_mod:<is>` can claim.
fn is_user_trait(name: &str) -> bool {
    !name.starts_with("__")
        && name.starts_with(|c: char| c.is_ascii_lowercase())
        && !BUILTIN_VARIABLE_TRAITS.contains(&name)
}

/// The scalar declaration `stmt` and the user traits it applies, when the
/// BEGIN prologue can apply them ahead of the declaration.
/// The user traits a declaration applies, each with its argument.
type AppliedTraits = Vec<(String, Option<Expr>)>;

fn liftable_traits(stmt: &Stmt) -> Option<(String, AppliedTraits)> {
    let Stmt::VarDecl {
        name,
        is_state: false,
        is_our: false,
        is_export: false,
        custom_traits,
        where_constraint: None,
        ..
    } = stmt
    else {
        return None;
    };
    if name.starts_with('&')
        || custom_traits.iter().any(|(t, _)| {
            matches!(
                t.as_str(),
                "__has_initializer" | "__scalar_bind" | "__constant"
            )
        })
    {
        return None;
    }
    let applied: Vec<_> = custom_traits
        .iter()
        .filter(|(t, _)| !t.starts_with("__"))
        .cloned()
        .collect();
    if applied.is_empty() || !applied.iter().all(|(t, _)| is_user_trait(t)) {
        return None;
    }
    Some((name.clone(), applied))
}

/// The prologue statements that apply `applied` to the variable `name`, with
/// `Variable.block.add_phaser` filing under `site`.
fn trait_body(name: &str, applied: &[(String, Option<Expr>)], site: &str) -> Vec<Stmt> {
    let call = |args: Vec<Expr>| {
        Stmt::Expr(Expr::Call {
            name: crate::symbol::Symbol::intern(APPLY_VAR_TRAIT_CALL),
            args,
            listop: false,
        })
    };
    let lit = |s: &str| Expr::Literal(crate::value::Value::str(s.to_string()));
    let mut body = vec![Stmt::Expr(read_var(name))];
    body.push(call(vec![lit(name), lit(VAR_TRAIT_SITE), lit(site)]));
    for (trait_name, arg) in applied {
        let mut args = vec![lit(name), lit(trait_name)];
        args.extend(arg.clone());
        body.push(call(args));
    }
    body.push(call(vec![
        lit(name),
        lit(VAR_TRAIT_SITE),
        Expr::Literal(crate::value::Value::NIL),
    ]));
    body
}

/// The call that replays the `replay`-kind phasers filed under `site` for
/// the variable `name`.
fn replay_call(name: &str, site: &str, replay: &str) -> Stmt {
    let lit = |s: String| Expr::Literal(crate::value::Value::str(s));
    Stmt::Expr(Expr::Call {
        name: crate::symbol::Symbol::intern(APPLY_VAR_TRAIT_CALL),
        args: vec![
            lit(name.to_string()),
            lit(format!("{VAR_TRAIT_REPLAY}{replay}")),
            lit(site.to_string()),
        ],
        listop: false,
    })
}

/// The `ENTER`/`LEAVE`/`KEEP`/`UNDO` phasers a scope with a lifted declaration
/// of `name` gets: each replays the phasers of its kind filed under `site`.
fn entry_phasers(name: &str, site: &str) -> Vec<Stmt> {
    REPLAYED_KINDS
        .iter()
        .map(|(kind, replay)| {
            let mut body = vec![replay_call(name, site, replay)];
            // Rakudo runs a `PRE`/`POST` a trait added without checking its
            // verdict; the phaser's own condition always holds.
            let condition = matches!(kind, PhaserKind::Pre | PhaserKind::Post).then(|| {
                body.push(Stmt::Expr(Expr::Literal(crate::value::Value::TRUE)));
                crate::symbol::Symbol::intern("{ ... }")
            });
            Stmt::Phaser {
                kind: kind.clone(),
                body,
                condition,
                end_index: None,
            }
        })
        .collect()
}

/// Gives the body of a class or package the phaser queues its variable traits
/// ask for (ADR-12131). The body runs when its declaration does, so the
/// declaration with a user trait moves to the head of the body with the traits
/// applied right after it, then the `ENTER` replay as the first statement of
/// the body and `LEAVE`/`KEEP`/`UNDO` phasers for the exit.
// Cost: O(n), n = number of statements in the body.
fn hoist_package_var_traits(body: &mut Vec<Stmt>) {
    if !body.iter().any(|s| liftable_traits(s).is_some()) {
        return;
    }
    let mut head = Vec::new();
    let mut exit = Vec::new();
    let mut rest = Vec::with_capacity(body.len());
    for stmt in std::mem::take(body) {
        let Some((name, applied)) = liftable_traits(&stmt) else {
            rest.push(stmt);
            continue;
        };
        let site = super::next_slot("__var_trait_site_");
        let mut decl = stmt;
        if let Stmt::VarDecl { custom_traits, .. } = &mut decl {
            custom_traits.retain(|(t, _)| t.starts_with("__"));
        }
        head.push(decl);
        head.extend(trait_body(&name, &applied, &site));
        head.push(replay_call(&name, &site, "ENTER"));
        exit.extend(entry_phasers(&name, &site).into_iter().skip(1));
    }
    head.append(&mut exit);
    head.append(&mut rest);
    *body = head;
}

struct PackageVarTraits;

impl crate::ast_visit::VisitMut for PackageVarTraits {
    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        if let Stmt::ClassDecl { body, .. } | Stmt::Package { body, .. } = stmt {
            hoist_package_var_traits(body);
        }
        crate::ast_visit::walk_stmt_mut(self, stmt);
    }
}

/// [`hoist_package_var_traits`] for every class and package body in `stmts`.
// Cost: O(n), n = size of the unit's AST.
pub(crate) fn lift_package_var_traits(stmts: &mut Vec<Stmt>) {
    use crate::ast_visit::VisitMut;
    PackageVarTraits.visit_stmts_mut(stmts);
}

/// Lifts the traits of the unit-level scalar declarations in `stmts` (a
/// compilation unit's top level) into BEGIN phasers, and gives the unit's own
/// phaser queues the replays. A declaration without an initializer becomes its
/// static half followed by a `BEGIN` that applies the traits over it, so the
/// prologue runs the handlers before the mainline's `ENTER` queue does. Must
/// run before the unit's block phasers are split off.
// Cost: O(n), n = number of statements in the unit's top level.
pub(crate) fn lift_unit_var_traits(stmts: &mut Vec<Stmt>) {
    if !stmts.iter().any(|s| liftable_traits(s).is_some()) {
        return;
    }
    let mut out = Vec::with_capacity(stmts.len() + 2);
    let mut entry = Vec::new();
    for stmt in std::mem::take(stmts) {
        let Some((name, applied)) = liftable_traits(&stmt) else {
            out.push(stmt);
            continue;
        };
        let site = super::next_slot("__var_trait_site_");
        let mut decl = stmt;
        if let Stmt::VarDecl { custom_traits, .. } = &mut decl {
            custom_traits.retain(|(t, _)| t.starts_with("__"));
        }
        out.push(decl);
        out.push(Stmt::Phaser {
            kind: PhaserKind::Begin,
            body: trait_body(&name, &applied, &site),
            condition: None,
            end_index: None,
        });
        entry.extend(entry_phasers(&name, &site));
    }
    entry.append(&mut out);
    *stmts = entry;
}

impl Walker<'_> {
    /// Lifts the trait application of the scalar declaration `stmt`, the
    /// statement at `index` of its scope. Returns whether it was lifted; when
    /// it was, `stmt` is left as the declaration's static half and bound.
    pub(super) fn lift_var_traits(&mut self, stmt: &mut Stmt, index: usize) -> bool {
        if self.frames.is_empty() || self.in_detached_type() {
            return false;
        }
        let Some((name, applied)) = liftable_traits(stmt) else {
            return false;
        };
        // `Variable.block.add_phaser` files its phaser under this site; the
        // scope's own phaser queues replay them.
        let site = super::next_slot("__var_trait_site_");
        let body = trait_body(&name, &applied, &site);
        let mut stripped = stmt.clone();
        if let Stmt::VarDecl { custom_traits, .. } = &mut stripped {
            custom_traits.retain(|(t, _)| t.starts_with("__"));
            custom_traits.push((VAR_TRAIT_SEEDED.to_string(), None));
        }
        self.bind_decl(&stripped, Some(index));
        if self.lift(&body, None, &PhaserKind::Begin) {
            self.current_frame().entry.extend(entry_phasers(&name, &site));
            *stmt = stripped;
            return true;
        }
        self.current_frame().bindings.pop();
        false
    }
}
