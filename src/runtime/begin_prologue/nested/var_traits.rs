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
const REPLAYED_KINDS: [(PhaserKind, &str); 4] = [
    (PhaserKind::Enter, "ENTER"),
    (PhaserKind::Leave, "LEAVE"),
    (PhaserKind::Keep, "KEEP"),
    (PhaserKind::Undo, "UNDO"),
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

impl Walker<'_> {
    /// Lifts the trait application of the scalar declaration `stmt`, the
    /// statement at `index` of its scope. Returns whether it was lifted; when
    /// it was, `stmt` is left as the declaration's static half and bound.
    pub(super) fn lift_var_traits(&mut self, stmt: &mut Stmt, index: usize) -> bool {
        if self.frames.is_empty() || self.in_package() {
            return false;
        }
        let Stmt::VarDecl {
            name,
            is_state: false,
            is_our: false,
            is_export: false,
            custom_traits,
            where_constraint: None,
            ..
        } = &*stmt
        else {
            return false;
        };
        if name.starts_with('&')
            || custom_traits.iter().any(|(t, _)| {
                matches!(
                    t.as_str(),
                    "__has_initializer" | "__scalar_bind" | "__constant"
                )
            })
        {
            return false;
        }
        let applied: Vec<_> = custom_traits
            .iter()
            .filter(|(t, _)| !t.starts_with("__"))
            .collect();
        if applied.is_empty() || !applied.iter().all(|(t, _)| is_user_trait(t)) {
            return false;
        }
        let name = name.clone();
        let mut body = vec![Stmt::Expr(read_var(&name))];
        // `Variable.block.add_phaser` files its phaser under this site; the
        // declaration replays them on every entry.
        let site = super::next_slot("__var_trait_site_");
        let site_call = |site: Option<&str>| {
            Stmt::Expr(Expr::Call {
                name: crate::symbol::Symbol::intern(APPLY_VAR_TRAIT_CALL),
                args: vec![
                    Expr::Literal(crate::value::Value::str(name.clone())),
                    Expr::Literal(crate::value::Value::str(VAR_TRAIT_SITE.to_string())),
                    Expr::Literal(match site {
                        Some(s) => crate::value::Value::str(s.to_string()),
                        None => crate::value::Value::NIL,
                    }),
                ],
                listop: false,
            })
        };
        body.push(site_call(Some(&site)));
        for (trait_name, arg) in applied {
            let mut args = vec![
                Expr::Literal(crate::value::Value::str(name.clone())),
                Expr::Literal(crate::value::Value::str(trait_name.clone())),
            ];
            args.extend(arg.clone());
            body.push(Stmt::Expr(Expr::Call {
                name: crate::symbol::Symbol::intern(APPLY_VAR_TRAIT_CALL),
                args,
                listop: false,
            }));
        }
        body.push(site_call(None));
        let mut stripped = stmt.clone();
        if let Stmt::VarDecl { custom_traits, .. } = &mut stripped {
            custom_traits.retain(|(t, _)| t.starts_with("__"));
            custom_traits.push((VAR_TRAIT_SEEDED.to_string(), None));
        }
        self.bind_decl(&stripped, Some(index));
        if self.lift(&body, None, &PhaserKind::Begin) {
            // The scope's own phaser queues replay what the handlers add
            // through `Variable.block.add_phaser`, ahead of its body.
            for (kind, replay) in REPLAYED_KINDS {
                let call = Stmt::Expr(Expr::Call {
                    name: crate::symbol::Symbol::intern(APPLY_VAR_TRAIT_CALL),
                    args: vec![
                        Expr::Literal(crate::value::Value::str(name.clone())),
                        Expr::Literal(crate::value::Value::str(format!(
                            "{VAR_TRAIT_REPLAY}{replay}"
                        ))),
                        Expr::Literal(crate::value::Value::str(site.clone())),
                    ],
                    listop: false,
                });
                self.current_frame().entry.push(Stmt::Phaser {
                    kind: kind.clone(),
                    body: vec![call],
                    condition: None,
                    end_index: None,
                });
            }
            *stmt = stripped;
            return true;
        }
        self.current_frame().bindings.pop();
        false
    }
}
