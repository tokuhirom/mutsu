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
        let mut stripped = stmt.clone();
        if let Stmt::VarDecl { custom_traits, .. } = &mut stripped {
            custom_traits.retain(|(t, _)| t.starts_with("__"));
        }
        self.bind_decl(&stripped, Some(index));
        if self.lift(&body, None, &PhaserKind::Begin) {
            *stmt = stripped;
            return true;
        }
        self.current_frame().bindings.pop();
        false
    }
}
