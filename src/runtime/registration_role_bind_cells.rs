//! The declaration-site cells a role body's `:=` declarations bind
//! (`RoleDef::body_bind_cells`, #11087), put in scope for each run of the
//! role's deferred body at composition.

use super::*;
use crate::symbol::Symbol;

/// One name [`Interpreter::enter_role_body_bind_cells`] rebound, with what to
/// put back when the body statement is done.
pub(crate) struct RoleBindCellSaved {
    sym: Symbol,
    prior: Option<Value>,
    was_pending_writeback: bool,
}

impl Interpreter {
    /// Bind each of `role_name`'s body-bind cells under its env name for one
    /// run of a deferred body statement, returning what each name held before
    /// so [`Self::leave_role_body_bind_cells`] can restore it.
    ///
    /// The role body runs in the composing scope, so a bare `$z` there would
    /// otherwise resolve to the composer's own `$z` (or a fresh-binding
    /// marker for one it has not assigned yet), not the declaration site's.
    // Cost: O(c), c = the role body's bind sources.
    pub(crate) fn enter_role_body_bind_cells(&mut self, role_name: &str) -> Vec<RoleBindCellSaved> {
        let cells = match self.registry().roles.get(role_name) {
            Some(role) if !role.body_bind_cells.is_empty() => role.body_bind_cells.clone(),
            _ => return Vec::new(),
        };
        let mut saved = Vec::with_capacity(cells.len());
        for (sym, cell) in cells {
            saved.push(RoleBindCellSaved {
                sym,
                prior: self.env.get_sym(sym).cloned(),
                was_pending_writeback: sym
                    .with_str(|name| self.pending_caller_var_writeback.contains(name)),
            });
            self.env.insert_sym(sym, cell);
        }
        saved
    }

    /// Undo [`Self::enter_role_body_bind_cells`]. A write the body made
    /// through a cell stays in the cell; only the composer's own binding of
    /// each name comes back. The body chunk recorded its bind as a caller-var
    /// write of the name, but that write went to the declaration site's
    /// cell, not the composer's same-named variable, so the record is dropped
    /// (unless a write of the composer's own was already pending) -- draining
    /// it would copy the restored env entry over the composer's slot.
    // Cost: O(c), c = the role body's bind sources.
    pub(crate) fn leave_role_body_bind_cells(&mut self, saved: Vec<RoleBindCellSaved>) {
        for RoleBindCellSaved {
            sym,
            prior,
            was_pending_writeback,
        } in saved.into_iter().rev()
        {
            if !was_pending_writeback {
                sym.with_str(|name| self.pending_caller_var_writeback.remove(name));
            }
            match prior {
                Some(value) => {
                    self.env.insert_sym(sym, value);
                }
                None => {
                    self.env.remove_sym(sym);
                }
            }
        }
    }
}
