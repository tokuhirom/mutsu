//! Closure scopes the compiled engine keeps live across a sub-pattern
//! (ADR-0135 Slice E): a spliced Regex value that closed over its own scope
//! (`CaptureIsolatedGroupScoped`) runs its body with that scope installed in
//! the env.
//!
//! The body is part of the program, so the run can backtrack into it after
//! leaving it, and out of it before it finished. Each install and uninstall is
//! therefore also an entry on the register trail, under a tag no register
//! index reaches: rewinding past a `ScopeEnter` uninstalls the scope, and
//! rewinding past a `ScopeExit` installs it again. The trail is not touched by
//! a ratchet's cut, so a committed body still unwinds its scope when the run
//! backtracks past it. Whatever is still installed when the run ends (it failed
//! inside a body whose trail entries a settled return or an empty stack had
//! already dropped) is uninstalled by [`Interpreter::rx_scopes_unwind`].

use std::sync::Arc;

use crate::runtime::Interpreter;
use crate::runtime::seq_helpers::RegexClosureBinding;
use crate::value::ValueMap;

/// Register-trail tag: undo a `ScopeEnter` (uninstall the scope).
pub(super) const UNDO_ENTER: usize = usize::MAX;
/// Register-trail tag: undo a `ScopeExit` (install the scope again).
pub(super) const UNDO_EXIT: usize = usize::MAX - 1;

/// One scope the run has installed at least once.
struct ScopeSave {
    scope: Arc<ValueMap>,
    /// What the install shadowed, while the scope is installed.
    saved: Option<Vec<RegexClosureBinding>>,
}

/// The scopes of one run, indexed by the value a `ScopeEnter` keeps in its
/// register (and the trail entries carry).
#[derive(Default)]
pub(super) struct Scopes {
    saves: Vec<ScopeSave>,
}

impl Interpreter {
    /// Install `scope` and record it; the index is the scope's handle.
    // Cost: O(b), b = the scope's bindings (one env insert each).
    pub(super) fn rx_scope_enter(&mut self, scopes: &mut Scopes, scope: &Arc<ValueMap>) -> usize {
        let saved = self.install_env_scope(scope);
        scopes.saves.push(ScopeSave {
            scope: Arc::clone(scope),
            saved: Some(saved),
        });
        scopes.saves.len() - 1
    }

    /// Uninstall the scope `k`.
    // Cost: O(b), b = the scope's bindings.
    pub(super) fn rx_scope_exit(&mut self, scopes: &mut Scopes, k: usize) {
        if let Some(save) = scopes.saves.get_mut(k) {
            let saved = save.saved.take();
            self.uninstall_regex_closure_scope(saved);
        }
    }

    /// Undo one tagged register-trail entry for the scope `k`: an enter is
    /// undone by uninstalling, an exit by installing again.
    // Cost: O(b), b = the scope's bindings.
    pub(super) fn rx_scope_undo(&mut self, scopes: &mut Scopes, tag: usize, k: usize) {
        let Some(save) = scopes.saves.get_mut(k) else {
            return;
        };
        if tag == UNDO_ENTER {
            let saved = save.saved.take();
            self.uninstall_regex_closure_scope(saved);
        } else {
            let scope = Arc::clone(&save.scope);
            let saved = self.install_env_scope(&scope);
            if let Some(save) = scopes.saves.get_mut(k) {
                save.saved = Some(saved);
            }
        }
    }

    /// Uninstall every scope still installed, innermost first.
    // Cost: O(s·b), s = the scopes the run installed, b = their bindings.
    pub(super) fn rx_scopes_unwind(&mut self, scopes: &mut Scopes) {
        for save in scopes.saves.iter_mut().rev() {
            if let Some(saved) = save.saved.take() {
                self.uninstall_regex_closure_scope(Some(saved));
            }
        }
    }
}
