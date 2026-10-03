//! Env bindings the compiled engine keeps live across a sub-pattern or a call
//! (ADR-0135 Slice E):
//!
//! - a spliced Regex value that closed over its own scope
//!   (`CaptureIsolatedGroupScoped`) runs its body with that scope installed;
//! - a `<subrule>` call frame runs its callee with the call's binding window
//!   installed: the callee's `$*` parameters and the object or closure
//!   arguments baking cannot carry into its code blocks
//!   (`install_subrule_dynamic_params`), and the rule's own `:my $*x`
//!   declarations (`enter_grammar_rule_dynvars`), which the walk installs
//!   around the callee's whole match.
//!
//! The body is part of the run, so the run can backtrack into it after leaving
//! it, and out of it before it finished. Each install and uninstall is
//! therefore also an entry on the register trail, under a tag no register
//! index reaches: rewinding past an install uninstalls, and rewinding past an
//! uninstall installs again. The trail is not touched by a ratchet's cut, so a
//! committed body still unwinds its bindings when the run backtracks past it.
//! Whatever is still installed when the run ends (it failed inside a body
//! whose trail entries a settled return or an empty stack had already dropped)
//! is uninstalled by [`Interpreter::rx_scopes_unwind`].

use std::sync::Arc;

use crate::runtime::Interpreter;
use crate::runtime::regex::regex_dynparams::SavedDynParams;
use crate::runtime::regex::regex_helpers::{grammar_dynvar_scope_pop, grammar_dynvar_scope_push};
use crate::runtime::seq_helpers::RegexClosureBinding;
use crate::symbol::Symbol;
use crate::value::{Value, ValueMap};

/// Register-trail tag: undo an install (uninstall).
pub(super) const UNDO_ENTER: usize = usize::MAX;
/// Register-trail tag: undo an uninstall (install again).
pub(super) const UNDO_EXIT: usize = usize::MAX - 1;

/// One set of bindings the run has installed at least once.
enum ScopeSave {
    /// A Regex value's closure scope, and what its install shadowed while it
    /// is installed.
    Closure {
        scope: Arc<ValueMap>,
        saved: Option<Vec<RegexClosureBinding>>,
    },
    /// A call's binding window. While installed, `saved` holds what each key
    /// shadowed (in install order); while not, `live` holds the values the
    /// window had when it was last uninstalled, so a write the callee's code
    /// made to a `$*` parameter survives backtracking into the callee.
    Window {
        live: Vec<(String, Option<Value>)>,
        saved: Option<SavedDynParams>,
        /// The keys whose values the callee's Match records.
        attach: Vec<String>,
        /// The rule's `:my $*x` declarations, marked as owned by a live rule
        /// frame while the window is installed.
        scope_keys: Option<Vec<String>>,
        /// The routine frame `(caller package, rule)` on the routine stack
        /// while the window is installed.
        routine: Option<(Symbol, Symbol)>,
    },
}

/// A call's binding window, as `rx_call_resolve` installed it.
pub(super) struct CallWindow {
    /// What each binding shadowed, in install order.
    pub(super) saved: SavedDynParams,
    /// The keys whose final values the callee's Match records for its action
    /// (`attach_grammar_dynvars_to_named_caps`).
    pub(super) attach: Vec<String>,
    /// The rule's own `:my $*x` declarations, when it has any. Not marked yet:
    /// [`Interpreter::rx_window_adopt`] marks them.
    pub(super) scope_keys: Option<Vec<String>>,
    /// A routine frame `(caller package, rule)` for the callee, while some
    /// method carries a `.wrap`: a wrapper reads its caller's rule name from
    /// a Backtrace (#9151), as from the frame the walk's eager arm pushes
    /// around each call (`subrule_candidate_ends_with_frame`). Not pushed
    /// yet: [`Interpreter::rx_window_adopt`] pushes it.
    pub(super) routine: Option<(Symbol, Symbol)>,
}

/// The bindings of one run, indexed by the handle an install returns (and the
/// trail entries carry).
#[derive(Default)]
pub(super) struct Scopes {
    saves: Vec<ScopeSave>,
}

impl Interpreter {
    /// Install `scope` and record it; the index is the scope's handle.
    // Cost: O(b), b = the scope's bindings (one env insert each).
    pub(super) fn rx_scope_enter(&mut self, scopes: &mut Scopes, scope: &Arc<ValueMap>) -> usize {
        let saved = self.install_env_scope(scope);
        scopes.saves.push(ScopeSave::Closure {
            scope: Arc::clone(scope),
            saved: Some(saved),
        });
        scopes.saves.len() - 1
    }

    /// Record a call's binding window, which `install_subrule_dynamic_params`
    /// has just installed (`saved` is what it shadowed); the index is its
    /// handle.
    // Cost: O(1).
    pub(super) fn rx_window_adopt(&mut self, scopes: &mut Scopes, window: CallWindow) -> usize {
        let CallWindow {
            saved,
            attach,
            scope_keys,
            routine,
        } = window;
        if let Some(keys) = &scope_keys {
            grammar_dynvar_scope_push(keys.iter().cloned());
        }
        if let Some((pkg, name)) = routine {
            self.rx_push_rule_routine(pkg, name);
        }
        scopes.saves.push(ScopeSave::Window {
            live: Vec::new(),
            saved: Some(saved),
            attach,
            scope_keys,
            routine,
        });
        scopes.saves.len() - 1
    }

    /// Push the routine frame of a rule invoked from `pkg`.
    // Cost: O(1).
    fn rx_push_rule_routine(&mut self, pkg: Symbol, name: Symbol) {
        let (line, file) = (self.current_source_line(), self.executing_source_file_sym());
        self.push_routine_with_location(pkg, name, line, file, None);
    }

    /// The current values of the call window `k`'s bindings, while it is
    /// installed. The callee's action runs later, in the reduce walk, and must
    /// still see its `$*` parameters (CSS::Specification's `usage` action reads
    /// `$*USAGE`), so its return records them on its Match, as the walk does
    /// (`attach_grammar_dynvars_to_named_caps`).
    // Cost: O(b), b = the bindings.
    pub(super) fn rx_window_values(&self, scopes: &Scopes, k: usize) -> Vec<(String, Value)> {
        match scopes.saves.get(k) {
            Some(ScopeSave::Window {
                saved: Some(_),
                attach,
                ..
            }) => attach
                .iter()
                .filter_map(|key| self.env.get(key).map(|v| (key.clone(), v.clone())))
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Uninstall the bindings `k`.
    // Cost: O(b), b = the bindings.
    pub(super) fn rx_scope_exit(&mut self, scopes: &mut Scopes, k: usize) {
        match scopes.saves.get_mut(k) {
            Some(ScopeSave::Closure { saved, .. }) => {
                let saved = saved.take();
                self.uninstall_regex_closure_scope(saved);
            }
            Some(ScopeSave::Window {
                live,
                saved,
                scope_keys,
                routine,
                ..
            }) => {
                let Some(shadowed) = saved.take() else {
                    return;
                };
                if scope_keys.is_some() {
                    grammar_dynvar_scope_pop();
                }
                if routine.is_some() {
                    self.routine_stack.pop();
                }
                live.clear();
                live.extend(
                    shadowed
                        .iter()
                        .map(|(key, _)| (key.clone(), self.env.get(key).cloned())),
                );
                self.restore_subrule_dynamic_params(shadowed);
            }
            None => {}
        }
    }

    /// Install the bindings `k` again, after an uninstall.
    // Cost: O(b), b = the bindings.
    fn rx_scope_reinstall(&mut self, scopes: &mut Scopes, k: usize) {
        match scopes.saves.get_mut(k) {
            Some(ScopeSave::Closure { scope, saved }) => {
                if saved.is_some() {
                    return;
                }
                let scope = Arc::clone(scope);
                let installed = self.install_env_scope(&scope);
                if let Some(ScopeSave::Closure { saved, .. }) = scopes.saves.get_mut(k) {
                    *saved = Some(installed);
                }
            }
            Some(ScopeSave::Window {
                live,
                saved,
                scope_keys,
                routine,
                ..
            }) => {
                if saved.is_some() {
                    return;
                }
                if let Some(keys) = scope_keys {
                    grammar_dynvar_scope_push(keys.iter().cloned());
                }
                if let Some((pkg, name)) = *routine {
                    let (line, file) =
                        (self.current_source_line(), self.executing_source_file_sym());
                    self.push_routine_with_location(pkg, name, line, file, None);
                }
                let mut shadowed = Vec::with_capacity(live.len());
                for (key, value) in live.iter() {
                    shadowed.push((key.clone(), self.env.get(key).cloned()));
                    match value {
                        Some(value) => {
                            self.env.insert(key.clone(), value.clone());
                        }
                        None => {
                            self.env.remove(key);
                        }
                    }
                }
                *saved = Some(shadowed);
            }
            None => {}
        }
    }

    /// Undo one tagged register-trail entry for the bindings `k`: an install
    /// is undone by uninstalling, an uninstall by installing again.
    // Cost: O(b), b = the bindings.
    pub(super) fn rx_scope_undo(&mut self, scopes: &mut Scopes, tag: usize, k: usize) {
        if tag == UNDO_ENTER {
            self.rx_scope_exit(scopes, k);
        } else {
            self.rx_scope_reinstall(scopes, k);
        }
    }

    /// Uninstall every binding still installed, innermost first.
    // Cost: O(s·b), s = the binding sets the run installed, b = their bindings.
    pub(super) fn rx_scopes_unwind(&mut self, scopes: &mut Scopes) {
        for k in (0..scopes.saves.len()).rev() {
            self.rx_scope_exit(scopes, k);
        }
    }
}
