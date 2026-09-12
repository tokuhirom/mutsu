//! The env ceremony around one grammar action-method call.
//!
//! An action method sees `$<name>` and `$0`..`$9` for *its own* match, so both
//! `invoke_grammar_actions` and its lazy leaf fast path
//! ([`Interpreter::invoke_leaf_action_lazy`]) have to hide whatever the parent
//! action left under those names, install this match's, and put the parent's
//! back afterwards. That save/restore used to be spelled inline — twice, once
//! per call site — as an `O(env)` scan filtering `k.starts_with("<") &&
//! k.ends_with(">")` plus `for i in 0..10 { env.remove(&i.to_string()) }`.
//!
//! Both spellings ask a question the symbol table already answers. Filtering
//! the visible env resolves EVERY key to a `&'static str` (a thread-local
//! round trip) and scans its bytes twice, and the digit loop allocates ten
//! `String`s and interns ten symbols per action. On a YAML parse that was
//! ~5,300 instructions per scan (two per action) and 25 interns per action —
//! together ~1.8% of the whole program
//! ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
//!
//! The capture-shape registry (`crate::symbol::capture_shaped_symbols`) is a
//! superset of the capture keys any env can hold — a key must be interned
//! before it can BE a key — so walking it and probing the overlay is
//! `O(capture names)`, a handful, with no allocation and no string scan. That
//! is the same reasoning `Interpreter::reset_capture_env_vars` already runs
//! on, and this module puts the action path on it too.

use super::*;
use crate::symbol::Symbol;

/// `$0`..`$9` as interned symbols, built once per process.
///
/// Ten is not a limit on captures — [`positional_key_sym`] interns beyond it —
/// only on what is worth pre-interning. The save/restore window the two call
/// sites use is `0..10`, so those twenty lookups are all served from here.
static POSITIONAL_KEY_SYMS: std::sync::LazyLock<[Symbol; 10]> =
    std::sync::LazyLock::new(|| std::array::from_fn(|i| Symbol::intern(&i.to_string())));

/// The env key a positional capture `$i` is stored under, as a `Symbol`.
pub(crate) fn positional_key_sym(i: usize) -> Symbol {
    match POSITIONAL_KEY_SYMS.get(i) {
        Some(sym) => *sym,
        None => Symbol::intern(&i.to_string()),
    }
}

/// How many positional capture slots an action call saves and restores.
pub(super) const SAVED_POSITIONAL_SLOTS: usize = 10;

impl Interpreter {
    /// Take the `$<name>` bindings the *parent* action left in this frame's
    /// overlay, removing them so they cannot leak into the action about to run.
    ///
    /// Overlay-only, exactly like the `env.iter()` scan it replaces: a binding
    /// inherited from an enclosing call frame is not this action's to hide.
    pub(super) fn take_action_named_captures(&mut self) -> Vec<(Symbol, Value)> {
        let (_, angle_keys) = crate::symbol::capture_shaped_symbols();
        let saved: Vec<(Symbol, Value)> = angle_keys
            .iter()
            .filter_map(|key| self.env.overlay_get_sym(*key).map(|v| (*key, v.clone())))
            .collect();
        for (key, _) in &saved {
            self.env.remove_sym(*key);
        }
        saved
    }

    /// Drop whatever `$<name>` bindings the action installed and put the
    /// parent's back.
    ///
    /// The registry is re-read here rather than reused from
    /// [`Self::take_action_named_captures`]: the action may have interned a
    /// capture name that did not exist when the save ran, and the scan this
    /// replaces saw those too.
    pub(super) fn restore_action_named_captures(&mut self, saved: Vec<(Symbol, Value)>) {
        let (_, angle_keys) = crate::symbol::capture_shaped_symbols();
        for key in angle_keys {
            if self.env.overlay_get_sym(key).is_some() {
                self.env.remove_sym(key);
            }
        }
        for (key, value) in saved {
            self.env.insert_sym(key, value);
        }
    }

    /// Take the parent action's `$0`..`$9`, clearing them for this action.
    pub(super) fn take_action_positional_captures(&mut self) -> Vec<Option<Value>> {
        (0..SAVED_POSITIONAL_SLOTS)
            .map(|i| self.env.remove_sym(positional_key_sym(i)))
            .collect()
    }

    /// Clear this action's `$0`..`$9` and put the parent's back.
    pub(super) fn restore_action_positional_captures(&mut self, saved: Vec<Option<Value>>) {
        for i in 0..SAVED_POSITIONAL_SLOTS {
            self.env.remove_sym(positional_key_sym(i));
        }
        for (i, value) in saved.into_iter().enumerate() {
            if let Some(value) = value {
                self.env.insert_sym(positional_key_sym(i), value);
            }
        }
    }
}
