//! `is default(...)` storage.
//!
//! A lexical's default is a property of ONE declaration, so it lives in the
//! env under `__mutsu_var_default::<name>` ([`MetaNs::VarDefault`]) and is
//! scoped exactly like the declaration's type constraint (`MetaNs::Type`):
//! a block's `my $x is default(3)` disappears with the block, and a closure
//! that captured a defaulted `$x` keeps seeing that default after an
//! unrelated same-named `my $x` elsewhere. A process-wide name-keyed table
//! got both of those wrong (#10796).
//!
//! An attribute's default is different: method dispatch registers it under
//! the twigil names a method body reads it by (`!x`, `.x`, `@!x`, ...), which
//! no lexical can carry, so those stay in the name-keyed
//! `Interpreter::attr_var_defaults` table.

use super::*;
use crate::meta_ns::MetaNs;

/// Process-global, monotonic: set the first time any lexical `is default(...)`
/// is registered on ANY interpreter. Gates the env probe in
/// [`Interpreter::var_default`], which sits on Nil-store paths of every scalar.
///
/// Process-global rather than per-interpreter for the reason
/// `ENV_TYPE_CONSTRAINT_SEEN` is: an interpreter that adopts another one's env
/// (nested EVAL, `clone_for_thread`) inherits its `__mutsu_var_default::*` keys
/// but starts with fresh fields. An over-set only makes the (correct) probe run.
static VAR_DEFAULT_SEEN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Whether `name` is one of the twigil names attribute defaults are
/// registered under (`!x`, `.x`, `@!x`, `%.x`, ...), as against a lexical.
// Cost: O(1).
fn is_attr_twigil_name(name: &str) -> bool {
    matches!(
        name.as_bytes(),
        [b'!' | b'.', ..] | [b'$' | b'@' | b'%' | b'&', b'!' | b'.', ..]
    )
}

impl Interpreter {
    /// Set the default value for a variable declared with `is default(...)`,
    /// in the current scope.
    // Cost: O(1) expected.
    pub(crate) fn set_var_default(&mut self, name: &str, value: Value) {
        if is_attr_twigil_name(name) {
            self.attr_var_defaults.insert(name.to_string(), value);
            self.attr_var_defaults_epoch += 1;
            return;
        }
        VAR_DEFAULT_SEEN.store(true, std::sync::atomic::Ordering::Relaxed);
        let key = MetaNs::VarDefault.key(Symbol::intern(name));
        self.env.insert_sym(key, value);
    }

    /// Whether method dispatch's attribute-default registration for
    /// `(owner_class, receiver_class)` is still in effect: nothing has
    /// changed `attr_var_defaults` or the class tables since it last ran. The
    /// registration re-derives the same values on every call otherwise —
    /// two table probes per attribute plus the six-name check per defaulted
    /// one, on each of the dozens of calls a Text::CSV field makes (#9494).
    // Cost: O(1) expected.
    pub(crate) fn attr_var_defaults_are_current(
        &self,
        owner_class: &str,
        receiver_class: &str,
    ) -> bool {
        let key = (Symbol::intern(owner_class), Symbol::intern(receiver_class));
        self.attr_var_defaults_current.get(&key)
            == Some(&(
                self.attr_var_defaults_epoch,
                self.registry().method_generation,
            ))
    }

    /// Record that `(owner_class, receiver_class)`'s attribute defaults were
    /// just registered (see [`Self::attr_var_defaults_are_current`]).
    // Cost: O(1) expected.
    pub(crate) fn note_attr_var_defaults_current(
        &mut self,
        owner_class: &str,
        receiver_class: &str,
    ) {
        let key = (Symbol::intern(owner_class), Symbol::intern(receiver_class));
        let stamp = (
            self.attr_var_defaults_epoch,
            self.registry().method_generation,
        );
        self.attr_var_defaults_current.insert(key, stamp);
    }

    /// Register `value` as the `is default(...)` of attribute `attr_name` under
    /// every name a method body reads it by (`$!x`, `$.x`, and the `@`/`%`
    /// forms, so `.VAR.default` works on container attributes too).
    ///
    /// Method dispatch re-registers the receiver class's attribute defaults on
    /// every call, and the value is nearly always the one already registered,
    /// so an unchanged entry is left alone: re-inserting it built and dropped
    /// six key strings per defaulted attribute per call — Text::CSV's
    /// `CSV::Field` (five `is default` attributes) paid ~30 of them on each of
    /// the dozens of method calls a parsed CSV field makes (#9494).
    // Cost: O(1) expected (six hash probes; an insert only on a changed value).
    pub(crate) fn set_attr_var_defaults(&mut self, attr_name: &str, value: Value) {
        let names = crate::qualified::attr_twigil_names(Symbol::intern(attr_name));
        for name in names {
            let name = name.as_str();
            if self
                .attr_var_defaults
                .get(name)
                .is_some_and(|old| crate::vm::vm_method_dispatch::cheaply_unchanged(old, &value))
            {
                continue;
            }
            self.attr_var_defaults
                .insert(name.to_string(), value.clone());
            self.attr_var_defaults_epoch += 1;
        }
    }

    /// Whether any variable in this program may carry an `is default(...)`
    /// trait. When false, no store has a default to substitute for a `Nil` and
    /// no declaration has an inherited one to clear — the gate the plain-scalar
    /// store fast path asks before committing.
    // Cost: O(1).
    #[inline(always)]
    pub(crate) fn has_var_defaults(&self) -> bool {
        VAR_DEFAULT_SEEN.load(std::sync::atomic::Ordering::Relaxed)
            || !self.attr_var_defaults.is_empty()
    }

    /// The `is default(...)` value of the variable `name` resolves to in the
    /// current scope, if it was declared with one.
    // Cost: O(1) expected.
    pub(crate) fn var_default(&self, name: &str) -> Option<&Value> {
        if is_attr_twigil_name(name) {
            if self.attr_var_defaults.is_empty() {
                return None;
            }
            return self.attr_var_defaults.get(name);
        }
        if !VAR_DEFAULT_SEEN.load(std::sync::atomic::Ordering::Relaxed) {
            return None;
        }
        self.env
            .get_sym(MetaNs::VarDefault.key(Symbol::intern(name)))
    }

    /// Drop the `is default(...)` a redeclared `name` would otherwise inherit
    /// from an enclosing same-named variable. Called on every `my`
    /// declaration; the declaration's own trait, if any, re-sets it right
    /// after. Scoped like the set: the enclosing variable keeps its default.
    // Cost: O(1) expected.
    pub(crate) fn clear_var_default(&mut self, name: &str) {
        if is_attr_twigil_name(name) {
            if self.attr_var_defaults.remove(name).is_some() {
                self.attr_var_defaults_epoch += 1;
            }
            return;
        }
        // `is default(...)` is rare: the common program never registers one,
        // and the key build plus env probe would be pure waste on the hottest
        // declaration path.
        if !VAR_DEFAULT_SEEN.load(std::sync::atomic::Ordering::Relaxed) {
            return;
        }
        let key = MetaNs::VarDefault.key(Symbol::intern(name));
        if self.env.contains_key_sym(key) {
            self.env.remove_sym(key);
        }
    }
}
