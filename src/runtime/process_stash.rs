//! The process-wide `PROCESS::` stash (ADR-11318).
//!
//! Rakudo keeps the process-level dynamics (`$*OUT`, `$*ERR`, `$*IN`, a
//! `PROCESS::<$x>` install, ...) in one stash for the whole process: a write to
//! `$PROCESS::OUT` — or a `$*OUT = ...` that finds no `my $*OUT` in the dynamic
//! scope and so lands on the process container — is seen by every thread,
//! including one that was already running. mutsu seeds the built-in dynamics
//! into each interpreter's own env base tier (ADR-0086), so a write kept there
//! never reached a thread that already existed.
//!
//! [`ProcessStash`] is that one store. Every interpreter of a lineage shares it
//! (`clone_for_thread` hands the child an `Arc` clone), a process-level write
//! goes ONLY to it (never into a frame's env, so no env ever holds a stale
//! copy of a published value), and a read that resolves to the process binding
//! is redirected to it. "Resolves to the process binding" is decided by
//! identity: the env still holds the process value as it was before the first
//! write — the base-tier seed — so a binding whose value is that very object,
//! with no `my $*X` declared in the dynamic scope, is the process binding.
//! Anything else (a `my $*X`, a dynamic parameter bound to another object, a
//! `start` block's inherited redirection) is a lexical binding and wins.

use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;
use crate::value::identity::values_same_object;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

/// One published process-level dynamic.
struct Entry {
    /// The current process-level value.
    value: Value,
    /// The value the process binding held before the first write (the
    /// interpreter's base-tier seed), if it had one. An env entry that is
    /// still this object is a stale copy of the process binding.
    initial: Option<Value>,
}

#[derive(Default)]
struct Inner {
    /// Set by the first write and never cleared: a program that never writes
    /// a process-level dynamic pays one relaxed load per dynamic read.
    populated: AtomicBool,
    entries: RwLock<rustc_hash::FxHashMap<String, Entry>>,
}

/// The `PROCESS::` stash shared by every interpreter of one lineage. Keyed by
/// the env spelling of the dynamic: `*name`, `@*name`, `%*name`.
#[derive(Clone, Default)]
pub(crate) struct ProcessStash(Arc<Inner>);

impl ProcessStash {
    /// Whether any process-level dynamic has been written.
    // Cost: O(1).
    #[inline]
    pub(crate) fn is_populated(&self) -> bool {
        self.0.populated.load(Ordering::Relaxed)
    }

    fn read(&self) -> std::sync::RwLockReadGuard<'_, rustc_hash::FxHashMap<String, Entry>> {
        self.0.entries.read().unwrap_or_else(|e| e.into_inner())
    }

    /// The current value of `key`.
    // Cost: O(|key|), one hash probe under the read lock.
    pub(crate) fn get(&self, key: &str) -> Option<Value> {
        if !self.is_populated() {
            return None;
        }
        self.read().get(key).map(|e| e.value.clone())
    }

    /// The current value of `key` and the value it held before the first write.
    // Cost: O(|key|), one hash probe under the read lock.
    fn get_with_initial(&self, key: &str) -> Option<(Value, Option<Value>)> {
        self.read()
            .get(key)
            .map(|e| (e.value.clone(), e.initial.clone()))
    }

    // Cost: O(|key|), one hash probe under the read lock.
    pub(crate) fn contains(&self, key: &str) -> bool {
        self.is_populated() && self.read().contains_key(key)
    }

    /// Publish `value` as the process-level `key`. `initial` supplies the
    /// pre-write value; it is only consulted on the key's first write.
    // Cost: O(|key|), one hash probe under the write lock.
    pub(crate) fn set(&self, key: &str, value: Value, initial: impl FnOnce() -> Option<Value>) {
        let mut entries = self.0.entries.write().unwrap_or_else(|e| e.into_inner());
        match entries.get_mut(key) {
            Some(entry) => entry.value = value,
            None => {
                let initial = initial();
                entries.insert(key.to_string(), Entry { value, initial });
            }
        }
        self.0.populated.store(true, Ordering::Relaxed);
    }

    /// Every published `(key, value)` pair.
    // Cost: O(n), n = published keys.
    pub(crate) fn entries(&self) -> Vec<(String, Value)> {
        if !self.is_populated() {
            return Vec::new();
        }
        self.read()
            .iter()
            .map(|(k, e)| (k.clone(), e.value.clone()))
            .collect()
    }
}

/// The stash key of a dynamic-variable env name: `*OUT` and `$*OUT` → `*OUT`,
/// `@*ARGS` / `%*ENV` as is. `None` for a name that is not a dynamic.
// Cost: O(1).
fn stash_key(name: &str) -> Option<&str> {
    let b = name.as_bytes();
    match b.first() {
        Some(b'*') => Some(name),
        Some(b'$') if b.get(1) == Some(&b'*') => Some(&name[1..]),
        Some(b'@' | b'%') if b.get(1) == Some(&b'*') => Some(name),
        _ => None,
    }
}

/// The stash key of a `PROCESS::`-qualified name: `$PROCESS::OUT` → `*OUT`,
/// `@PROCESS::x` → `@*x`, `%PROCESS::x` → `%*x`, `PROCESS::x` → `*x`.
// Cost: O(|name|).
fn process_qualified_key(name: &str) -> Option<String> {
    let (sigil, rest) = match name.as_bytes().first() {
        Some(b'$') => ("", &name[1..]),
        Some(b'@') => ("@", &name[1..]),
        Some(b'%') => ("%", &name[1..]),
        _ => ("", name),
    };
    let bare = rest.strip_prefix("PROCESS::")?;
    Some(format!("{sigil}*{bare}"))
}

impl Interpreter {
    /// Redirect a dynamic-variable read to the process stash when the binding
    /// the env resolved (`found`) is the process binding. `found` is returned
    /// unchanged for a lexical binding, a non-dynamic name, or a name nobody
    /// published; a miss (`None`) on a published name yields the stash value.
    // Cost: O(1) when nothing was ever published; else O(|name|), a stash probe
    // plus, for a stale-looking binding, one env probe for a `my $*X` marker.
    #[inline]
    pub(crate) fn resolve_process_dynamic(
        &self,
        name: &str,
        found: Option<Value>,
    ) -> Option<Value> {
        if !self.process_dynamics.is_populated() {
            return found;
        }
        self.resolve_process_dynamic_slow(name, found)
    }

    fn resolve_process_dynamic_slow(&self, name: &str, found: Option<Value>) -> Option<Value> {
        let Some(key) = stash_key(name) else {
            return found;
        };
        let Some((current, initial)) = self.process_dynamics.get_with_initial(key) else {
            return found;
        };
        match found {
            None => Some(current),
            Some(v) if self.binding_is_process_level(key, &v, &current, initial.as_ref()) => {
                Some(current)
            }
            found => found,
        }
    }

    /// The process stash's answer for a read of `name`, or `None` to let the
    /// ordinary env lookup answer it. A `PROCESS::`-qualified name always
    /// reads the stash; a dynamic (`$*X`, `*X`, `@*X`, `%*X`) reads it when
    /// its binding is the process one (see [`Self::resolve_process_dynamic`]).
    // Cost: O(1) when nothing was ever published; else O(|name|), a stash
    // probe, an env probe and, for a stale-looking binding, a marker probe.
    #[inline]
    pub(crate) fn process_dynamic_read(&self, name: &str) -> Option<Value> {
        if !self.process_dynamics.is_populated() {
            return None;
        }
        self.process_dynamic_read_slow(name)
    }

    fn process_dynamic_read_slow(&self, name: &str) -> Option<Value> {
        if let Some(key) = process_qualified_key(name) {
            return self.process_dynamics.get(&key);
        }
        let key = stash_key(name)?;
        let (current, initial) = self.process_dynamics.get_with_initial(key)?;
        match self.env_dynamic_binding(name) {
            None => Some(current),
            Some(found)
                if self.binding_is_process_level(key, &found, &current, initial.as_ref()) =>
            {
                Some(current)
            }
            Some(_) => None,
        }
    }

    /// The env's binding of the dynamic `name`, looked up in the same order
    /// as [`Self::get_dynamic_handle`]: the spelling asked for, then its
    /// twin (`$*OUT` and `*OUT` are both seeded).
    fn env_dynamic_binding(&self, name: &str) -> Option<Value> {
        let env = self.env();
        env.get(name)
            .or_else(|| match name.strip_prefix('$') {
                Some(bare) if bare.starts_with('*') => env.get(bare),
                Some(_) => None,
                None if name.starts_with('*') => env.get(&format!("${name}")),
                None => None,
            })
            .cloned()
    }

    /// Whether `found`, the value the env resolved for the dynamic `key`, is
    /// the process binding rather than a lexical one: it is the process value
    /// as it was before any write (`initial`) or the current one (`current` —
    /// a frame-exit writeback can copy the process value into a caller's env),
    /// and no `my $*X` is in scope.
    fn binding_is_process_level(
        &self,
        key: &str,
        found: &Value,
        current: &Value,
        initial: Option<&Value>,
    ) -> bool {
        let found = found.clone().into_deref();
        (values_same_object(&found, current)
            || initial.is_some_and(|initial| values_same_object(&found, initial)))
            && !self.dynamic_declared_lexically(key)
    }

    /// Whether a `my $*X` for `key` is visible from the current frame.
    fn dynamic_declared_lexically(&self, key: &str) -> bool {
        self.env()
            .contains_key_sym(MetaNs::LexicalDynamic.key_for_str(key))
    }

    /// The base-tier seed for `key`: the process value before any write.
    fn process_dynamic_seed(&self, key: &str) -> Option<Value> {
        self.env()
            .dyn_base()
            .and_then(|base| base.get(&Symbol::intern(key)).cloned())
    }

    /// Publish a write to `$PROCESS::X` / `PROCESS::<$X>` (env spelling `key`).
    /// It never touches the env: every reader of a process binding consults
    /// the stash, so an env copy would only be a stale one later.
    // Cost: O(|key|), one stash write plus a base-tier probe on the first write.
    pub(crate) fn publish_process_dynamic(&mut self, key: &str, value: Value) {
        let key = stash_key(key).unwrap_or(key);
        let seed = || self.process_dynamic_seed(key);
        self.process_dynamics.set(key, value, seed);
        // A later `$*X = ...` from any frame must pass `CheckDynamicVarDeclared`
        // — installing a process-level default declares it.
        self.set_var_dynamic(key, true);
    }

    /// A by-name write to the dynamic `name` (`$*X = ...`, a `temp` restore):
    /// when the binding it would update is the process binding, publish it to
    /// the stash and report `true` so the caller skips the env write. A write
    /// to a lexical binding (`my $*X`, a dynamic parameter) returns `false`.
    // Cost: O(1) for a non-dynamic name; else O(|name|), one env probe, one
    // stash probe and, for a process binding, one marker probe and a write.
    pub(crate) fn publish_process_dynamic_write(&mut self, name: &str, value: &Value) -> bool {
        let Some(key) = stash_key(name) else {
            return false;
        };
        let process_level = match self.env_dynamic_binding(name) {
            // No binding at all: only a published name can be written here
            // (`CheckDynamicVarDeclared` rejects the rest).
            None => self.process_dynamics.contains(key),
            Some(found) => {
                match self.process_dynamics.get_with_initial(key) {
                    Some((current, initial)) => {
                        self.binding_is_process_level(key, &found, &current, initial.as_ref())
                    }
                    // Never written: the process binding is the base-tier seed.
                    None => match self.process_dynamic_seed(key) {
                        Some(seed) => {
                            values_same_object(&found.into_deref(), &seed)
                                && !self.dynamic_declared_lexically(key)
                        }
                        None => false,
                    },
                }
            }
        };
        if !process_level {
            return false;
        }
        self.process_dynamics
            .set(key, value.clone(), || self.process_dynamic_seed(key));
        true
    }
}
