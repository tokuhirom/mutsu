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
//! is published to it (and still mirrored into the writer's env, which the
//! many native readers of `$*SPEC`/`$*CWD`/... consult directly), and a read
//! that resolves to the process binding is redirected to it. "Resolves to the
//! process binding" is decided by identity: an env copy of the process binding
//! holds either the process value as it was before the first write (the
//! base-tier seed) or a value that was published since, so a binding whose
//! value is one of those very objects, with no `my $*X` declared in the dynamic
//! scope, is the process binding. Anything else (a `my $*X`, a dynamic
//! parameter bound to another object, a `start` block's inherited redirection)
//! is a lexical binding and wins.

use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;
use crate::value::identity::values_same_object;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

/// How many superseded values an entry remembers (see [`Entry::superseded`]).
const SUPERSEDED_CAP: usize = 16;

/// One published process-level dynamic.
struct Entry {
    /// The current process-level value.
    value: Value,
    /// The value the process binding held before the first write (the
    /// interpreter's base-tier seed), if it had one. An env entry that is
    /// still this object is a stale copy of the process binding.
    initial: Option<Value>,
    /// The most recent values this one superseded, newest last. A writer
    /// mirrors what it publishes into its own env, so after another thread
    /// publishes, that mirror is a stale copy of one of these. Bounded so a
    /// program that swaps handles in a loop does not keep every one alive; a
    /// copy older than the window reads as a lexical binding.
    superseded: std::collections::VecDeque<Value>,
}

impl Entry {
    /// Whether `found` is (a stale copy of) this process binding.
    fn holds(&self, found: &Value) -> bool {
        values_same_object(found, &self.value)
            || self
                .initial
                .as_ref()
                .is_some_and(|initial| values_same_object(found, initial))
            || self
                .superseded
                .iter()
                .any(|old| values_same_object(found, old))
    }
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

    /// The current value of `key` if `found` (an env binding) is that process
    /// binding — the value itself, its pre-write seed, or a recently
    /// superseded value (see [`Entry::superseded`]). The outer `None` means
    /// nothing was published under `key`.
    // Cost: O(|key| + SUPERSEDED_CAP), one hash probe under the read lock.
    fn current_if_holds(&self, key: &str, found: &Value) -> Option<Option<Value>> {
        let entries = self.read();
        let entry = entries.get(key)?;
        Some(entry.holds(found).then(|| entry.value.clone()))
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
            Some(entry) => {
                if values_same_object(&entry.value, &value) {
                    entry.value = value;
                } else {
                    let old = std::mem::replace(&mut entry.value, value);
                    if entry.superseded.len() == SUPERSEDED_CAP {
                        entry.superseded.pop_front();
                    }
                    entry.superseded.push_back(old);
                }
            }
            None => {
                let initial = initial();
                entries.insert(
                    key.to_string(),
                    Entry {
                        value,
                        initial,
                        superseded: std::collections::VecDeque::new(),
                    },
                );
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
    // plus, for a process-looking binding, one env probe for a `my $*X` marker.
    #[inline]
    pub(crate) fn resolve_process_dynamic(
        &self,
        name: &str,
        found: Option<Value>,
    ) -> Option<Value> {
        if !self.process_dynamics.is_populated() {
            return found;
        }
        let Some(key) = stash_key(name) else {
            return found;
        };
        match found {
            None => self.process_dynamics.get(key),
            Some(v) => self.process_value_for_binding(key, &v).unwrap_or(Some(v)),
        }
    }

    /// The process stash's answer for a read of `name`, or `None` to let the
    /// ordinary env lookup answer it. A `PROCESS::`-qualified name always
    /// reads the stash; a dynamic (`$*X`, `*X`, `@*X`, `%*X`) reads it when
    /// its binding is the process one (see [`Self::resolve_process_dynamic`]).
    // Cost: O(1) when nothing was ever published; else O(|name|), a stash
    // probe, an env probe and, for a process-looking binding, a marker probe.
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
        match self.env_dynamic_binding(name) {
            None => self.process_dynamics.get(key),
            Some(found) => self.process_value_for_binding(key, &found).flatten(),
        }
    }

    /// For an env binding `found` of the published dynamic `key`: `Some(Some(
    /// current))` when it is the process binding, `Some(None)` when it is a
    /// lexical one, `None` when nothing is published under `key`.
    fn process_value_for_binding(&self, key: &str, found: &Value) -> Option<Option<Value>> {
        let current = self
            .process_dynamics
            .current_if_holds(key, &found.clone().into_deref())?;
        Some(current.filter(|_| !self.dynamic_declared_lexically(key)))
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

    /// Whether the env's binding of the dynamic `name` (stash key `key`) is
    /// the process binding — or there is none, which only a published name
    /// (or a fresh `PROCESS::` install) can reach.
    fn binding_is_process_level(&self, name: &str, key: &str) -> bool {
        let Some(found) = self.env_dynamic_binding(name) else {
            return true;
        };
        match self.process_value_for_binding(key, &found) {
            Some(current) => current.is_some(),
            // Never written: the process binding is the base-tier seed.
            None => {
                self.process_dynamic_seed(key)
                    .is_some_and(|seed| values_same_object(&found.into_deref(), &seed))
                    && !self.dynamic_declared_lexically(key)
            }
        }
    }

    /// Publish a write to `$PROCESS::X` / `PROCESS::<$X>` (env spelling
    /// `key`). Returns whether the writer's env binding is the process one, in
    /// which case the caller mirrors the value into the env too (the native
    /// readers of `$*SPEC`, `$*CWD`, ... read the env directly); a `my $*X`
    /// in scope keeps its own value.
    // Cost: O(|key|), one stash write plus an env probe and, on the first
    // write, a base-tier probe.
    pub(crate) fn publish_process_dynamic(&mut self, key: &str, value: Value) -> bool {
        let key = stash_key(key).unwrap_or(key);
        let mirror = self.binding_is_process_level(key, key);
        self.process_dynamics
            .set(key, value, || self.process_dynamic_seed(key));
        // A later `$*X = ...` from any frame must pass `CheckDynamicVarDeclared`
        // — installing a process-level default declares it.
        self.set_var_dynamic(key, true);
        mirror
    }

    /// A by-name write to the dynamic `name` (`$*X = ...`, a `temp` save or
    /// restore): when the binding it updates is the process binding, publish
    /// the value to the stash as well. The caller still performs its env write.
    // Cost: O(1) for a non-dynamic name; else O(|name|), one env probe, one
    // stash probe, a marker probe and, for a process binding, a stash write.
    pub(crate) fn publish_process_dynamic_write(&mut self, name: &str, value: &Value) {
        let Some(key) = stash_key(name) else {
            return;
        };
        if self.binding_is_process_level(name, key) {
            self.process_dynamics
                .set(key, value.clone(), || self.process_dynamic_seed(key));
        }
    }
}
