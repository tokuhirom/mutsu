//! Grammar rules declared on one thread reach every thread (ADR-17216).
//!
//! A `token`/`rule`/`regex` declared in a grammar (or any package) is a method
//! of that package, and a package is world state: rakudo installs it once per
//! process. mutsu keeps rule definitions in the per-thread routine tables
//! (ADR-17216 §3.2), whose lexical-scope rollbacks they share with `my sub`s.
//! So a grammar `EVAL`'d on a worker had its class on every thread but its
//! rules only on that worker: `G.parse` on the main thread died with "No such
//! method 'TOP'".
//!
//! Every package-owned rule declaration is therefore also appended to one
//! world log. A thread replays the entries it has not seen into its own
//! tables at the entry of a call (one atomic load when nothing is new). The
//! routine-table restore keeps package-owned rules (`restore_routine_registry`),
//! so a replayed rule outlives the scope it was replayed in, as the declaring
//! thread's own copy does. A `my token`, or a rule of a `my`-scoped package,
//! is lexical and is not published.

use super::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
enum WorldTokenEntry {
    /// A candidate registered under `key`; `multi` pushes it beside the key's
    /// other candidates instead of replacing them.
    Token {
        key: Symbol,
        def: Arc<FunctionDef>,
        multi: bool,
    },
    /// A `proto token`/`proto rule` marker, with its positional signature.
    Proto {
        key: String,
        params: Option<Arc<Vec<crate::ast::ParamDef>>>,
    },
}

/// The append-only log of published rule declarations, shared by every
/// thread ([`crate::runtime::dispatch_state::DispatchState`]).
#[derive(Default)]
pub(crate) struct WorldTokenLog {
    entries: Mutex<Vec<WorldTokenEntry>>,
    /// `entries.len()`, readable without the lock.
    len: AtomicUsize,
}

impl WorldTokenLog {
    // Cost: O(1) amortized, one push under the mutex.
    fn append(&self, entry: WorldTokenEntry) -> usize {
        let mut entries = self
            .entries
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        entries.push(entry);
        let index = entries.len() - 1;
        self.len.store(entries.len(), Ordering::Release);
        index
    }
}

/// Whether a rule of `package` is package-owned (world) rather than lexical.
// Cost: O(|package|).
fn publishable_package(package: &str) -> bool {
    !package.is_empty()
        && !package.contains('\0')
        && !crate::qualified::is_global_package(Symbol::intern(package))
}

impl Interpreter {
    /// Append a rule candidate this thread registered to the world log, unless
    /// it is lexical.
    // Cost: O(|package|) plus one mutex push.
    pub(crate) fn publish_world_token(
        &mut self,
        package: &str,
        key: Symbol,
        def: Arc<FunctionDef>,
        multi: bool,
    ) {
        if !publishable_package(package) || self.is_my_scoped_type_name(package) {
            return;
        }
        let index = self
            .dispatch
            .world_tokens
            .append(WorldTokenEntry::Token { key, def, multi });
        self.note_own_world_token(index);
    }

    /// [`Self::publish_world_token`] for a `proto token` marker under the
    /// qualified `key`.
    // Cost: O(|key|) plus one mutex push.
    pub(crate) fn publish_world_proto_token(
        &mut self,
        key: &str,
        params: Option<Arc<Vec<crate::ast::ParamDef>>>,
    ) {
        let Some((package, _)) = key.rsplit_once("::") else {
            return;
        };
        if !publishable_package(package) || self.is_my_scoped_type_name(package) {
            return;
        }
        let index = self.dispatch.world_tokens.append(WorldTokenEntry::Proto {
            key: key.to_string(),
            params,
        });
        self.note_own_world_token(index);
    }

    /// The entry at `index` is this thread's own: skip replaying it when it is
    /// the next one this thread would replay.
    fn note_own_world_token(&mut self, index: usize) {
        if self.dispatch.world_tokens_seen == index {
            self.dispatch.world_tokens_seen = index + 1;
        }
    }

    /// Replay the rule declarations other threads published since this thread
    /// last looked.
    // Cost: O(1), one atomic load, when nothing is new; otherwise O(e * c),
    // e = new entries, c = candidates under each entry's key.
    #[inline]
    pub(crate) fn sync_world_tokens(&mut self) {
        let len = self.dispatch.world_tokens.len.load(Ordering::Acquire);
        if len != self.dispatch.world_tokens_seen {
            self.replay_world_tokens(len);
        }
    }

    #[cold]
    fn replay_world_tokens(&mut self, len: usize) {
        let seen = self.dispatch.world_tokens_seen;
        let new: Vec<WorldTokenEntry> = {
            let entries = self
                .dispatch
                .world_tokens
                .entries
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            entries[seen.min(entries.len())..len.min(entries.len())].to_vec()
        };
        self.dispatch.world_tokens_seen = len;
        let missing: Vec<WorldTokenEntry> = {
            let routines = self.routines();
            new.into_iter()
                .filter(|entry| match entry {
                    WorldTokenEntry::Token { key, def, multi } => {
                        match routines.token_defs.get(key) {
                            None => true,
                            Some(defs) => *multi && !defs.iter().any(|d| Arc::ptr_eq(d, def)),
                        }
                    }
                    WorldTokenEntry::Proto { key, .. } => !routines.proto_tokens.contains(key),
                })
                .collect()
        };
        if missing.is_empty() {
            return;
        }
        let mut routines = self.routines_mut();
        for entry in missing {
            match entry {
                WorldTokenEntry::Token { key, def, .. } => {
                    Arc::make_mut(&mut routines.token_defs)
                        .entry(key)
                        .or_default()
                        .push(def);
                }
                WorldTokenEntry::Proto { key, params } => {
                    if let Some(params) = params {
                        routines.proto_token_params.insert(key.clone(), params);
                    }
                    Arc::make_mut(&mut routines.proto_tokens).insert(key);
                }
            }
        }
        drop(routines);
        crate::runtime::regex_parse::TOKEN_DEFS_GEN.fetch_add(1, Ordering::Relaxed);
    }
}
