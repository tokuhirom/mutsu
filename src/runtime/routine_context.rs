//! Which execution context's routine tables a closure resolves its names
//! through (#18135, ADR-17216 §3.2).
//!
//! Every execution context (the main interpreter and each spawned thread)
//! holds its own routine tables, forked copy-on-write at spawn. A routine is
//! lexical in Raku, so a closure must resolve a by-name call through the
//! tables of the context that *wrote* it, not through whichever thread
//! happens to run it. A worker spawned early (at `BEGIN`, or by a module's
//! mainline during `use`) and fed closures through a `Channel` never re-forks,
//! so its own copy lacks every routine registered after its spawn: the call
//! misses (`Unknown function: ok`) or dispatches over a stale multi candidate
//! set.
//!
//! So a closure records a weak handle to the routine-table cell of the context
//! it was created in (`SubData::routine_ctx`). Invoked on another context whose
//! tables are not its creator's current ones, the closure runs with a
//! copy-on-write share of the creator's tables installed, and the caller's are
//! put back afterwards ([`Interpreter::enter_closure_routine_context`]). Writes
//! made during the call land in the share (copy-on-write), never in the
//! creator's cell, so a routine scope on one thread still cannot install its
//! inner subs where another thread resolves them.

use std::sync::{Arc, RwLock};

use super::fn_keys_index::SavedFnKeysIndex;
use super::routine_tables::RoutineTables;
use super::Interpreter;
use crate::value::RoutineContextHandle;

/// One execution context's routine-table cell (`TypeState::routines`).
pub(crate) type RoutineCell = RwLock<Arc<RoutineTables>>;

/// The caller's routine tables, replaced by
/// [`Interpreter::enter_closure_routine_context`] and handed back to
/// [`Interpreter::leave_closure_routine_context`], together with the caller's
/// base-name key index over them.
pub(crate) struct SavedRoutineTables {
    tables: Arc<RoutineTables>,
    keys_index: SavedFnKeysIndex,
}

impl Interpreter {
    /// The handle a closure created now records: this context's routine-table
    /// cell.
    // Cost: O(1), one weak-count increment.
    #[inline]
    pub(crate) fn routine_context_handle(&self) -> RoutineContextHandle {
        let cell: std::sync::Weak<RoutineCell> = Arc::downgrade(&self.types.routines);
        let cell: std::sync::Weak<dyn std::any::Any + Send + Sync> = cell;
        RoutineContextHandle::new(cell)
    }

    /// Install the current routine tables of the context a closure was created
    /// in, when they are not what this context resolves through already.
    /// Returns what to restore, or `None` when nothing changed (this context
    /// is the creator, the creator's tables are the ones installed here, or the
    /// creator is gone or unrecorded).
    // Cost: O(1): a weak upgrade, and for a closure from another context one
    // read lock of the creator's cell and a pointer compare.
    #[inline]
    pub(crate) fn enter_closure_routine_context(
        &mut self,
        creator: &RoutineContextHandle,
    ) -> Option<SavedRoutineTables> {
        if creator.is_unset() || creator.points_to(Arc::as_ptr(&self.types.routines).cast()) {
            return None;
        }
        self.enter_foreign_routine_context(creator)
    }

    #[cold]
    fn enter_foreign_routine_context(
        &mut self,
        creator: &RoutineContextHandle,
    ) -> Option<SavedRoutineTables> {
        let any = creator.upgrade()?;
        let cell = any.downcast_ref::<RoutineCell>()?;
        let theirs = Arc::clone(&*cell.read().unwrap_or_else(|e| e.into_inner()));
        let mut ours = self
            .types
            .routines
            .write()
            .unwrap_or_else(|e| e.into_inner());
        // The same content under another `Arc` (this context forked the
        // creator's tables and since only renewed their version, as a
        // resolution-memo invalidation does) resolves every name alike:
        // swapping would only retire every memo twice per call (#14812).
        if Arc::ptr_eq(&theirs, &ours) || theirs.same_content_as(&ours) {
            return None;
        }
        let saved = std::mem::replace(&mut *ours, theirs);
        drop(ours);
        // The base-name key index (`fn_keys_by_base`) describes the functions
        // map it was filled from, and it is not tagged with that map's
        // version: over the creator's map it would answer the caller's keys
        // (an inner `sub` the caller's map lacks reads as unregistered, or one
        // it has as registered). Set the caller's index aside and start an
        // empty one over the installed map.
        let keys_index = self.take_fn_keys_index();
        self.bump_routine_generation();
        Some(SavedRoutineTables {
            tables: saved,
            keys_index,
        })
    }

    /// Put back what [`Self::enter_closure_routine_context`] replaced.
    // Cost: O(1).
    pub(crate) fn leave_closure_routine_context(&mut self, saved: SavedRoutineTables) {
        *self
            .types
            .routines
            .write()
            .unwrap_or_else(|e| e.into_inner()) = saved.tables;
        self.restore_fn_keys_index(saved.keys_index);
        self.bump_routine_generation();
    }

    /// Move the registry write generation: memos keyed by it cover what the
    /// routine tables answer. `fn_resolve_gen` mirrors the installed
    /// functions map's version, so the memos tagged with it follow the swap:
    /// a version names one map content, so the ones tagged with the newly
    /// installed map's version are valid again and the rest are retired.
    // Cost: O(1).
    fn bump_routine_generation(&mut self) {
        self.types.registry_write_gen.store(
            crate::runtime::registry_gen::next_generation(),
            std::sync::atomic::Ordering::Relaxed,
        );
        self.sync_fn_resolve_gen();
    }
}
