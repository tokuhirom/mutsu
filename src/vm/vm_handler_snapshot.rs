//! Shared, owned copies of a region's bytecode and function table for the
//! CATCH/CONTROL handlers that run INLINE at a deep raise site (ADR-0072,
//! #9469, #9510).
//!
//! An inline handler needs its installing frame's `CompiledCode` and
//! `CompiledFns` after the borrow that `exec_try_catch_op_inner` holds is out
//! of reach, so the handler entry owns them through an `Arc`. Deep-cloning
//! both on every region entry made each entry O(ops + constants + fns); the
//! copy is instead taken once per code object / function-table version and
//! shared by every later entry.

use std::sync::{Arc, OnceLock};

use crate::opcode::{CompiledCode, CompiledFns};
use crate::runtime::Interpreter;

/// The lazily-taken shared copy of the [`CompiledCode`] that holds this cell.
///
/// Cloning a `CompiledCode` yields an EMPTY cell: the clone is a separate
/// value that may be mutated before it runs, so it must not inherit a copy of
/// the original.
#[derive(Default)]
pub(crate) struct CodeSnapshot(OnceLock<Arc<CompiledCode>>);

impl Clone for CodeSnapshot {
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl std::fmt::Debug for CodeSnapshot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("CodeSnapshot")
    }
}

impl CompiledCode {
    /// An owned copy of this code for an inline handler entry.
    // Cost: O(1) once taken; the first call per code object is O(c),
    // c = ops + constants.
    pub(crate) fn shared_snapshot(&self) -> Arc<CompiledCode> {
        if let Some(snapshot) = self.handler_snapshot.0.get() {
            // A chunk that grew after the copy was taken (it only ever grows)
            // is not served the stale copy; it pays a fresh clone instead.
            if snapshot.ops.len() == self.ops.len()
                && snapshot.constants.len() == self.constants.len()
            {
                return snapshot.clone();
            }
            return Arc::new(self.clone());
        }
        let fresh = Arc::new(self.clone());
        let _ = self.handler_snapshot.0.set(fresh.clone());
        fresh
    }
}

impl Interpreter {
    /// An owned copy of `fns` for an inline handler entry, shared between
    /// entries while the table is unchanged. `CompiledFns::id` is redrawn on
    /// every mutation and never reused, so a matching id proves the cached copy
    /// has the same contents.
    // Cost: O(1) while the table is unchanged; O(f) after a mutation,
    // f = compiled functions.
    pub(crate) fn shared_fns_snapshot(&mut self, fns: &CompiledFns) -> Arc<CompiledFns> {
        let id = fns.id();
        if id != 0
            && let Some((cached_id, cached)) = &self.handler_fns_snapshot
            && *cached_id == id
        {
            return cached.clone();
        }
        let fresh = Arc::new(fns.clone());
        if id != 0 {
            self.handler_fns_snapshot = Some((id, fresh.clone()));
        }
        fresh
    }
}
