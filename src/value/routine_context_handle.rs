//! The execution context a closure was created in (#18135).
//!
//! A closure resolves its by-name calls through the routine tables of the
//! context that wrote it, not through whichever thread runs it
//! (`runtime::routine_context`). The value layer cannot name that runtime
//! type, so a closure carries an opaque weak handle to the context's
//! routine-table cell; only the runtime looks inside it.

use std::any::Any;
use std::sync::Weak;

/// A weak handle to an execution context's routine-table cell, or unset.
#[derive(Clone, Default)]
pub(crate) struct RoutineContextHandle(Option<Weak<dyn Any + Send + Sync>>);

impl std::fmt::Debug for RoutineContextHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("RoutineContextHandle")
    }
}

impl RoutineContextHandle {
    // Cost: O(1).
    pub(crate) fn new(cell: Weak<dyn Any + Send + Sync>) -> Self {
        Self(Some(cell))
    }

    /// Whether no context was recorded.
    // Cost: O(1).
    #[inline]
    pub(crate) fn is_unset(&self) -> bool {
        self.0.is_none()
    }

    /// Whether this handle names the cell at `ptr`.
    // Cost: O(1).
    #[inline]
    pub(crate) fn points_to(&self, ptr: *const ()) -> bool {
        self.0
            .as_ref()
            .is_some_and(|w| std::ptr::eq(w.as_ptr().cast::<()>(), ptr))
    }

    /// The cell, while its context is alive.
    // Cost: O(1).
    pub(crate) fn upgrade(&self) -> Option<std::sync::Arc<dyn Any + Send + Sync>> {
        self.0.as_ref()?.upgrade()
    }
}
