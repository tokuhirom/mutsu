//! The `.^set_name` name of a role composition (ADR-0060), kept behind its
//! own lock on [`MixinOverrides`].
//!
//! A rename is written in place, through every alias of the composition-keyed
//! `.WHAT` node, and that node is shared by every thread spawned after it was
//! made (a worker's registry starts as its parent's snapshot). NativeCall's
//! `CArray[T]` / `Pointer[T]` `^parameterize` renames it on every call, so two
//! threads parameterizing one type at once wrote the node concurrently. The
//! name used to be an entry of the `overrides` map, inserted through
//! `gc_contents_mut` with no lock, while other threads walked that map: a
//! racing insert rehashed it under them and the process died with SIGSEGV
//! (#14765: Cro's server and client threads both run `CArray[int32]` in
//! `Cro::TCP::NoDelay` for their first connection). The program cannot
//! synchronize a node it does not know exists, so this sharing has to be sound
//! on its own (ADR-12814 §2.1, guarantee 2).

use super::MixinOverrides;
use std::sync::{Arc, PoisonError};

impl MixinOverrides {
    /// The name `.^set_name` gave this composition, if any.
    // Cost: O(1).
    pub(crate) fn type_name(&self) -> Option<Arc<str>> {
        self.type_name
            .read()
            .unwrap_or_else(PoisonError::into_inner)
            .clone()
    }

    /// Rename this composition, visibly through every alias of the node.
    /// Writing the name it already has changes nothing, so the steady state
    /// of a type parameterized on every call takes only the read lock.
    // Cost: O(n), n = name length (the copy into a fresh `Arc<str>`).
    pub(crate) fn set_type_name(&self, name: &str) {
        if self.type_name().as_deref() == Some(name) {
            return;
        }
        *self
            .type_name
            .write()
            .unwrap_or_else(PoisonError::into_inner) = Some(Arc::from(name));
    }
}
