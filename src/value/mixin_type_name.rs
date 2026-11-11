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

/// The `+{...}` suffix of a composition's type name, memoized for the one
/// base name it was last rendered against (`what_type_name` renders a mixin
/// as `<base><suffix>`; the base is the composition's inner value's type, so
/// it is the same on every call for one composition). Rendering walks every
/// marker of the composition, sorts the roles by application order and
/// formats each, and a `Type but Role` type object is named that way on every
/// `=:=`, `.^name` and smartmatch against it.
///
/// The suffix is a function of the `overrides` markers alone, so every
/// `&mut self` accessor of `overrides` clears the memo; the role cell and the
/// `.^set_name` name do not take part. The lock only guards the memo slot
/// (a composition node is shared across threads, see the module doc).
#[derive(Debug, Default)]
pub(crate) struct NameSuffixMemo(std::sync::Mutex<Option<RenderedSuffix>>);

/// The base name a suffix was rendered against, and the suffix (`None`: the
/// composition adds no `+{...}` to that base).
type RenderedSuffix = (Arc<str>, Option<Arc<str>>);

impl NameSuffixMemo {
    /// Drop the memo: the markers it was rendered from are about to change.
    // Cost: O(1).
    pub(super) fn forget(&mut self) {
        *self.0.get_mut().unwrap_or_else(PoisonError::into_inner) = None;
    }
}

impl MixinOverrides {
    /// The memoized suffix for `base`, rendering it with `render` on a miss.
    // Cost: O(1) plus an O(|suffix|) copy on a hit; a miss is `render`.
    pub(crate) fn name_suffix_for(
        &self,
        base: &str,
        render: impl FnOnce() -> Option<String>,
    ) -> Option<String> {
        if let Some((memo_base, suffix)) = self
            .name_suffix
            .0
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .as_ref()
            && &**memo_base == base
        {
            return suffix.as_deref().map(str::to_string);
        }
        let suffix = render();
        *self
            .name_suffix
            .0
            .lock()
            .unwrap_or_else(PoisonError::into_inner) =
            Some((Arc::from(base), suffix.as_deref().map(Arc::from)));
        suffix
    }
}
