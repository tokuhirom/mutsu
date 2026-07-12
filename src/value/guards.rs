//! Borrow guards for smart-pointer payloads of [`ValueView`](super::ValueView)
//! (3b-1 step B-guards).
//!
//! After the NaN-boxing flip a `Value` is one packed 64-bit word: the variant
//! tag lives in the word, so a `&'a Arc<T>` / `&'a Gc<T>` cannot be handed out
//! — no smart pointer exists in memory to reference. What CAN be produced is a
//! by-value reconstruction of the smart pointer from the payload address,
//! wrapped in `ManuallyDrop` so it derefs without touching refcounts. That is
//! [`RefGuard`]: the documented guard-type exit of the 3b-0 wall
//! ([docs/nanbox-3b0-api-wall.md](../../docs/nanbox-3b0-api-wall.md) §2,
//! ADR-0005 §2.1).
//!
//! While `Value` still stores the enum, a guard is a bitwise copy of the
//! borrowed pointer (`ptr::read`), never dropped; the `PhantomData` borrow
//! pins the source for `'a`, so refcounts and Bacon-Rajan bookkeeping are
//! untouched either way.

use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ops::Deref;
use std::sync::Arc;

/// A borrowed smart-pointer payload, held by value without owning a
/// reference. Deref yields the smart pointer itself (`&Arc<T>` etc.), so call
/// sites read exactly like the `&'a Arc<T>` fields they replace.
///
/// Deliberately NOT `Clone`/`Copy` (mirroring `ValueView`): to keep a payload
/// beyond the view's borrow, clone the smart pointer (`Arc::clone(&*guard)`),
/// which pays the refcount bump explicitly.
pub struct RefGuard<'a, P> {
    inner: ManuallyDrop<P>,
    _borrow: PhantomData<&'a P>,
}

impl<'a, P> RefGuard<'a, P> {
    /// Enum-storage constructor: bitwise-copy an existing smart pointer
    /// borrowed from the `Value`. Post-flip, `view()` instead reconstructs
    /// the pointer from the packed payload address.
    #[inline]
    pub(crate) fn borrowed(source: &'a P) -> Self {
        // SAFETY: the copy is wrapped in ManuallyDrop and never dropped, so
        // the source keeps its unique ownership of the reference; the
        // PhantomData borrow keeps `source` alive and unmutated for 'a.
        Self {
            inner: ManuallyDrop::new(unsafe { std::ptr::read(source) }),
            _borrow: PhantomData,
        }
    }
}

impl<P> Deref for RefGuard<'_, P> {
    type Target = P;
    #[inline]
    fn deref(&self) -> &P {
        &self.inner
    }
}

impl<P: std::fmt::Debug> std::fmt::Debug for RefGuard<'_, P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

/// Guarded borrow of an `Arc<T>` payload.
pub type ArcRef<'a, T> = RefGuard<'a, Arc<T>>;
/// Guarded borrow of a `Gc<T>` payload.
pub type GcRef<'a, T> = RefGuard<'a, crate::gc::Gc<T>>;
/// Guarded borrow of a `WeakGc<T>` payload.
pub type WeakGcRef<'a, T> = RefGuard<'a, crate::gc::WeakGc<T>>;
