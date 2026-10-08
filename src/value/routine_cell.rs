//! A routine's composition cell (ADR-11827).
//!
//! In rakudo a `Routine` is an object, and `$r does R` reblesses it in place:
//! every alias, every role argument that captured it and the registry entry
//! see `R`. mutsu's `does` builds a `Value::Mixin` around the code object, so
//! the composition has to live somewhere every value of the same routine
//! shares. That is this cell: one per routine, held by every `SubData` of it
//! (and by a named routine's `FunctionDef`, which rebuilds share), written by
//! `does` and read wherever a routine is dispatched on or role-checked.
//!
//! The cell is shared across Raku threads, so it is behind a lock; a routine
//! never mixed into pays one relaxed atomic load (`composed`).

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

use super::MixinOverrides;
use crate::gc::Gc;
use crate::symbol::Symbol;

#[derive(Default)]
struct Inner {
    /// Set once a composition is stored; never cleared.
    composed: AtomicBool,
    overrides: RwLock<Option<Gc<MixinOverrides>>>,
    /// A `Signature` bound to the routine's `Code.$!signature`
    /// (`nqp::bindattr($r, Code, '$!signature', $sig)`, upstream
    /// NativeCall's `nativecast(Signature, $ptr)`), which `.signature`
    /// answers from then on.
    signature: RwLock<Option<super::Value>>,
    /// The type object the routine's return-type spelling names in its
    /// declaring scope (a `my class` storage name, an imported or `constant`
    /// type term), recorded where that scope is visible so `.returns` asked
    /// from elsewhere -- upstream NativeCall's `$routine.returns` -- answers
    /// the same type.
    return_type: RwLock<Option<super::Value>>,
    /// The name `Code.set_name` / `nqp::setcodename` gave the routine. Every
    /// `&foo` read of a named routine builds a fresh `SubData` from its
    /// registry entry, so a rename written into one of those would be lost on
    /// the next read; the cell is what they all share (#11844).
    renamed: RwLock<Option<Symbol>>,
    /// Set once a rename is stored; never cleared.
    has_renamed: AtomicBool,
    /// The `Routine.prec` an operator's `is equiv/tighter/looser/assoc` traits
    /// declared. A first-class `Sub` outlives its registry entry (an operator
    /// declared inside `sub EXPORT` and exported through the returned `Map`),
    /// so `.prec` cannot be read back from the registry alone.
    op_prec: std::sync::OnceLock<crate::op_prec::OpPrec>,
}

/// See the module docs. Cloning shares the cell: a clone of the handle is the
/// same routine. A Raku-level `.clone` of a routine takes [`Self::forked`].
#[derive(Clone, Default)]
pub(crate) struct RoutineCell(Arc<Inner>);

impl std::fmt::Debug for RoutineCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("RoutineCell")
    }
}

impl RoutineCell {
    /// The roles composed into the routine so far, if any.
    // Cost: O(1); one atomic load when nothing was composed.
    pub(crate) fn get(&self) -> Option<Gc<MixinOverrides>> {
        if !self.0.composed.load(Ordering::Acquire) {
            return None;
        }
        self.0.overrides.read().ok().and_then(|g| g.clone())
    }

    /// Record `overrides` as the routine's composition.
    // Cost: O(1).
    pub(crate) fn set(&self, overrides: Gc<MixinOverrides>) {
        if let Ok(mut slot) = self.0.overrides.write() {
            *slot = Some(overrides);
        }
        self.0.composed.store(true, Ordering::Release);
    }

    /// The `Signature` bound to the routine's `$!signature`, if any.
    // Cost: O(1).
    pub(crate) fn bound_signature(&self) -> Option<super::Value> {
        self.0.signature.read().ok().and_then(|g| g.clone())
    }

    /// Bind `signature` as the routine's `$!signature`.
    // Cost: O(1).
    pub(crate) fn bind_signature(&self, signature: super::Value) {
        if let Ok(mut slot) = self.0.signature.write() {
            *slot = Some(signature);
        }
    }

    /// The return type object recorded for the routine, if any.
    // Cost: O(1).
    pub(crate) fn return_type(&self) -> Option<super::Value> {
        self.0.return_type.read().ok().and_then(|g| g.clone())
    }

    /// Record the routine's return type object, unless one is recorded.
    // Cost: O(1).
    pub(crate) fn note_return_type(&self, ty: super::Value) {
        if let Ok(mut slot) = self.0.return_type.write()
            && slot.is_none()
        {
            *slot = Some(ty);
        }
    }

    /// The declared operator precedence recorded for the routine, if any.
    // Cost: O(1).
    pub(crate) fn op_prec(&self) -> Option<&crate::op_prec::OpPrec> {
        self.0.op_prec.get()
    }

    /// Record the routine's declared operator precedence (first write wins).
    // Cost: O(1).
    pub(crate) fn note_op_prec(&self, prec: crate::op_prec::OpPrec) {
        let _ = self.0.op_prec.set(prec);
    }

    /// The name the routine was last renamed to, if it ever was.
    // Cost: O(1); one atomic load for a routine that was never renamed.
    pub(crate) fn renamed(&self) -> Option<Symbol> {
        if !self.0.has_renamed.load(Ordering::Acquire) {
            return None;
        }
        self.0.renamed.read().ok().and_then(|g| *g)
    }

    /// Record `name` as the routine's name, for every value of the routine.
    // Cost: O(1).
    pub(crate) fn rename(&self, name: Symbol) {
        if let Ok(mut slot) = self.0.renamed.write() {
            *slot = Some(name);
        }
        self.0.has_renamed.store(true, Ordering::Release);
    }

    /// A new, unshared cell starting from this one's composition: what a
    /// Raku-level `.clone` of the routine gets (ADR-11827 §2.3).
    // Cost: O(1).
    pub(crate) fn forked(&self) -> Self {
        let fresh = Self::default();
        if let Some(name) = self.renamed() {
            fresh.rename(name);
        }
        if let Some(overrides) = self.get() {
            fresh.set(overrides);
        }
        if let Some(signature) = self.bound_signature() {
            fresh.bind_signature(signature);
        }
        if let Some(ty) = self.return_type() {
            fresh.note_return_type(ty);
        }
        if let Some(prec) = self.op_prec() {
            fresh.note_op_prec(prec.clone());
        }
        fresh
    }
}
