//! A named sub's code object linked to the activation that declared it
//! (ADR-12529 phase 3, #12692; the VM half is `vm/vm_lexsub_capture.rs`).
//!
//! The capture a `&g` read builds in the declaring code holds that
//! activation's cells, and the frame that runs the sub links to it
//! ([`StaticLink::UnitOuter::activation`]). The per-routine by-name stores
//! (ADR-0024's unit-lexical buckets, mutsu#9111's aliases, TRIR's outer
//! bindings) are keyed by the sub, not the activation, so they ask
//! [`Env::frame_capture_holds`] and step aside for a name the activation
//! binds. A flattening of the frame's chain keeps the capture in its record.

use super::Env;
use super::static_link::{StaticLink, lexically_scoped, suppressed};
use crate::env_capture_view::CaptureView;
use crate::symbol::Symbol;
use std::sync::Arc;

impl Env {
    /// Mark this env, a named sub's code-object capture, as holding the cells
    /// of the activation that declared the sub: the sub's frame then links
    /// capture-first ([`Self::link_static_outer_to_unit`]), and its free
    /// variables resolve here before the per-name routine stores
    /// ([`Self::frame_capture_holds`]).
    // Cost: O(1), plus the overlay's copy-on-write when it is shared.
    pub(crate) fn mark_activation_capture(&mut self) {
        self.cow_mut().mark_activation_capture();
    }

    /// Whether this env is a capture marked by
    /// [`Self::mark_activation_capture`].
    // Cost: O(1).
    #[inline]
    pub(crate) fn is_activation_capture(&self) -> bool {
        self.inner.is_activation_capture()
    }

    /// Whether the frame this env runs in is linked to a declaring
    /// activation ([`StaticLink::UnitOuter::activation`]) whose capture binds
    /// `key`: the name is the frame's lexical outer's, which a by-name store
    /// keyed by the routine (ADR-0024's unit-lexical buckets, mutsu#9111's
    /// aliases, TRIR's outer bindings) must not answer instead.
    // Cost: O(1) when the chain holds no static link; else O(d + l),
    // d = tiers above the frame root, l = the capture's layers.
    #[inline]
    pub(crate) fn frame_capture_holds(&self, key: Symbol) -> bool {
        self.chain_has_static_link
            && lexically_scoped(key)
            && !suppressed()
            && self.frame_activation().is_some_and(|view| view.get(&key).is_some())
    }

    /// Whether the frame this env runs in is linked to a declaring
    /// activation: a closure created here closes over that activation too.
    // Cost: O(1) when the chain holds no static link; else O(d), d = tiers
    // above the frame root.
    pub(crate) fn runs_in_activation_frame(&self) -> bool {
        self.chain_has_static_link && self.frame_activation().is_some()
    }

    /// The declaring activation's capture of the frame this env runs in: the
    /// link of its frame root, or -- the frame flattened into the bottom of
    /// the chain -- the record the flattening left.
    // Cost: O(d), d = tiers above the frame root.
    pub(super) fn frame_activation(&self) -> Option<&Arc<CaptureView>> {
        let mut cur = self;
        loop {
            if cur.frame_root || cur.parent.is_none() {
                let StaticLink::UnitOuter { activation, .. } = cur.inner.static_link()?;
                return activation.as_ref();
            }
            cur = cur.parent.as_deref()?;
        }
    }

    /// [`Self::frame_capture_holds`] for a name the caller may hold only as
    /// text; it is interned only in a frame linked to an activation.
    // Cost: as [`Self::frame_capture_holds`], plus an intern when `sym` is
    // `None` in such a frame.
    #[inline]
    pub(crate) fn frame_capture_holds_name(&self, name: &str, sym: Option<Symbol>) -> bool {
        if !self.chain_has_static_link {
            return false;
        }
        let Some(view) = self.frame_activation() else {
            return false;
        };
        let key = sym.unwrap_or_else(|| Symbol::intern(name));
        lexically_scoped(key) && !suppressed() && view.get(&key).is_some()
    }
}
