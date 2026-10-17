//! Lookups confined to the running call frame (ADR-12529).
//!
//! A frame's env chain continues into its callers' tiers, so a plain lookup of
//! a name the frame did not declare can answer with a caller's binding. A
//! reader that has a better source than the callers for such a name (a unit's
//! recorded type lanes, `vm/vm_unit_lexical_meta_lanes.rs`) first asks only
//! the frame's own tiers.

use super::Env;
use crate::symbol::Symbol;
use crate::value::Value;

impl Env {
    /// `key`'s binding in the running call frame's own tiers: from this env
    /// down to and including the frame root, never a caller's tier or a
    /// capture. The whole chain when no tier is a frame root (the program
    /// scope, a module body).
    // Cost: O(d), d = the frame's tiers.
    pub(crate) fn get_sym_in_frame(&self, key: Symbol) -> Option<&Value> {
        let mut cur = self;
        loop {
            if let Some(v) = cur.inner.get(&key) {
                return Some(v);
            }
            if cur.frame_root {
                return None;
            }
            cur = cur.parent.as_deref()?;
        }
    }

    /// A variable's type record (`__mutsu_type::x` and kin) when the running
    /// code declares the variable itself (ADR-13757 step 4): the record was
    /// written in this frame, so the lookup ends at the frame root and never
    /// reads a same-named record of a frame that called it. A key removed in
    /// a tier of the frame (an untyped parameter's shadowing) answers `None`.
    /// The whole chain when no tier is a frame root (the program scope).
    // Cost: O(d), d = the frame's tiers.
    pub(crate) fn get_record_in_frame(&self, key: Symbol) -> Option<&Value> {
        let mut cur = self;
        loop {
            if let Some(v) = cur.inner.get(&key) {
                return Some(v);
            }
            if cur.is_tombstoned(key) || cur.frame_root {
                return None;
            }
            match cur.parent.as_deref() {
                Some(parent) => cur = parent,
                None => return cur.base_get(key),
            }
        }
    }
}
