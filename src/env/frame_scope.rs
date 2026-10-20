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
    /// A type namespace binding in this frame or its lexical outer, never
    /// in an intervening caller. Parameter binding uses this even while
    /// ordinary parameter/default lookups temporarily ignore static links.
    // Cost: O(d + l), d = frame/static-outer tiers, l = lexical capture layers.
    pub(crate) fn get_lexical_type_binding(&self, key: Symbol, name: Symbol) -> Option<&Value> {
        if crate::symbol::env_key_probe(key) == crate::symbol::EnvKeyProbe::Absent {
            return None;
        }
        let mut current = self;
        loop {
            if let Some(value) = current.inner.get(&key) {
                return Some(value);
            }
            if current.is_tombstoned(key) {
                return None;
            }
            if current.frame_root {
                if let Some(super::static_link::StaticLink::UnitOuter {
                    target,
                    activation,
                    capture_first,
                    capture_names,
                }) = current.inner.static_link()
                {
                    if let Some(value) = activation.as_ref().and_then(|capture| capture.get(&key)) {
                        return Some(value);
                    }
                    let captures_type = *capture_first
                        || capture_names
                            .as_ref()
                            .is_some_and(|names| names.contains(&name) || names.contains(&key));
                    if captures_type
                        && let Some(value) = current
                            .fallback
                            .as_deref()
                            .and_then(|capture| capture.get(&key))
                    {
                        return Some(value);
                    }
                    return target.get_sym(key);
                }
                if !current.outer_is_unit {
                    return current
                        .fallback
                        .as_deref()
                        .and_then(|capture| capture.get(&key));
                }
                return current
                    .outer_is_unit
                    .then_some(current.parent.as_deref())
                    .flatten()
                    .and_then(|parent| Self::program_scope_from(parent).get_sym(key));
            }
            current = current.parent.as_deref()?;
        }
    }

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
