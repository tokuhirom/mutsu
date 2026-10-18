//! `DYNAMIC::<key>`: a name looked up through the dynamic scope (#13590,
//! ADR-12529 §2.1).
//!
//! Rakudo resolves a `DYNAMIC::` key by walking the dynamic scope frame by
//! frame, the running frame first, and looking the key up exactly as spelled
//! in each frame's own lexical pad. So `DYNAMIC::<$x>` finds a caller's
//! ordinary `my $x`, a frame's own `my $*d` shadows a caller's from the
//! frame's entry on (its pad entry exists before the declaration runs), and a
//! process-level dynamic such as `$*OUT`, which lives in no pad, is not found.
//!
//! A mutsu frame keeps its lexicals in two places: the slots its code
//! declares (`CompiledCode::locals`) and its env tiers. Each `VmCallFrame`
//! records its caller's code (`caller_code`) next to the caller's slot region
//! and env, so the walk reads every frame the same way the running one is
//! read. A call path that pushes no `VmCallFrame` (a light call) leaves its
//! frame out of the walk; its names are still in the env tiers of the frame
//! the walk reaches next.

use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// The value `DYNAMIC::<key>` names, or `None` when no frame of the
    /// dynamic scope binds `key`.
    // Cost: O(f * (l + d)), f = frames on the call stack, l = slots each
    // frame's code declares, d = each frame's env tiers. Rakudo: O(f * p),
    // p = pad lookup.
    pub(super) fn dynamic_stash_keyed_value(
        &self,
        code: &CompiledCode,
        key: &str,
    ) -> Option<Value> {
        let env_key = key.strip_prefix('$').unwrap_or(key);
        if env_key.is_empty() {
            return None;
        }
        let scalar = key.starts_with('$');
        let sym = Symbol::intern(env_key);
        // A dynamic's binding is its env entry, written when its `my $*d`
        // runs; the slot is seeded from the callers' binding at the frame's
        // entry, so before the declaration runs only the env tells the frame's
        // own (still unset) variable from the caller's.
        let dynamic = env_key.trim_start_matches(['@', '%', '&']).starts_with('*');
        let any = || Value::package(Symbol::intern("Any"));
        let from_slot = |value: &Value| {
            // A declared scalar no store reached yet is `Any`, as an unset
            // `Scalar` reads.
            if scalar && value.is_nil() {
                any()
            } else {
                value.clone()
            }
        };
        if let Some(i) = code.locals.iter().position(|name| name == env_key) {
            if dynamic {
                return Some(
                    self.env()
                        .get_sym_in_frame(sym)
                        .cloned()
                        .unwrap_or_else(any),
                );
            }
            if let Some(value) = self.locals.get(i) {
                return Some(from_slot(value));
            }
        }
        if let Some(value) = self.env().get_sym_in_frame(sym) {
            return Some(value.clone());
        }
        let slots = self.locals.all_slots();
        let mut region_end = self.locals.base();
        for frame in self.call_frames.iter().rev() {
            let Some(base) = frame.saved_locals_base.as_ref().map(|b| b.base()) else {
                continue;
            };
            let region = slots.get(base..region_end).unwrap_or(&[]);
            region_end = base;
            if frame.caller_code != 0 {
                // SAFETY: `caller_code` is the address of the `CompiledCode`
                // the caller was running when it pushed this frame; that
                // caller is suspended until the frame pops, so its code is
                // alive while the frame is on `call_frames`.
                let caller = unsafe { &*(frame.caller_code as *const CompiledCode) };
                if let Some(i) = caller.locals.iter().position(|name| name == env_key) {
                    if dynamic {
                        return Some(
                            frame
                                .saved_env
                                .get_sym_in_frame(sym)
                                .cloned()
                                .unwrap_or_else(any),
                        );
                    }
                    if let Some(value) = region.get(i) {
                        return Some(from_slot(value));
                    }
                }
            }
            if let Some(value) = frame.saved_env.get_sym_in_frame(sym) {
                return Some(value.clone());
            }
        }
        None
    }

    /// Push the stash `DYNAMIC::<key>` indexes: the one entry the dynamic
    /// scope binds under `key`, or none.
    // Cost: as `dynamic_stash_keyed_value`.
    pub(super) fn push_dynamic_stash_for_key(&mut self, code: &CompiledCode, key: &str) {
        let mut entries = ValueMap::default();
        if let Some(value) = self.dynamic_stash_keyed_value(code, key) {
            entries.insert(key.to_string(), value);
        }
        let stash = self.pseudo_stash_hash(entries);
        self.stack.push(stash);
    }
}
