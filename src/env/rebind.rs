//! Rebinding a name this env's overlay already holds, without the general
//! insert's map write.
//!
//! A loop rebinds its topic (or named parameter) once per iteration, and the
//! key is in the overlay from the second iteration on. [`Env::insert_sym`]
//! pays for the general case each time: the copy-on-write probe, a hashing
//! `HashMap::insert`, and the new-key bookkeeping it then skips. Overwriting
//! the existing slot is the same write — the key set, and so every index
//! derived from it, is unchanged — at the cost of one lookup.

use super::{Env, file_key};
use crate::symbol::Symbol;
use crate::value::Value;
use std::sync::Arc;

impl Env {
    /// Exactly [`Self::insert_sym`], faster when `key` is already in this
    /// env's own overlay and the overlay is not shared with another env.
    ///
    /// Every step `insert_sym` takes for a key that is already present is
    /// taken here too, in the same order (the tombstone, frame-write and
    /// code-entry notes, then the sigilless-alias note `Tier::insert` makes);
    /// what is skipped is only the copy-on-write probe and the map's insert,
    /// both of which are the identity for a present key in a unique overlay.
    /// The two keys whose insert has side effects of its own (`$?FILE`, a
    /// rebound `&return`) and every other shape take `insert_sym` itself.
    // Cost: O(1).
    #[inline]
    pub(crate) fn rebind_sym(&mut self, key: Symbol, value: Value) {
        if key == file_key() || key == crate::symbol::wk::rebound_return() {
            self.insert_sym(key, value);
            return;
        }
        self.untombstone(key);
        self.note_frame_write(key);
        self.note_code_entry(key);
        if let Some(tier) = Arc::get_mut(&mut self.inner)
            && let Some(slot) = tier.get_mut(&key)
        {
            crate::env_tier::note_alias_entry(key, &value);
            *slot = value;
            return;
        }
        self.cow_mut().insert(key, value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rebind_overwrites_a_present_key_and_inserts_an_absent_one() {
        let mut env = Env::new();
        let k = Symbol::intern("rebind_test_k");
        env.rebind_sym(k, Value::int(1));
        assert_eq!(
            env.get_sym(k).map(|v| v.to_string_value()),
            Some("1".into())
        );
        env.rebind_sym(k, Value::int(2));
        assert_eq!(
            env.get_sym(k).map(|v| v.to_string_value()),
            Some("2".into())
        );
    }

    #[test]
    fn rebind_on_a_shared_overlay_leaves_the_other_holder_alone() {
        let mut env = Env::new();
        let k = Symbol::intern("rebind_test_shared");
        env.insert_sym(k, Value::int(1));
        let snapshot = env.clone();
        env.rebind_sym(k, Value::int(2));
        assert_eq!(
            env.get_sym(k).map(|v| v.to_string_value()),
            Some("2".into())
        );
        assert_eq!(
            snapshot.get_sym(k).map(|v| v.to_string_value()),
            Some("1".into())
        );
    }
}
