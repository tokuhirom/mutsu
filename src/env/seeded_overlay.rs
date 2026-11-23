//! A frame overlay adopted from a pre-built tier (#18274).
//!
//! Every compiled method call injects its class's class-body `my` statics
//! into its fresh frame overlay (`Interpreter::inject_class_body_statics`),
//! one insert per static per call. The writes are the same on every call
//! until the statics tables change, so they are applied once to a shared
//! [`Tier`] kept with the class's statics plan, and a frame whose overlay
//! has had no write yet adopts that tier instead. The frame sees exactly the
//! overlay the inserts would have built; its first own write copies the
//! shared tier on write (`Env::cow_mut`), so the plan's tier is never
//! mutated through a frame.

use std::sync::Arc;

use super::{Env, SymMap, Tier, empty_overlay_ref, file_key, note_env_key};
use crate::symbol::Symbol;
use crate::value::Value;

impl Env {
    /// `writes` applied in order to an empty tier, with the side effects
    /// `insert_sym_noting` has that outlive the call made once here
    /// (`note_env_key`'s process-wide latches; [`Tier::new`] marks the keys
    /// and alias entries). `None` when a key has a per-env insert side effect
    /// a shared tier cannot replay: `?FILE` sets the env's file symbol, and
    /// `&return` latches the rebound-return flag at insert time.
    // Cost: O(w), w = |writes|.
    pub(crate) fn seed_tier_for_writes(writes: &[(Symbol, Value)]) -> Option<Arc<Tier>> {
        if writes
            .iter()
            .any(|(k, _)| *k == file_key() || *k == crate::symbol::wk::rebound_return())
        {
            return None;
        }
        let mut map = SymMap::with_capacity_and_hasher(writes.len(), Default::default());
        for (k, v) in writes {
            note_env_key(k.as_str());
            map.insert(*k, v.clone());
        }
        Some(Arc::new(Tier::new(map)))
    }

    /// Take `tier` as this env's overlay when the overlay is still the shared
    /// empty one and nothing an insert would update is present: no
    /// tombstones (an insert untombstones its key), no frame-write log or
    /// code-entry index (an insert records its key in them) and no capture
    /// fallback (whose merged view an insert resets). Then inserting each of
    /// the tier's entries would have produced exactly `tier`. Returns `false`,
    /// changing nothing, otherwise.
    // Cost: O(1).
    pub(crate) fn adopt_fresh_overlay(&mut self, tier: &Arc<Tier>) -> bool {
        if !Arc::ptr_eq(&self.inner, empty_overlay_ref())
            || self.tombstones.is_some()
            || self.frame_writes.is_some()
            || self.code_entries.is_some()
            || self.fallback.is_some()
        {
            return false;
        }
        self.inner = Arc::clone(tier);
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(name: &str) -> Symbol {
        Symbol::intern(name)
    }

    #[test]
    fn adopted_overlay_matches_inserts_and_copies_on_write() {
        let writes: Vec<(Symbol, Value)> = vec![
            (s("seeded_a"), Value::int(1)),
            (s("seeded_b"), Value::int(2)),
            (s("seeded_a"), Value::int(3)),
        ];
        let tier = Env::seed_tier_for_writes(&writes).expect("plain keys seed a tier");
        let parent = Env::new();
        let mut adopted = Env::scoped_child(parent.clone());
        assert!(adopted.adopt_fresh_overlay(&tier));
        let mut inserted = Env::scoped_child(parent);
        for (k, v) in &writes {
            inserted.insert_sym(*k, v.clone());
        }
        for k in ["seeded_a", "seeded_b"] {
            assert_eq!(adopted.get_sym(s(k)), inserted.get_sym(s(k)));
        }
        assert_eq!(adopted.get_sym(s("seeded_a")).cloned(), Some(Value::int(3)));
        // The frame's own write copies the shared tier, which stays as built.
        adopted.insert_sym(s("seeded_a"), Value::int(9));
        assert_eq!(adopted.get_sym(s("seeded_a")).cloned(), Some(Value::int(9)));
        assert_eq!(tier.get(&s("seeded_a")).cloned(), Some(Value::int(3)));
        // A second adoption is refused once the overlay holds a write.
        assert!(!adopted.adopt_fresh_overlay(&tier));
    }

    #[test]
    fn file_key_writes_seed_no_tier() {
        let writes = vec![(file_key(), Value::int(1))];
        assert!(Env::seed_tier_for_writes(&writes).is_none());
    }
}
