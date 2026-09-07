//! A stable per-container identity for `.WHICH`.
//!
//! `Array`/`Hash`/`Seq`/`Slip`/`Promise`/`Channel` `.WHICH` is reference-based,
//! exactly as raku's is — but it used the container's ADDRESS, and an address is
//! only unique among *live* objects. A `.WHICH` string always outlives its
//! object, so two temporaries could compare equal simply because the allocator
//! handed the second one the block the first had just freed:
//!
//! ```raku
//! say [1, 2].WHICH eq [3, 4, 5].WHICH;   # raku: False   mutsu was: True
//! ```
//!
//! [`WhichId`] replaces the address with a lazily minted, monotonically
//! increasing number that is never reused. It is deliberately *not* minted at
//! construction: a container that is never asked for its identity pays only the
//! eight bytes of the (untouched) counter.
//!
//! ## Why cloning resets it
//!
//! `Clone` yields a FRESH, unminted id rather than copying. The container types
//! embedding this are cloned in exactly two situations, and resetting is right
//! for both:
//!
//! - A genuinely new container (`my @b = @a`) must not share `@a`'s identity.
//! - A `Gc::make_mut` copy-on-write rebuild produces a new allocation, which is
//!   what the ADDRESS-derived identity already reported as a new object. So the
//!   id is never *less* stable than the pointer it replaces — it only stops two
//!   unrelated objects from colliding.
//!
//! In-place mutation of a live container (`@a.push(3)`) goes through the
//! aliased write path rather than `make_mut`, so it keeps both the allocation
//! and the id — `.WHICH` stays stable across a push, as in rakudo.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock, Weak};

use crate::value::Value;

/// Source of the ids. Starts at 1 so `0` can mean "not yet minted".
static NEXT_WHICH_ID: AtomicU64 = AtomicU64::new(1);

/// A lazily assigned, never-reused object id, embedded in a container's data.
#[derive(Debug, Default)]
pub struct WhichId(AtomicU64);

impl WhichId {
    /// This container's id, minting one on first use.
    ///
    /// Racing threads may both mint; the loser's number is simply discarded and
    /// both observe the winner's, so the id a container reports is stable from
    /// the first read onward.
    pub fn get(&self) -> u64 {
        match self.0.load(Ordering::Relaxed) {
            0 => {
                let minted = NEXT_WHICH_ID.fetch_add(1, Ordering::Relaxed);
                match self
                    .0
                    .compare_exchange(0, minted, Ordering::Relaxed, Ordering::Relaxed)
                {
                    Ok(_) => minted,
                    Err(won) => won,
                }
            }
            id => id,
        }
    }
}

impl Clone for WhichId {
    fn clone(&self) -> Self {
        Self::default()
    }
}

/// `WhichId` is identity, not content: it must never make two structurally
/// equal containers compare unequal (`[1,2] eqv [1,2]` is `True`), so every
/// `WhichId` is equal to every other.
impl PartialEq for WhichId {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for WhichId {}

/// The same never-reused identity for a container whose payload is a bare
/// `Arc<Vec<Value>>` and so has nowhere to embed a [`WhichId`] — `Slip`.
///
/// Keyed by address like the buggy scheme it replaces, but each entry keeps a
/// `Weak` to the object the id was minted for. When the allocator hands a new
/// object the address of a dead one, that `Weak` no longer upgrades and a fresh
/// id is minted — which is exactly the collision the embedded `WhichId` avoids
/// by construction. Entries are only created for slips whose identity is
/// actually asked for, and dead ones are swept whenever the table doubles.
pub fn slip_which_id(items: &std::sync::Arc<Vec<Value>>) -> u64 {
    static TABLE: OnceLock<Mutex<SlipIds>> = OnceLock::new();
    let mut table = TABLE
        .get_or_init(|| {
            Mutex::new(SlipIds {
                ids: HashMap::new(),
                sweep_at: 64,
            })
        })
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let key = std::sync::Arc::as_ptr(items) as usize;
    if let Some((weak, id)) = table.ids.get(&key)
        && weak.upgrade().is_some()
    {
        return *id;
    }
    let id = NEXT_WHICH_ID.fetch_add(1, Ordering::Relaxed);
    table
        .ids
        .insert(key, (std::sync::Arc::downgrade(items), id));
    if table.ids.len() >= table.sweep_at {
        table.ids.retain(|_, (weak, _)| weak.upgrade().is_some());
        table.sweep_at = (table.ids.len() * 2).max(64);
    }
    id
}

struct SlipIds {
    ids: HashMap<usize, (Weak<Vec<Value>>, u64)>,
    /// Table size at which dead entries are swept, doubled after each sweep so
    /// the amortized cost per mint stays constant.
    sweep_at: usize,
}

#[cfg(test)]
mod tests {
    use super::WhichId;

    /// The container types this is embedded in are the two hottest in the
    /// interpreter, so the cost of the change is pinned here: one `AtomicU64`,
    /// no padding growth beyond it.
    #[test]
    fn embedding_costs_one_word() {
        use std::mem::size_of;
        assert_eq!(size_of::<WhichId>(), 8, "the id is a single AtomicU64");
        // ArrayData/HashData are `Vec`/`HashMap` plus a handful of `Option`s,
        // all 8-aligned, so the field adds exactly its own size.
        assert_eq!(size_of::<crate::value::ArrayData>() % 8, 0);
        assert_eq!(size_of::<crate::value::HashData>() % 8, 0);
        // Measured 2026-09-07: 200 -> 208 bytes for both, i.e. exactly the
        // field, with no padding growth. Pinned so a later field cannot slip in
        // unmeasured on the two hottest container types.
        assert_eq!(size_of::<crate::value::ArrayData>(), 208);
        assert_eq!(size_of::<crate::value::HashData>(), 208);
    }

    #[test]
    fn ids_are_stable_and_distinct() {
        let a = WhichId::default();
        let b = WhichId::default();
        assert_eq!(a.get(), a.get(), "an id is stable once minted");
        assert_ne!(a.get(), b.get(), "two containers get different ids");
    }

    #[test]
    fn cloning_mints_a_fresh_id() {
        let a = WhichId::default();
        let first = a.get();
        let copy = a.clone();
        assert_ne!(first, copy.get(), "a clone is a different object");
    }

    #[test]
    fn all_ids_compare_equal() {
        // Structural equality must ignore identity (`[1,2] eqv [1,2]`).
        let a = WhichId::default();
        let b = WhichId::default();
        let _ = a.get();
        assert_eq!(a, b);
    }
}
