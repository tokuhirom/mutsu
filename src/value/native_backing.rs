//! `ArrayData`'s native `array[T]` decode cache (docs/adr/0030).
//!
//! `NativeBacking` replaces the flat `native_storage` / `native_dirty` /
//! `native_snapshot` fields `ArrayData` used to carry directly. The cache
//! lives behind a [`SyncUnsafeCell`] so `ArrayData::items()` can keep its
//! `&self` signature — and therefore so can `Deref for ArrayData`, which is
//! the chokepoint every array read in the interpreter goes through.
//!
//! # The generation graveyard
//!
//! A re-sync **pushes** a fresh `Box<Vec<Value>>` onto `DecodeCache::generations`
//! rather than overwriting the live one. `Box`'s heap allocation gives a
//! decoded vector a stable address independent of the `Vec<Box<_>>` that
//! holds it, so a `&Vec<Value>` handed out by an earlier `items()` stays
//! valid across a later re-sync that pushes another generation — the
//! obligation ADR-0013's `gc_contents_mut` states-and-audits at ~62 call
//! sites cannot be discharged the same way here, because `Deref` puts it on
//! every `&ArrayData` in the interpreter instead (docs/adr/0030 §2.2).
//!
//! Every `&mut self` entry point (`sync_into_seed_mut`,
//! `sync_into_seed_readonly`) has a borrow-checker proof that no shared
//! borrow into any generation is live, so each one prunes the graveyard back
//! to a single generation. Growth is bounded by observed native-side writes,
//! not by reads: a pure read loop over an unchanging native array allocates
//! one generation total.

use super::sync_cell::SyncUnsafeCell;
use super::{BufData, Value};
use crate::gc::Gc;

struct DecodeCache {
    /// The live decode, plus every generation a `&Vec<Value>` might still be
    /// borrowed from. Empty until the first re-sync — until then `ArrayData`'s
    /// own `items` field (the "seed") is authoritative.
    ///
    /// `Box` is load-bearing here, not an efficiency choice: the returned
    /// reference is `&'a Vec<Value>`, i.e. a reference to the *middle* Vec's
    /// own control block, which must stay valid across a later `push` that
    /// may reallocate this outer `Vec`. Without the `Box` indirection, a
    /// `push` moving the outer buffer would invalidate every previously
    /// returned `&Vec<Value>` — exactly the UB the generation graveyard
    /// exists to prevent (module docs).
    #[allow(clippy::vec_box)]
    generations: Vec<Box<Vec<Value>>>,
    /// `true` when the seed (or the popped-back-into-seed last generation)
    /// holds elements not yet encoded into `node`'s bytes.
    dirty: bool,
    /// The native bytes as of the last resync, used to detect an external
    /// (native-side / FFI) write cheaply — a slice comparison, not a clone.
    snapshot: Option<Vec<u8>>,
}

/// The shared native payload node plus its lazily-filled decode cache. See
/// the module docs for the generation graveyard this exists to make sound.
pub(crate) struct NativeBacking {
    node: Gc<BufData>,
    cache: SyncUnsafeCell<DecodeCache>,
}

impl std::fmt::Debug for NativeBacking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // `UnsafeCell` (and therefore `SyncUnsafeCell`) is deliberately not
        // `Debug` — reading it without synchronization would be exactly the
        // kind of casual access this module exists to avoid. Show the node,
        // which is plain `Debug`, and nothing else.
        f.debug_struct("NativeBacking")
            .field("node", &self.node)
            .finish_non_exhaustive()
    }
}

impl NativeBacking {
    /// `initial_snapshot` must be `node`'s bytes at construction time — the
    /// caller (`ArrayData::promote_native_storage`) just encoded them and
    /// already has them at hand, so this avoids an immediate redundant sync.
    pub(crate) fn new(node: Gc<BufData>, initial_snapshot: Vec<u8>) -> Box<Self> {
        Box::new(NativeBacking {
            node,
            cache: SyncUnsafeCell::new(DecodeCache {
                generations: Vec::new(),
                dirty: false,
                snapshot: Some(initial_snapshot),
            }),
        })
    }

    pub(crate) fn node(&self) -> &Gc<BufData> {
        &self.node
    }

    /// Pure decision: given the currently-known-best vector and cache state,
    /// is `node`'s bytes still in sync with it? If not, compute the fresh
    /// native bytes and the freshly decoded vector.
    fn compute_resync(
        node: &Gc<BufData>,
        dirty: bool,
        snapshot: Option<&[u8]>,
        current: &[Value],
    ) -> Option<(Vec<u8>, Vec<Value>)> {
        if !dirty && snapshot == Some(node.bytes.as_slice()) {
            return None;
        }
        Some(if dirty {
            (
                crate::value::value_buf::encode_storage(node, current),
                current.to_vec(),
            )
        } else {
            (
                node.bytes.clone(),
                crate::value::value_buf::decode_storage(node),
            )
        })
    }

    /// Overwrite `node`'s bytes in place.
    ///
    /// # Safety-relevant note
    ///
    /// This is the one read-path cache fill mutsu performs under a shared
    /// borrow the caller keeps using (docs/adr/0030 §1.4) — `bytes` is a
    /// fresh, independently-owned `Vec`, never itself derived from `node`,
    /// so this is a plain overwrite through the `Gc` handle, not the
    /// self-referential `&self`-derived-pointer write ADR-0013 §8 measured
    /// as UB. It legitimately reuses `gc_contents_mut` because the caller
    /// (this module) holds the `node` handle directly and drops the `&mut`
    /// immediately after — the shape `gc_contents_mut`'s contract requires.
    fn write_node_bytes(node: &Gc<BufData>, bytes: &[u8]) {
        // SAFETY: see above — no borrow into `node`'s payload is carried
        // across this call.
        unsafe {
            let data = crate::gc::gc_contents_mut(node);
            data.bytes.clear();
            data.bytes.extend_from_slice(bytes);
        }
    }

    /// Read path (`ArrayData::items`): sync if needed and return the
    /// authoritative vector. `seed` is `ArrayData.items`, used only until the
    /// first generation exists.
    pub(crate) fn sync_and_borrow<'a>(&'a self, seed: &'a Vec<Value>) -> &'a Vec<Value> {
        // SAFETY: this is the shape `SyncUnsafeCell` exists for — a write
        // under the shared `&self` the caller (via `ArrayData::items`,
        // reached through `Deref`) keeps using afterward. No `&DecodeCache`
        // reference is held across the write below: `best_known`'s borrow
        // ends when it is passed to `compute_resync` (which clones out of
        // it), and the returned reference is formed fresh from `cache`
        // afterward. Prior generations are never rewritten in place (module
        // docs), so no previously-returned `&Vec<Value>` is invalidated.
        let cache = unsafe { &mut *self.cache.get() };
        let best_known: &Vec<Value> = cache.generations.last().map(|b| b.as_ref()).unwrap_or(seed);
        match Self::compute_resync(
            &self.node,
            cache.dirty,
            cache.snapshot.as_deref(),
            best_known,
        ) {
            None => cache.generations.last().map(|b| b.as_ref()).unwrap_or(seed),
            Some((bytes, decoded)) => {
                Self::write_node_bytes(&self.node, &bytes);
                cache.generations.push(Box::new(decoded));
                cache.dirty = false;
                cache.snapshot = Some(bytes);
                cache.generations.last().unwrap().as_ref()
            }
        }
    }

    /// Shared exclusive-access resync: collapse the graveyard into `*seed`
    /// (safe — `&mut self` proves no shared borrow into any generation is
    /// live) and clear it. Returns whether a real resync against the native
    /// bytes happened (used to decide the `dirty` bit afterward).
    fn resync_into_seed_mut(&mut self, seed: &mut Vec<Value>) -> bool {
        let node = self.node.clone();
        let cache = self.cache.get_mut();
        let best_known: &Vec<Value> = cache.generations.last().map(|b| b.as_ref()).unwrap_or(seed);
        let resynced =
            Self::compute_resync(&node, cache.dirty, cache.snapshot.as_deref(), best_known);
        match resynced {
            Some((bytes, decoded)) => {
                Self::write_node_bytes(&node, &bytes);
                *seed = decoded;
                cache.dirty = false;
                cache.snapshot = Some(bytes);
                cache.generations.clear();
                true
            }
            None => {
                if let Some(last) = cache.generations.pop() {
                    *seed = *last;
                }
                cache.generations.clear();
                false
            }
        }
    }

    /// Owning/exclusive-access read (`ArrayData::take_items`/`into_items`):
    /// pull the authoritative content into `seed`, leaving `dirty` as the
    /// resync decided (matches the read path's bookkeeping) — unlike
    /// [`Self::sync_into_seed_mut`], nothing is about to hand out a further
    /// `&mut` for external mutation.
    pub(crate) fn sync_into_seed_readonly(&mut self, seed: &mut Vec<Value>) {
        self.resync_into_seed_mut(seed);
    }

    /// Write path (`ArrayData::items_mut`): pull the authoritative content
    /// into `seed` and mark it dirty, since the caller is about to hand out
    /// a `&mut` into `seed` for external mutation.
    pub(crate) fn sync_into_seed_mut(&mut self, seed: &mut Vec<Value>) {
        self.resync_into_seed_mut(seed);
        self.cache.get_mut().dirty = true;
    }

    /// Test-only observability for the pruning probe in
    /// `native_cache_shapes` — the graveyard's size is otherwise private
    /// bookkeeping no production call site should ever branch on.
    #[cfg(test)]
    pub(crate) fn generation_count(&self) -> usize {
        // SAFETY: read-only, and the test harness is single-threaded here.
        unsafe { &*self.cache.get() }.generations.len()
    }

    /// Every `Value` edge retained across all generations — used only by
    /// `Trace`, which must never sync (a sync mid-collect would re-enter
    /// allocation from inside the collector, same reasoning as the original
    /// flat-field `native_storage` read in `Trace for ArrayData`).
    pub(crate) fn trace_edges(&self, visit: &mut dyn FnMut(&crate::gc::ErasedGc)) {
        // SAFETY: read-only. GC trace runs at a collect safepoint, never
        // concurrently with a `sync_and_borrow`/`sync_into_seed_*` call on
        // this same cache (the residual cross-thread race is the narrower,
        // separately-tracked ADR-0001 layer 3c concern, unaffected by this
        // primitive — see `SyncUnsafeCell`'s docs).
        let cache = unsafe { &*self.cache.get() };
        for generation in &cache.generations {
            for v in generation.iter() {
                v.gc_trace(visit);
            }
        }
    }
}

impl Clone for NativeBacking {
    /// The node is cloned as a `Gc` **handle** — the copy shares the payload
    /// node, matching the derived `Clone` this replaces (probed benign on
    /// both builds: `my int @d = @c; @d[0] = 555` leaves `@c[0]` unchanged).
    /// The cache is **not** shared: the clone starts from a single fresh
    /// generation holding a deep copy of the live decode (or none, if this
    /// backing was never synced) — a fresh `ArrayData` has handed out no
    /// references, so the graveyard has nothing to keep alive.
    fn clone(&self) -> Self {
        // SAFETY: read-only snapshot of the cache state; the borrow ends
        // within this function, well before any write to the clone's own
        // (independent) cell.
        let cache = unsafe { &*self.cache.get() };
        let generations = match cache.generations.last() {
            Some(live) => vec![Box::new((**live).clone())],
            None => Vec::new(),
        };
        NativeBacking {
            node: self.node.clone(),
            cache: SyncUnsafeCell::new(DecodeCache {
                generations,
                dirty: cache.dirty,
                snapshot: cache.snapshot.clone(),
            }),
        }
    }
}
