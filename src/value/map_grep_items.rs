//! The source elements of a deferred `.map` / `.grep` Seq
//! ([`crate::value::SeqSource::MapGrep`]).

use crate::value::Value;
use std::sync::Arc;

/// The source elements a [`crate::value::SeqSource::MapGrep`] body maps or greps over.
#[derive(Clone, Debug)]
pub(crate) enum MapGrepItems {
    /// Materialized at the `.map`/`.grep` call (a Range, a Seq, a hash, a
    /// shaped array's leaves, the listop form's flattened arguments, ...).
    Snapshot(Arc<Vec<Value>>),
    /// A (non-shaped) Array read at pull time, as Rakudo's `.map` iterates
    /// the Array's own iterator: nothing is copied at the call, so
    /// `@a.map(&f).head(3)` is O(1) in `@a.elems`, and `my $m = @a.map(&f);
    /// @a.push(4)` maps the pushed element too.
    Live(Value),
    /// The not-yet-run `.map` / `.grep` Seq this one was called on
    /// (`@a.grep(&g).map(&f)`), pulled one element at a time as the
    /// downstream callback needs it, so the two callbacks interleave per
    /// element as Rakudo's pull pipeline does (#11176): a map block reading
    /// the `$/` the grep's `m//` set sees that element's match, not the last
    /// one. See [`MapGrepChain`].
    Chain(Arc<MapGrepChain>),
}

/// The upstream of a chained `.map` / `.grep` ([`MapGrepItems::Chain`]): the
/// upstream Seq's stolen [`crate::value::SeqSource::MapGrep`] plus every
/// element it has produced so far, indexed by the downstream's `pos` exactly
/// as a snapshot would be.
pub(crate) struct MapGrepChain {
    state: std::sync::Mutex<MapGrepChainState>,
}

struct MapGrepChainState {
    /// Every element the upstream has produced so far.
    produced: Vec<Value>,
    /// What is left to pull: the upstream `SeqSource::MapGrep`, `Reified`
    /// once it ran dry, `Taken` while a pull holds it (or after one failed).
    source: crate::value::SeqSource,
}

impl std::fmt::Debug for MapGrepChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MapGrepChain").finish_non_exhaustive()
    }
}

impl MapGrepChain {
    /// A chain over `prefix`, the elements the upstream already produced
    /// (a boolification pulls one), and `source`, the rest.
    pub(crate) fn new(prefix: Vec<Value>, source: crate::value::SeqSource) -> Arc<Self> {
        Arc::new(Self {
            state: std::sync::Mutex::new(MapGrepChainState {
                produced: prefix,
                source,
            }),
        })
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, MapGrepChainState> {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    /// Whether the upstream has nothing left to produce.
    // Cost: O(1).
    pub(crate) fn exhausted(&self) -> bool {
        !matches!(self.lock().source, crate::value::SeqSource::MapGrep { .. })
    }

    /// Pull more upstream elements through `pull` (which advances the
    /// upstream source and reports whether it ran dry) and append them.
    /// Returns how many arrived. The source is out of the lock while `pull`
    /// runs the upstream callback, so nothing it does can deadlock here; a
    /// failed pull leaves it `Taken`.
    // Cost: `pull`'s cost.
    pub(crate) fn extend(
        &self,
        pull: impl FnOnce(
            &mut crate::value::SeqSource,
        ) -> Result<(Vec<Value>, bool), crate::value::RuntimeError>,
    ) -> Result<usize, crate::value::RuntimeError> {
        let mut source = {
            let mut state = self.lock();
            if !matches!(state.source, crate::value::SeqSource::MapGrep { .. }) {
                return Ok(0);
            }
            std::mem::replace(&mut state.source, crate::value::SeqSource::Taken)
        };
        let (items, exhausted) = pull(&mut source)?;
        let mut state = self.lock();
        let arrived = items.len();
        state.produced.extend(items);
        state.source = if exhausted {
            crate::value::SeqSource::Reified
        } else {
            source
        };
        Ok(arrived)
    }
}

impl MapGrepItems {
    /// A live view of `target` when it is a non-shaped, non-itemized Array
    /// (whose list-context elements are exactly its items), else the snapshot
    /// `snapshot` builds.
    // Cost: O(1) for an Array; `snapshot`'s cost otherwise.
    pub(crate) fn of(target: &Value, snapshot: impl FnOnce() -> Vec<Value>) -> Self {
        match target.view() {
            crate::value::ValueView::Array(_, kind)
                if !kind.is_itemized() && !super::shaped_array::is_shaped_array(target) =>
            {
                MapGrepItems::Live(target.clone())
            }
            _ => MapGrepItems::Snapshot(Arc::new(snapshot())),
        }
    }

    /// How many source elements there are now.
    // Cost: O(1).
    pub(crate) fn len(&self) -> usize {
        match self {
            MapGrepItems::Snapshot(items) => items.len(),
            MapGrepItems::Live(array) => match array.view() {
                crate::value::ValueView::Array(items, _) => items.len(),
                _ => 0,
            },
            MapGrepItems::Chain(chain) => chain.lock().produced.len(),
        }
    }

    /// Run `f` over the source elements as they are now.
    // Cost: O(1) plus `f`'s cost.
    pub(crate) fn with_items<R>(&self, f: impl FnOnce(&[Value]) -> R) -> R {
        match self {
            MapGrepItems::Snapshot(items) => f(items),
            MapGrepItems::Live(array) => match array.view() {
                crate::value::ValueView::Array(items, _) => f(&items),
                _ => f(&[]),
            },
            MapGrepItems::Chain(chain) => f(&chain.lock().produced),
        }
    }

    /// A copy of the source elements `start..end` (clamped to the length).
    // Cost: O(end - start).
    pub(crate) fn slice(&self, start: usize, end: usize) -> Vec<Value> {
        match self {
            MapGrepItems::Snapshot(items) => {
                let end = end.min(items.len());
                items
                    .get(start..end)
                    .map(<[Value]>::to_vec)
                    .unwrap_or_default()
            }
            MapGrepItems::Live(array) => match array.view() {
                crate::value::ValueView::Array(items, _) => {
                    let end = end.min(items.len());
                    items
                        .get(start..end)
                        .map(<[Value]>::to_vec)
                        .unwrap_or_default()
                }
                _ => Vec::new(),
            },
            MapGrepItems::Chain(chain) => {
                let state = chain.lock();
                let end = end.min(state.produced.len());
                state
                    .produced
                    .get(start..end)
                    .map(<[Value]>::to_vec)
                    .unwrap_or_default()
            }
        }
    }

    pub(crate) fn trace_edges(&self, visit: &mut dyn FnMut(&crate::gc::ErasedGc)) {
        match self {
            MapGrepItems::Snapshot(items) => {
                for v in items.iter() {
                    v.gc_trace(visit);
                }
            }
            MapGrepItems::Live(array) => array.gc_trace(visit),
            MapGrepItems::Chain(chain) => {
                let state = chain.lock();
                for v in state.produced.iter() {
                    v.gc_trace(visit);
                }
                state.source.trace_edges(visit);
            }
        }
    }
}
