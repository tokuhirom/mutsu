//! The source elements of a deferred `.map` / `.grep` Seq
//! ([`crate::value::SeqSource::MapGrep`]).

use crate::value::Value;
use std::sync::Arc;

/// The source elements a [`crate::value::SeqSource::MapGrep`] body maps or greps over.
#[derive(Clone)]
pub(crate) enum MapGrepItems {
    /// Materialized at the `.map`/`.grep` call (a Range, a Seq, a hash, a
    /// shaped array's leaves, the listop form's flattened arguments, ...).
    Snapshot(Arc<Vec<Value>>),
    /// A (non-shaped) Array read at pull time, as Rakudo's `.map` iterates
    /// the Array's own iterator: nothing is copied at the call, so
    /// `@a.map(&f).head(3)` is O(1) in `@a.elems`, and `my $m = @a.map(&f);
    /// @a.push(4)` maps the pushed element too.
    Live(Value),
}

impl MapGrepItems {
    /// A live view of `target` when it is a non-shaped, non-itemized Array
    /// (whose list-context elements are exactly its items), else the snapshot
    /// `snapshot` builds.
    // Cost: O(1) for an Array; `snapshot`'s cost otherwise.
    pub(crate) fn of(target: &Value, snapshot: impl FnOnce() -> Vec<Value>) -> Self {
        match target.view() {
            crate::value::ValueView::Array(_, kind)
                if !kind.is_itemized() && !crate::runtime::utils::is_shaped_array(target) =>
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
        }
    }
}
