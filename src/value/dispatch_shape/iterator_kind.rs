//! Which opaque shape a built-in iterator has (ADR-11276 §9.126, #14124).
//!
//! mutsu's built-in iterators are instances of one class, `Iterator`, whose
//! state lives in the instance's attributes: an array-backed cursor
//! (`items`/`index`, topped up from a `lazy_source`), a deferred `.map`/`.grep`
//! stream, or a `squish` scan. Rakudo's are one class per kind, each doing
//! `Iterator` and, when it knows its count, `PredictiveIterator`. The shape
//! decodes that from the attributes, since one class name has two shapes.

use super::DispatchShape;
use crate::value::{Value, ValueView};

/// The attribute a deferred `.map`/`.grep` stream keeps its private body under
/// (`runtime::iterator_map_grep_stream` re-exports it).
pub(crate) const MAP_GREP_STREAM_ATTR: &str = "map_grep_stream";

/// The shape of an `Iterator` instance with these attributes:
/// `PredictiveIterator` for an array-backed one that knows how many elements
/// it has left, `Iterator` alone for a stream (Rakudo's map iterator), a
/// `squish` scan (`Any::Squish-With`) and an iterator over a lazy source with
/// no known count (a `gather`, an unending range).
// Cost: O(1), four attribute probes.
pub(crate) fn shape_of<'a>(mut attribute: impl FnMut(&str) -> Option<&'a Value>) -> DispatchShape {
    let array_backed = matches!(
        attribute("items").map(Value::view),
        Some(ValueView::Array(..))
    );
    let predictive = array_backed
        && attribute(MAP_GREP_STREAM_ATTR).is_none()
        && attribute("squish_source").is_none()
        && (attribute("known_count").is_some() || attribute("lazy_source").is_none());
    if predictive {
        DispatchShape::PredictiveIterator
    } else {
        DispatchShape::Iterator
    }
}
