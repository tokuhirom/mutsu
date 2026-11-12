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

/// The attribute that marks an array-backed iterator whose rakudo counterpart
/// is not a `PredictiveIterator` (a hash's).
pub(crate) const NOT_PREDICTIVE_ITERATOR_ATTR: &str = "not_predictive";

/// Whether `value` is one of mutsu's built-in iterators that reports itself
/// lazy (the `is_lazy` attribute): `Some(answer)` for such an iterator, `None`
/// for any other value. The single answer behind every `is-lazy` row.
// Cost: O(1), one attribute probe.
pub(crate) fn native_iterator_is_lazy(value: &Value) -> Option<bool> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Iterator" => Some(matches!(
            attributes.as_map().get("is_lazy").map(Value::view),
            Some(ValueView::Bool(true))
        )),
        _ => None,
    }
}

/// The attribute that marks a built-in iterator over an increasing sequence
/// (a non-empty `Range`): rakudo's range iterator answers `True` to
/// `is-monotonically-increasing`, every other built-in iterator `False`.
pub(crate) const MONOTONIC_ITERATOR_ATTR: &str = "monotonic";

/// The attribute that marks a built-in iterator whose elements are not
/// reproducible (a `gather`, `.pick`, `.roll`): `is-deterministic` is `False`.
pub(crate) const NONDETERMINISTIC_ITERATOR_ATTR: &str = "nondeterministic";

/// `(is-deterministic, is-monotonically-increasing)` of one of mutsu's
/// built-in iterators, `None` for any other value. A deferred `.map`/`.grep`
/// stream and a hash's iterator are never deterministic, as in rakudo.
// Cost: O(1), a few attribute probes.
pub(crate) fn native_iterator_determinism(value: &Value) -> Option<(bool, bool)> {
    match value.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name == "Iterator" => {
            let map = attributes.as_map();
            let monotonic = map.get(MONOTONIC_ITERATOR_ATTR).is_some();
            // An increasing range's iterator (even one pulled through the
            // identity pipe `Range.lazy` builds) is reproducible.
            let deterministic = monotonic
                || (map.get(MAP_GREP_STREAM_ATTR).is_none()
                    && map.get(NOT_PREDICTIVE_ITERATOR_ATTR).is_none()
                    && map.get(NONDETERMINISTIC_ITERATOR_ATTR).is_none());
            Some((deterministic, monotonic))
        }
        _ => None,
    }
}

/// The shape of an `Iterator` instance with these attributes:
/// `PredictiveIterator` for an array-backed one that knows how many elements
/// it has left, `Iterator` alone for a stream (Rakudo's map iterator), a
/// `squish` scan (`Any::Squish-With`) and an iterator over a lazy source with
/// no known count (a `gather`, an unending range).
// Cost: O(1), four attribute probes, each by a pre-interned symbol: this
// runs on every method call on an iterator.
pub(crate) fn shape_of<'a>(
    mut attribute: impl FnMut(crate::symbol::Symbol) -> Option<&'a Value>,
) -> DispatchShape {
    use crate::symbol::wk;
    let array_backed = matches!(
        attribute(wk::iter_items()).map(Value::view),
        Some(ValueView::Array(..))
    );
    let predictive = array_backed
        && attribute(wk::iter_map_grep_stream()).is_none()
        && attribute(wk::iter_squish_source()).is_none()
        && attribute(wk::iter_not_predictive()).is_none()
        && (attribute(wk::iter_known_count()).is_some()
            || attribute(wk::iter_lazy_source()).is_none());
    if predictive {
        DispatchShape::PredictiveIterator
    } else {
        DispatchShape::Iterator
    }
}

#[cfg(test)]
mod tests {
    use crate::symbol::wk;

    /// `shape_of` reads by the pre-interned symbols; each must name the
    /// attribute its string constant (or literal) names where the iterator
    /// instances are built.
    #[test]
    fn iterator_shape_symbols_match_their_names() {
        let pairs = [
            (wk::iter_items(), "items"),
            (wk::iter_map_grep_stream(), super::MAP_GREP_STREAM_ATTR),
            (wk::iter_squish_source(), "squish_source"),
            (wk::iter_not_predictive(), super::NOT_PREDICTIVE_ITERATOR_ATTR),
            (wk::iter_known_count(), "known_count"),
            (wk::iter_lazy_source(), "lazy_source"),
            (wk::iter_index(), "index"),
        ];
        for (sym, name) in pairs {
            assert_eq!(sym.as_str(), name);
        }
    }
}
