//! The routines behind the late rows `dispatch_method_by_name_3` reaches by
//! the receiver's MRO (ADR-11276 §9.139) that are not one of that cascade's
//! own `dispatch_*` helpers.

use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `Seq.Numeric`: the count of a `Seq` backed by a `PredictiveIterator`
    /// (its `count-only`), or its elements'. `None` for any other receiver.
    // Cost: O(1), plus the iterator's `count-only`.
    pub(crate) fn seq_numeric_late(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Seq(items) = target.view() else {
            return None;
        };
        let seq_id = items.identity();
        if let Some(iter) = self.predictive_seq_iter_for(seq_id) {
            return Some(self.call_method_with_values(iter, "count-only", vec![]));
        }
        Some(Ok(Value::int(items.len() as i64)))
    }

    /// The `count-only` of the built-in iterator a `Seq.new($iterator)` nothing
    /// has pulled will pull from, or `None` when the receiver is not such a
    /// Seq or its iterator cannot predict its count (a lazy source, a user
    /// class). Rakudo's `Seq.elems` asks the iterator's `count-only`, which
    /// advances nothing for an Array or Range iterator, so the Seq and the
    /// iterator stay usable afterwards.
    // Cost: O(1) for an array-backed iterator.
    pub(crate) fn seq_unpulled_count_only(&mut self, target: &Value) -> Option<Value> {
        let ValueView::Seq(body) = target.view() else {
            return None;
        };
        let iterator = body.unpulled_iterator()?;
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = iterator.view()
        else {
            return None;
        };
        // Only an iterator over a plain element array (`@a.iterator`,
        // `(1..5).iterator`) predicts its count: a `.map`/`.grep` stream or a
        // `squish` scan counts by running, like Rakudo's.
        if class_name != "Iterator"
            || !attributes.contains_key("items")
            || attributes.contains_key(crate::value::MAP_GREP_STREAM_ATTR)
            || attributes.contains_key("squish_source")
        {
            return None;
        }
        self.iterator_count_only_from_attrs(&attributes.to_map())
            .ok()
            .flatten()
    }

    /// `Date.Date` / `.DateTime` (and `DateTime`'s) on a type object: the
    /// coercion's own type object. `None` for an instance, whose answer is
    /// the temporal rows'.
    // Cost: O(1).
    pub(crate) fn temporal_type_coercion_late(target: &Value, to: &str) -> Option<Value> {
        matches!(target.view(), ValueView::Package(_)).then(|| Value::package(Symbol::intern(to)))
    }

    /// `Thread.run`: start a `Thread.new`-constructed thread, answering the
    /// invocant itself rather than a rebuilt copy.
    // Cost: O(1), plus the thread's start.
    pub(crate) fn thread_run_late(&mut self, target: &Value) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        if class_name != "Thread" {
            return None;
        }
        let attrs = attributes.as_map().clone();
        Some(self.dispatch_thread_run(target, &attrs))
    }
}
