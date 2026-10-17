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
