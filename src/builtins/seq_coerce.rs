// Pure `.Seq` coercion over structural receivers (Seq/Array/Slip/Range and bare
// scalars). These carry no interpreter state: they wrap the receiver's elements
// in a `Seq`. Single authoritative impl shared by the bytecode VM
// (native dispatch) and the interpreter's `dispatch_seq_coercion`.
//
// Receivers whose `.Seq` needs interpreter state / a carrier — a `Supply`
// (drains its on-demand callback / value buffer), a `LazyList` (forced through
// the lazy bridge), or a `Buf`/`Blob` Instance (reads the `bytes` attribute) —
// return `None` here and are handled by the interpreter.
//
// Spec: https://docs.raku.org/routine/Seq

use crate::runtime::utils::value_to_list;
use crate::value::{Value, ValueView};

/// Coerce a structural receiver to a `Seq`, or `None` to fall through to the
/// interpreter (Supply / LazyList / any Instance — including Buf/Blob and
/// generic objects, which the interpreter wraps after its special-cases).
pub(crate) fn to_seq_structural(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Seq(_) => Some(target.clone()),
        ValueView::Array(items, ..) => Some(Value::seq(items.to_vec())),
        ValueView::Slip(items) => Some(Value::seq(items.to_vec())),
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => Some(Value::seq(value_to_list(target))),
        // `Any.Seq` is `self.list.Seq`, and a hash's (or Set/Bag/Mix's) list is its
        // Pairs: `%h.Seq` is `(:a(1),).Seq`, NOT a one-element Seq holding the
        // hash (#10758). The receiver is decontainerized, so an itemized hash
        // decomposes too.
        ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => Some(
            Value::seq(crate::runtime::utils::value_to_list_for_receiver(target)),
        ),
        // Supply (carrier), LazyList (lazy bridge), and any Instance (Buf/Blob
        // byte read, or the generic 1-element wrap) are left to the interpreter.
        ValueView::LazyList(_) | ValueView::Instance { .. } => None,
        // A bare scalar becomes a one-element Seq (matches the interpreter's
        // `other => Seq([other])` tail).
        _ => Some(Value::seq(vec![target.clone()])),
    }
}
