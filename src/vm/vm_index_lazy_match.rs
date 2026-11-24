//! `$m[$i]` on a lazy Match (a regex match or a grammar cursor), answered
//! without walking the general subscript path's preamble (#18416).
//!
//! An action method reads its positional captures this way (`$/[0]`, Cro::Uri's
//! `@$0` loop), once per parsed node. The general path
//! (`exec_index_op_with_positional`) reaches the Match arm only after probing
//! the receiver for an `is Array` parent, a `Sequence` role with its own
//! `iterator`, an enum aggregate and a user `postcircumfix`, which cost ~6k
//! instructions per read; the arm itself is one slot of the memoized capture
//! list.

use super::*;

impl Interpreter {
    /// The element `target[index]` when `target` is a lazy Match and `index` a
    /// non-negative `Int`, or `None` to leave the read to the general path.
    ///
    /// For that shape the general path answers the Match arm: nothing before it
    /// applies to a lazy Match receiver with an `Int` index, except a class that
    /// does `Sequence` with its own `iterator` (subscripted through its
    /// elements) and a user `postcircumfix:<[ ]>`. The caller checks the second;
    /// a grammar cursor whose class does `Sequence` declines here, and a plain
    /// `Match` never does it.
    // Cost: O(1) once the Match node is materialized; O(c) on its first read,
    // c = the node's captures.
    #[inline]
    pub(super) fn lazy_match_positional_element(
        &self,
        target: &Value,
        index: &Value,
    ) -> Option<Value> {
        if !target.is_lazy_match_value() {
            return None;
        }
        let ValueView::Int(i) = index.view() else {
            return None;
        };
        let i = usize::try_from(i).ok()?;
        let class = target.match_dispatch_class();
        if class != "Match" && self.class_does_role(class, "Sequence") {
            return None;
        }
        target.lazy_match_positional_capture(i)
    }
}
