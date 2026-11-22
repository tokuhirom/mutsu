//! The shared cell a hash element holds, for a `.VAR` subscript or a bind
//! source (`IndexVarRef`).

use super::*;

impl Interpreter {
    /// The cell a hash element holds when it is a caller's container -- a
    /// `*%h` slurpy element bound from `:a($y)`, an element bound with `:=` --
    /// so `%h<a>.VAR` reflects the `Scalar` it is instead of the value it
    /// holds (#18372). `None` for any other receiver or element.
    // Cost: O(1) amortized (one key lookup).
    pub(crate) fn hash_element_cell(target: &Value, key: &Value) -> Option<Value> {
        let ValueView::Str(key) = key.view() else {
            return None;
        };
        let target = target.descalarize();
        let ValueView::Hash(map) = target.view() else {
            return None;
        };
        // A bound `Proxy` is the element's container itself (`%h<k> := $p`
        // reflects as `Proxy`); the ordinary read already answers that.
        map.get(key.as_str())
            .filter(|v| match v.view() {
                ValueView::ContainerRef(cell) => {
                    !matches!(cell.load().view(), ValueView::Proxy { .. })
                }
                _ => false,
            })
            .cloned()
    }
}
