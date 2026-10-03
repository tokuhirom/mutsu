//! Normalizing a positional subscript before it is carried as a key string.
use super::*;

impl Interpreter {
    /// A positional subscript that is a `Bool` or a non-integer real names the
    /// element its `Int` does (`@a[True]` is `@a[1]`, `@a[1.7]` is `@a[1]`).
    /// The chained element stores carry their keys as strings, where `"False"`
    /// or `"1.7"` would name no element; every other index passes unchanged.
    // Cost: O(1).
    pub(super) fn positional_index_as_int(index: Value) -> Value {
        match index.view() {
            ValueView::Bool(b) => Value::int(i64::from(b)),
            ValueView::Num(_) | ValueView::Rat(..) | ValueView::FatRat(..) => {
                Value::int(crate::runtime::to_int(&index))
            }
            _ => index,
        }
    }
}
