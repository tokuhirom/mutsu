//! Normalizing a positional subscript before it is carried as a key string.
use super::*;

impl Interpreter {
    /// A positional subscript that is a `Bool`, a non-integer real or a
    /// numeric enum value names the element its `Int` does (`@a[True]` is
    /// `@a[1]`, `@a[1.7]` is `@a[1]`, `@a[B]` of `enum <A B>` is `@a[1]`).
    /// The chained element stores carry their keys as strings, where `"False"`,
    /// `"1.7"` or `"B"` would name no element; every other index passes
    /// unchanged.
    // Cost: O(1).
    pub(super) fn positional_index_as_int(index: Value) -> Value {
        match index.view() {
            ValueView::Bool(b) => Value::int(i64::from(b)),
            // `Inf` and `NaN` name no element and have no `Int`: they pass
            // unchanged, so the store's own check raises "Cannot convert Inf to
            // Int" instead of a wrapped `0` addressing a real slot.
            ValueView::Num(f) if !f.is_finite() => index,
            ValueView::Num(_) | ValueView::Rat(..) | ValueView::FatRat(..) => {
                Value::int(crate::runtime::to_int(&index))
            }
            ValueView::Enum { value, .. } => match value.as_index_i64() {
                Some(i) => Value::int(i),
                None => index,
            },
            _ => index,
        }
    }
}
