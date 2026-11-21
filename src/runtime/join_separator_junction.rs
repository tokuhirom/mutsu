//! A Junction separator of `join` / `.join`: rakudo's `Str() $separator`
//! coercion autothreads, so the join runs once per eigenstate and the answers
//! form a Junction of the same kind (#18221).

use super::*;

impl Interpreter {
    /// `join_once(separator)` for each eigenstate of `separator`, collected as
    /// a Junction of its kind; `None` when `separator` is no Junction, so the
    /// caller joins as usual.
    // Cost: O(e * j), e = eigenstates, j = cost of one join.
    pub(crate) fn thread_join_separator(
        &mut self,
        separator: Option<&Value>,
        mut join_once: impl FnMut(&mut Self, Value) -> Result<Value, RuntimeError>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Junction { kind, values } = separator?.view() else {
            return None;
        };
        let mut joined = Vec::with_capacity(values.len());
        for eigenstate in values.iter() {
            match join_once(self, eigenstate.clone()) {
                Ok(value) => joined.push(value),
                Err(err) => return Some(Err(err)),
            }
        }
        Some(Ok(Value::junction(kind, joined)))
    }
}
