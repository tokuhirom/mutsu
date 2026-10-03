//! The one-operand form of the `[~]` reduction.
//!
//! `[~] x` is `infix:<~>(x)`, and which `infix:<~>` candidate takes the lone
//! operand decides the result:
//!
//! - `(@args)` is narrower than `($x)` for a Positional, so a lone list, even
//!   an itemized one (`[~] $x` with `$x = (1, 2)`), is joined: "12", not
//!   "1 2";
//! - `(Any \a) { a.Str }` stringifies anything else, so `[~] 5` is the Str
//!   "5" and `[~] Set.new("a","b")` is the set's `.Str`, "a b";
//! - `(Blob:D $a)` returns a Blob unchanged.

use super::*;

impl Interpreter {
    /// Resolve `[~]` over the single element left in `list`. Returns the result
    /// when the candidate rules decide it, or `None` when `list` now holds the
    /// elements of a joined Positional (two or more) or a lone Blob, which the
    /// ordinary fold handles.
    ///
    // Cost: O(e) concatenations, e = elements of the lone Positional.
    pub(super) fn reduce_concat_single(
        &mut self,
        list: &mut Vec<Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        if let ValueView::Array(items, _) = list[0].view() {
            let items: Vec<Value> = items.iter().cloned().collect();
            if items.is_empty() {
                return Ok(Some(Value::str(String::new())));
            }
            *list = items;
            if list.len() > 1 {
                return Ok(None);
            }
        }
        // The Blob test is on the ELEMENT, not the whole operand:
        // `my @chunks = Blob.new; [~] @chunks` arrives with an Array operand
        // holding one Blob, which must not be stringified.
        if Self::value_is_buf(&list[0]) {
            return Ok(None);
        }
        let v = self.reduction_step_with_args(
            crate::compiled_operator::InfixShape::lower("~").as_ref(),
            None,
            vec![Value::str(String::new()), list[0].clone()],
        )?;
        Ok(Some(v))
    }
}
