//! A Callable `:nth` for `.match` / `.subst` (`:nth(*-1)`, `:nth({2})`,
//! `:nd{2;}`).
//!
//! rakudo calls the Callable once, when the number of matches is known, and
//! uses its answer as the `:nth` value: an Int selects one match (so `.match`
//! answers a single Match, not a list), a list selects several. A
//! WhateverCode (`*-1`) takes the match count as its argument; a bare block
//! (`{2}`) is called with none.

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// The `:nth` value `nth` stands for once `total` matches are known: the
    /// Callable's answer, or `nth` itself when it is no Callable.
    // Cost: O(1) plus one call of the Callable.
    pub(crate) fn resolve_callable_nth(
        &mut self,
        nth: &Value,
        total: usize,
    ) -> Result<Value, RuntimeError> {
        if !matches!(nth.view(), ValueView::Sub(_)) {
            return Ok(nth.clone());
        }
        let arity = match self.code_arity_count(nth, "arity") {
            Some(result) => result?.to_f64() as i64,
            None => 0,
        };
        let args = if arity >= 1 {
            vec![Value::int(total as i64)]
        } else {
            Vec::new()
        };
        self.call_sub_value(nth.clone(), args)
    }

    /// The 1-based indices an `:nth` value `nth` selects out of `total`
    /// matches, ascending and each within `total`, and whether `nth` names a
    /// single match (an Int, a Num, `*`, or a Callable answering one) rather
    /// than a list. A Callable answering an index outside the matches selects
    /// nothing; a literal index below 1 or a list out of order is an error.
    // Cost: O(r) plus one call of a Callable `nth`, r = indices selected.
    pub(crate) fn nth_selection(
        &mut self,
        nth: &Value,
        total: usize,
    ) -> Result<(Vec<usize>, bool), RuntimeError> {
        let resolved = self.resolve_callable_nth(nth, total)?;
        let single =
            Self::is_single_nth(&resolved) || matches!(resolved.view(), ValueView::Whatever);
        if matches!(nth.view(), ValueView::Sub(_))
            && Self::callable_nth_out_of_range(&resolved, total)
        {
            return Ok((Vec::new(), single));
        }
        let mut indices = self.resolve_nth_value_indices(&resolved, total)?;
        indices.retain(|&n| n <= total);
        Ok((indices, single))
    }

    /// Whether a Callable's single-index answer `value` falls outside the
    /// `total` matches: such an answer selects nothing (`:nth(*-5)` on two
    /// matches is Nil), where a literal out-of-range index is an error.
    // Cost: O(1).
    pub(crate) fn callable_nth_out_of_range(value: &Value, total: usize) -> bool {
        matches!(value.view(), ValueView::Int(i) if i < 1 || i as usize > total)
    }
}
