//! The list-associative chain of the sequence operator:
//! `infix:<...>(a, b, c, ...)`, which `1 ... 3, 10 ... 20` (and the
//! parenthesized form of the same) lowers to.

use super::*;

impl Interpreter {
    /// Evaluate `infix:<...>(op0, op1, ..., opM)` as rakudo does. Each operand
    /// is a list. The first element of operand `i` (`i >= 1`) is the endpoint
    /// of segment `i-1`. When that endpoint is a number or a string, segment
    /// `i-1` stops short of it and it also seeds segment `i`. Any other
    /// endpoint (a closure, `*`) is consumed by segment `i-1`. Segment `i`
    /// runs from operand `i`'s remaining elements, and the last operand's
    /// elements follow the last segment:
    /// `1 ... (3, 10) ... 20` is `1 2`, `3 10 17`, `20`.
    // Cost: O(n), n = elements produced.
    pub(super) fn eval_chained_sequence(
        &mut self,
        args: &[Value],
        exclude_end: bool,
    ) -> Result<Value, RuntimeError> {
        // ADR-0040 slices 1-2: each argument is a seed/endpoint list the
        // operator DECOMPOSES, so strip the itemization an element carries.
        let args: Vec<Value> = args
            .iter()
            .map(|v| v.clone().deitemize_element())
            .collect();
        if args.len() < 3 {
            let left = args.first().cloned().unwrap_or(Value::NIL);
            let right = args.last().cloned().unwrap_or(Value::NIL);
            return self.eval_sequence(left, right, exclude_end);
        }
        let mut groups: Vec<Vec<Value>> = args.iter().map(Self::chain_operand_items).collect();
        let last = groups.len() - 1;
        let mut ends = Vec::with_capacity(last);
        let mut exclusive = Vec::with_capacity(last);
        for group in groups.iter_mut().skip(1) {
            let end = group.first().cloned().unwrap_or(Value::NIL);
            let stops_short = Self::chain_endpoint_seeds_next(&end);
            if !stops_short && !group.is_empty() {
                group.remove(0);
            }
            ends.push(end);
            exclusive.push(stops_short);
        }
        // Finite runs collect in `out`; an infinite segment is kept as the
        // lazy list it is, so `(1 ... 4, 8 ... *)[^10]` stays lazy.
        let mut out: Vec<Value> = Vec::new();
        let mut parts: Vec<Value> = Vec::new();
        for i in 0..last {
            let seeds = &groups[i];
            if seeds.is_empty() {
                continue;
            }
            let left = if seeds.len() == 1 {
                seeds[0].clone()
            } else {
                Value::array(seeds.clone())
            };
            let segment = self.eval_sequence(left, ends[i].clone(), exclusive[i])?;
            match segment.view() {
                ValueView::LazyList(ll) if !ll.has_finite_closure_endpoint() => {
                    if !out.is_empty() {
                        parts.push(Value::array(std::mem::take(&mut out)));
                    }
                    parts.push(segment.clone());
                }
                _ => out.extend(self.chain_segment_items(segment)?),
            }
        }
        out.extend(groups[last].iter().cloned());
        if parts.is_empty() {
            return Ok(Value::array(out));
        }
        if !out.is_empty() {
            parts.push(Value::array(out));
        }
        Ok(Self::lazy_concat_pipe(parts, false))
    }

    /// An operand of the chain as the list of its elements: a list or a
    /// range gives its elements, any other value is a one-element list.
    // Cost: O(n), n = elements of the operand.
    fn chain_operand_items(value: &Value) -> Vec<Value> {
        match value.view() {
            ValueView::Array(items, ..) => items.iter().cloned().collect(),
            ValueView::Seq(_)
            | ValueView::Slip(_)
            | ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(value),
            _ => vec![value.clone()],
        }
    }

    /// Whether a chain endpoint is a value (a number or a string) that a
    /// segment stops short of and that seeds the next segment, rather than a
    /// matcher the segment consumes.
    // Cost: O(1).
    fn chain_endpoint_seeds_next(end: &Value) -> bool {
        end.is_numeric() || matches!(end.view(), ValueView::Str(_))
    }

    /// The elements one segment produced.
    // Cost: O(n), n = elements of the segment.
    fn chain_segment_items(&mut self, segment: Value) -> Result<Vec<Value>, RuntimeError> {
        Ok(match segment.view() {
            ValueView::Array(items, ..) => items.iter().cloned().collect(),
            ValueView::Seq(_) => crate::runtime::utils::value_to_list(&segment),
            ValueView::LazyList(ll) => self.force_lazy_list_vm(&ll)?,
            _ => vec![segment],
        })
    }
}
