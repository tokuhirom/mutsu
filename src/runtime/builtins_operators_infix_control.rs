//! Sequence and short-circuit infix routines with non-folding behavior.
use super::*;
use crate::value::ValueView;

impl Interpreter {
    // Cost: O(n), n = operand count plus produced sequence length.
    pub(super) fn call_infix_sequence_or_short(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match op {
            "..." | "...^" | "^..." | "^...^" => {
                let exclude_end = op == "...^" || op == "^...^";
                let exclude_start = op.starts_with('^');

                let mut result = if args.len() > 2 {
                    // Chained sequence: multiple waypoints
                    // args = [seed, waypoint1, waypoint2, ..., endpoint]
                    // Each waypoint can be a list: first element is endpoint for
                    // previous segment, whole list is seed for next segment.
                    self.eval_chained_sequence(args, exclude_end)?
                } else {
                    let left = args[0].clone();
                    let right = args.last().cloned().unwrap_or(Value::NIL);
                    self.eval_sequence(left, right, exclude_end)?
                };

                if exclude_start {
                    // Remove the first element
                    let new_result = match result.view() {
                        ValueView::Array(items, kind) if !items.is_empty() => {
                            let new_items = items[1..].to_vec();
                            Some(if kind.is_real_array() {
                                Value::real_array(new_items)
                            } else {
                                Value::array(new_items)
                            })
                        }
                        ValueView::LazyList(ll) => Some(Value::lazy_list(crate::gc::Gc::new(
                            crate::value::LazyList::new_skip_first_pipe(Value::lazy_list(
                                ll.clone(),
                            )),
                        ))),
                        _ => None,
                    };
                    if let Some(new_result) = new_result {
                        result = new_result;
                    }
                }
                return Ok(Some(result));
            }
            "andthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if !crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Some(Value::slip_arc(std::sync::Arc::new(vec![]))));
                    }
                    acc = rhs.clone();
                }
                return Ok(Some(acc));
            }
            "notandthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Some(Value::NIL));
                    }
                    acc = rhs.clone();
                }
                return Ok(Some(acc));
            }
            "orelse" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Some(acc));
                    }
                    acc = rhs.clone();
                }
                return Ok(Some(acc));
            }
            "//" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Some(acc));
                    }
                    acc = if let ValueView::Sub(_) = rhs.view() {
                        self.call_sub_value(rhs.clone(), vec![])?
                    } else {
                        rhs.clone()
                    };
                }
                return Ok(Some(acc));
            }
            "^^" | "xor" => {
                let mut truthy_val: Option<Value> = None;
                for arg in args {
                    let val = if let ValueView::Sub(_) = arg.view() {
                        self.call_sub_value(arg.clone(), vec![])?
                    } else {
                        arg.clone()
                    };
                    if val.truthy() {
                        if truthy_val.is_some() {
                            return Ok(Some(Value::NIL));
                        }
                        truthy_val = Some(val);
                    }
                }
                return Ok(Some(truthy_val.unwrap_or(Value::NIL)));
            }
            _ => {}
        }
        Ok(None)
    }
}
