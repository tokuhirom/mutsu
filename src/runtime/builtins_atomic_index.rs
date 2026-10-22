//! Index resolution shared by the element atomics (`cas(@a[i], ...)`,
//! `atomic-fetch-inc(@a[i])`, ...), #12695.

use super::*;
use crate::runtime::shared_store::atomic_lane_str_key;
use crate::value::ValueView;

impl Interpreter {
    /// The array index named by `index`: an `Int`, a numeric string, or a
    /// `WhateverCode` (`*-1`) resolved against the container's current length.
    // Cost: O(1) amortized, plus one call of the WhateverCode; the
    // container's first atomic touch boxes its elements once, O(e).
    pub(super) fn atomic_elem_index(
        &mut self,
        arr_name: &str,
        index: &Value,
    ) -> Result<i64, RuntimeError> {
        match index.view() {
            ValueView::Int(i) => Ok(i),
            ValueView::Sub(_) => {
                let atomic_key = atomic_lane_str_key(arr_name, false);
                self.init_celled_atomic_store(atomic_key, arr_name);
                let len = {
                    let root = self.threads.shared_vars.atomic_lane_scope(arr_name);
                    let shared = root.own_map().read().unwrap();
                    match shared.get(atomic_key).map(Value::view) {
                        Some(ValueView::Array(items, ..)) => items.len() as i64,
                        _ => 0,
                    }
                };
                let resolved = self.call_sub_value(index.clone(), vec![Value::int(len)])?;
                Ok(match resolved.view() {
                    ValueView::Int(i) => i,
                    _ => resolved.to_string_value().parse::<i64>().unwrap_or(0),
                })
            }
            _ => Ok(index.to_string_value().parse::<i64>().unwrap_or(0)),
        }
    }
}
