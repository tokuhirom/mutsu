//! The routine behind `Any`'s late positional rows (ADR-11276 §9.138):
//! `AT-POS`, `EXISTS-POS`, `ASSIGN-POS`, `BIND-POS` and `DELETE-POS` of a list
//! or array with several indices, a shaped array's bounds and a native array's
//! refusals. They were an `Array` block of `call_method_with_values`.

use super::methods::{
    check_shaped_bounds, make_not_enough_dimensions_error, multidim_assign_pos,
    multidim_at_pos, multidim_bind_pos, multidim_delete_pos, multidim_exists_pos,
    shaped_multidim_exists_pos,
};
use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// One of the five positional methods on a list or array; `None` when the
    /// call is not one this routine answers (one index on a plain array is
    /// the subscript rows').
    // Cost: O(d), d = dimensions of a shaped invocant (`shaped_array_shape`)
    // or indices of the call; O(1) otherwise. The element type is read off the
    // container (`ArrayData::value_type`), not looked up through the env.
    pub(crate) fn array_pos_late(
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Array(items, _) = target.view() else {
            return None;
        };
        // Detect shaped array and native typed array properties
        let shape = crate::runtime::utils::shaped_array_shape(target);
        let is_native = items
            .value_type
            .as_deref()
            .is_some_and(crate::runtime::native_types::is_native_array_element_type);

        // For shaped arrays, validate dimension counts for ASSIGN-POS and DELETE-POS
        if let Some(ref shape) = shape {
            let needed_dims = shape.len();
            let (action, got_dims) = match method {
                "ASSIGN-POS" if args.len() >= 2 => ("assign to", args.len() - 1),
                "DELETE-POS" => ("delete from", args.len()),
                _ => ("", needed_dims),
            };
            if got_dims < needed_dims {
                return Some(Err(make_not_enough_dimensions_error(
                    action,
                    got_dims,
                    needed_dims,
                )));
            }
        }

        // Multi-arg *-POS methods on arrays: dig into nested arrays.
        // `BIND-POS($idx, $value)` and `ASSIGN-POS($idx, $value)` are the
        // one-index forms below.
        if args.len() >= 2 && !(matches!(method, "ASSIGN-POS" | "BIND-POS") && args.len() < 3) {
            let answer = match method {
                "AT-POS" => shape
                    .as_ref()
                    .map_or(Ok(()), |shape| check_shaped_bounds(shape, args))
                    .map(|()| multidim_at_pos(target, args)),
                "EXISTS-POS" => Ok(Value::truth(match shape {
                    Some(ref shape) if !is_native => {
                        shaped_multidim_exists_pos(target, args, shape)
                    }
                    _ => multidim_exists_pos(target, args),
                })),
                // Cost: O(d), d = dimensions (in place through each level's node).
                "ASSIGN-POS" => {
                    let (indices, value) = args.split_at(args.len() - 1);
                    shape
                        .as_ref()
                        .map_or(Ok(()), |shape| check_shaped_bounds(shape, indices))
                        .and_then(|()| multidim_assign_pos(target, indices, value[0].clone()))
                        .map(|_| value[0].clone())
                }
                "BIND-POS" => {
                    if is_native {
                        return Some(Err(RuntimeError::new(
                            "Cannot bind to a natively typed array",
                        )));
                    }
                    let (indices, value) = args.split_at(args.len() - 1);
                    multidim_bind_pos(target, indices, value[0].clone()).map(|_| value[0].clone())
                }
                "DELETE-POS" => {
                    if is_native {
                        return Some(Err(RuntimeError::new(
                            "Cannot delete from a natively typed array",
                        )));
                    }
                    shape
                        .as_ref()
                        .map_or(Ok(()), |shape| check_shaped_bounds(shape, args))
                        .and_then(|()| multidim_delete_pos(target, args))
                }
                _ => return None,
            };
            return Some(answer);
        }
        match (method, args) {
            // Cost: O(1) expected (bounds check plus an `initialized` HashSet
            // probe; a sparse array is stored densely, as in Rakudo).
            ("EXISTS-POS", [idx]) => {
                let index = match idx.view() {
                    ValueView::Int(i) if i >= 0 => Some(i as usize),
                    ValueView::Num(f) if f >= 0.0 => Some(f as usize),
                    _ => None,
                };
                // An in-range slot may still be a hole — a deleted element or
                // an unassigned gap — and reporting those as existing made
                // `:exists` disagree with itself once it started dispatching
                // through EXISTS-POS (a mixin). A shaped array is no exception:
                // it is fixed-size, but `my @a[3]` starts with every slot
                // unassigned.
                Some(Ok(Value::truth(
                    index.is_some_and(|i| i < items.len() && !items.hole_at(i)),
                )))
            }
            // Cost: O(1) amortized, in place through the shared node (as ASSIGN-POS).
            ("BIND-POS", [idx, value]) => {
                if is_native {
                    return Some(Err(RuntimeError::new("Cannot bind to a natively typed array")));
                }
                let index = match idx.view() {
                    ValueView::Int(i) if i >= 0 => Some(i as usize),
                    ValueView::Num(f) if f >= 0.0 => Some(f as usize),
                    _ => None,
                };
                let Some(index) = index else {
                    return Some(Err(RuntimeError::new("Cannot BIND-POS with a negative index")));
                };
                // SAFETY: audited aliased in-place container write (see
                // value::aliased_mut); no borrow into the node is live.
                let data = unsafe { crate::value::gc_contents_mut(&items) };
                data.store_element(index, Value::bound_element(value.clone()));
                Some(Ok(value.clone()))
            }
            _ => None,
        }
    }
}
