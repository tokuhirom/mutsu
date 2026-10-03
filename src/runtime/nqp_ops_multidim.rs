//! `nqp::existspos` and the multi-dimensional positional ops (#11493):
//! `atpos2d` / `atpos3d` / `atposnd` and `bindpos2d` / `bindpos3d` /
//! `bindposnd`, each with its `_i` / `_n` / `_s` twins.
//!
//! A link of the chained `nqp::` tables (`... -> nqp_ops_list -> here ->
//! nqp_ops_native`).
//!
//! mutsu stores a shaped array (`my @a[2;2]`, `array[int].new(:shape(2,2))`)
//! as nested rows, and its `$!reified` is the array itself, so a
//! multi-dimensional op walks every index but the last with the one-dimensional
//! element read (`nqp_backing::elem_at`, MoarVM's index rule), then hands the
//! row and the last index to the matching ONE-dimensional op (`atpos_i`,
//! `bindpos_s`, ...). There is no second element-access implementation to
//! drift: `atpos2d_i($a, $i, $j)` is `atpos_i(atpos($a, $i), $j)`.
//!
//! Rakudo rejects these ops on a non-shaped (`P6opaque`) Array; mutsu does not
//! record shape on the value (the declared dims live beside the variable), so
//! a nested array of arrays is accepted as well. A missing row is the error
//! MoarVM gives for an index outside a dimension.

use super::*;
use crate::value::ValueView;

/// The indices of an `nqp::*nd` op: the `int` list it takes as its second
/// operand.
fn nd_indices(v: &Value) -> Result<Vec<i64>, RuntimeError> {
    match v.view() {
        ValueView::Array(items, _) => Ok(items.iter().map(crate::runtime::to_int).collect()),
        _ => Err(RuntimeError::new(
            "nqp::atposnd/bindposnd: the indices must be an int list",
        )),
    }
}

/// Whether `op` is an `nd` form (`atposnd*` / `bindposnd*`), which takes its
/// indices as one list. (`"bindpos2d"` itself contains "nd", so the test is on
/// the prefix.)
fn is_nd(op: &str) -> bool {
    op.starts_with("atposnd") || op.starts_with("bindposnd")
}

/// The one-dimensional op a multi-dimensional op ends in: `atpos2d_i` →
/// `atpos_i`, `bindposnd` → `bindpos`.
fn one_dim_op(op: &str) -> &'static str {
    let read = op.starts_with("atpos");
    match (read, op.rsplit_once('_').map(|(_, kind)| kind)) {
        (true, Some("i")) => "atpos_i",
        (true, Some("n")) => "atpos_n",
        (true, Some("s")) => "atpos_s",
        (true, _) => "atpos",
        (false, Some("i")) => "bindpos_i",
        (false, Some("n")) => "bindpos_n",
        (false, Some("s")) => "bindpos_s",
        (false, _) => "bindpos",
    }
}

impl Interpreter {
    /// Try `existspos` or a multi-dimensional positional op. `None` means
    /// "not an op this table knows".
    pub(crate) fn call_nqp_op_multidim(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::existspos($list, $i): 1 when slot `$i` (negative counts
            // from the end) holds a value -- not past the end, not a hole a
            // past-the-end store opened, not a deleted or null slot. The same
            // predicate `@a[$i]:exists` reads (`ArrayData::hole_at`).
            // Cost: O(1).
            "existspos" => {
                let target = args
                    .first()
                    .cloned()
                    .unwrap_or(Value::NIL)
                    .deref_container();
                let idx = args.get(1).map(crate::runtime::to_int).unwrap_or(0);
                let exists = match target.view() {
                    ValueView::Array(items, _) => {
                        let len = items.len() as i64;
                        let i = if idx < 0 { len + idx } else { idx };
                        (0..len).contains(&i)
                            && !items.hole_at(i as usize)
                            && !matches!(
                                items.get(i as usize).map(Value::view),
                                Some(ValueView::Nil)
                            )
                    }
                    _ => match Interpreter::nqp_elems_len_of(&target) {
                        Some(len) => {
                            let len = len as i64;
                            let i = if idx < 0 { len + idx } else { idx };
                            (0..len).contains(&i)
                        }
                        None => false,
                    },
                };
                Ok(Value::int(i64::from(exists)))
            }

            // nqp::atpos2d / 3d / nd (and _i/_n/_s): walk to the row, then the
            // one-dimensional read.
            // Cost: O(d), d = dimensions (one O(1) element read per index).
            "atpos2d" | "atpos2d_i" | "atpos2d_n" | "atpos2d_s" | "atpos3d" | "atpos3d_i"
            | "atpos3d_n" | "atpos3d_s" | "atposnd" | "atposnd_i" | "atposnd_n" | "atposnd_s" => {
                let indices = match self.multidim_indices(op, args) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                self.multidim_tail(op, args, &indices, None)
            }
            // nqp::bindpos2d / 3d / nd (and _i/_n/_s): walk to the row, then
            // the one-dimensional bind (in place, shared with every holder).
            // Cost: O(d), d = dimensions; growing the last row as bindpos does.
            "bindpos2d" | "bindpos2d_i" | "bindpos2d_n" | "bindpos2d_s" | "bindpos3d"
            | "bindpos3d_i" | "bindpos3d_n" | "bindpos3d_s" | "bindposnd" | "bindposnd_i"
            | "bindposnd_n" | "bindposnd_s" => {
                let indices = match self.multidim_indices(op, args) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                let val_at = if is_nd(op) { 2 } else { indices.len() + 1 };
                let val = args.get(val_at).cloned().unwrap_or(Value::NIL);
                self.multidim_tail(op, args, &indices, Some(val))
            }
            _ => return self.call_nqp_op_native(op, args),
        })
    }

    /// The index list of a multi-dimensional op: the `2d`/`3d` forms take the
    /// indices as positional operands, the `nd` forms as one int list.
    fn multidim_indices(&self, op: &str, args: &[Value]) -> Result<Vec<i64>, RuntimeError> {
        let iarg = |i: usize| args.get(i).map(crate::runtime::to_int).unwrap_or(0);
        if is_nd(op) {
            nd_indices(&args.get(1).cloned().unwrap_or(Value::NIL).deref_container())
        } else if op.contains("3d") {
            Ok(vec![iarg(1), iarg(2), iarg(3)])
        } else {
            Ok(vec![iarg(1), iarg(2)])
        }
    }

    /// Walk every index but the last, then run the one-dimensional op on the
    /// row reached (a read when `val` is `None`, a bind otherwise).
    fn multidim_tail(
        &mut self,
        op: &str,
        args: &[Value],
        indices: &[i64],
        val: Option<Value>,
    ) -> Result<Value, RuntimeError> {
        let Some((&last, outer)) = indices.split_last() else {
            return Err(RuntimeError::new(format!(
                "nqp::{op}: needs at least one index"
            )));
        };
        let mut row = args
            .first()
            .cloned()
            .unwrap_or(Value::NIL)
            .deref_container();
        for &i in outer {
            row = match crate::runtime::nqp_backing::elem_at(&row, i)? {
                Some(next) => next.deref_container(),
                None => {
                    return Err(RuntimeError::new(format!(
                        "nqp::{op}: index {i} out of range for this dimension"
                    )));
                }
            };
        }
        let mut one_dim_args = vec![row, Value::int(last)];
        one_dim_args.extend(val);
        self.dispatch_nqp_op(one_dim_op(op), &one_dim_args)
    }
}

#[cfg(test)]
mod tests {
    use super::one_dim_op;

    #[test]
    fn multi_dimensional_ops_end_in_their_one_dimensional_twin() {
        assert_eq!(one_dim_op("atpos2d"), "atpos");
        assert_eq!(one_dim_op("atpos3d_i"), "atpos_i");
        assert_eq!(one_dim_op("atposnd_s"), "atpos_s");
        assert_eq!(one_dim_op("bindpos2d_n"), "bindpos_n");
        assert_eq!(one_dim_op("bindposnd"), "bindpos");
    }
}
