//! Atomic fetch / store / read-modify-write on an array or hash ELEMENT
//! (`atomic-fetch-inc(@a[0])`, `@a[0]⚛++`, `nqp::atomicinc_i(@a[0])`), #11812.
//!
//! The element is the same Track B element cell that `cas(@a[0], ...)` swaps
//! (`init_celled_atomic_store` / `celled_array_elem` / `celled_hash_elem`), so
//! every atomic form on one element shares a single lock: an increment cannot
//! interleave with a `cas` on the same element. The variable-target forms live
//! in `builtins_atomic`; this is their element counterpart.

use super::*;
use crate::runtime::shared_store::atomic_lane_str_key;
use crate::value::ValueView;

/// What `__mutsu_atomic_elem` does to the element.
#[derive(Clone, Copy)]
enum ElemOp {
    Fetch,
    Store,
    /// Add the operand and answer the value before the add.
    FetchAdd,
    /// Add the operand and answer the value after the add.
    AddFetch,
}

impl Interpreter {
    /// `__mutsu_atomic_elem(container_name, key, op, operand?)`: the compiler's
    /// lowering of an atomic routine whose target is `@arr[i]` / `%h{k}`.
    /// `op` is `fetch`, `store`, `fetch-add` or `add-fetch`; an increment is an
    /// add of 1, a decrement an add of -1.
    // Cost: O(1) amortized (a lock on the element cell). The container's
    // first atomic touch boxes its elements once, O(e), e = its elements.
    pub(super) fn builtin_atomic_elem(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_atomic_elem requires a container name, a key and an op",
            ));
        }
        let name = args[0].to_string_value();
        let op = match args[2].to_string_value().as_str() {
            "fetch" => ElemOp::Fetch,
            "store" => ElemOp::Store,
            "fetch-add" => ElemOp::FetchAdd,
            "add-fetch" => ElemOp::AddFetch,
            other => {
                return Err(RuntimeError::new(format!(
                    "__mutsu_atomic_elem: unknown op '{other}'"
                )));
            }
        };
        let operand = args.get(3).cloned().unwrap_or(Value::NIL);
        let is_hash = name.starts_with('%');
        if matches!(op, ElemOp::Store | ElemOp::FetchAdd | ElemOp::AddFetch) {
            self.check_readonly_for_modify(&name)?;
        }
        let atomic_key = atomic_lane_str_key(&name, is_hash);
        self.init_celled_atomic_store(atomic_key, &name);
        let cell = if is_hash {
            let key = args[1].to_string_value();
            self.celled_hash_elem(atomic_key, &name, &key)
        } else {
            let index = match args[1].view() {
                ValueView::Int(i) => i,
                _ => args[1].to_string_value().parse::<i64>().unwrap_or(0),
            };
            self.celled_array_elem(atomic_key, &name, index)
        };
        let result = match op {
            ElemOp::Fetch => {
                // The one lenient op that reads no element type of its own: a
                // narrow native-int array refuses it too (#12008).
                self.refuse_narrow_element(&name)?;
                let guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                guard.clone()
            }
            ElemOp::Store => {
                // Typed container: reject a wrong-typed value as a plain
                // element assignment would; the element is a `Scalar`, so an
                // aggregate is itemized on the way in (as `cas` does).
                self.check_atomic_elem_type(&name, &operand)?;
                let value = Self::itemize_value_for_element_store(operand);
                let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                *guard = value.clone();
                value
            }
            ElemOp::FetchAdd | ElemOp::AddFetch => {
                let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                let current = match guard.view() {
                    ValueView::Nil | ValueView::Package(_) => Value::int(0),
                    _ => guard.clone(),
                };
                let next = crate::builtins::arith_add(current.clone(), operand)?;
                *guard = next.clone();
                if matches!(op, ElemOp::FetchAdd) {
                    current
                } else {
                    next
                }
            }
        };
        if !matches!(op, ElemOp::Fetch)
            && let Ok(mut dirty) = self.threads.shared_vars_dirty.write()
        {
            dirty.insert(name);
        }
        Ok(result)
    }
}
