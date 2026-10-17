//! `Routine::WrapHandle.restore`, the handler of its row (ADR-11276 §9.160).

use super::*;

impl Interpreter {
    /// `Routine::WrapHandle.restore`: take the wrapper this handle stands for
    /// out of its routine's (or method candidate's) wrap chain. `True` the
    /// first time, `False` on every later call (raku).
    // Cost: O(w), w = wrappers in the chain.
    pub(crate) fn wrap_handle_restore(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let handle_id = attributes
            .get("handle-id")
            .and_then(|v| v.as_int().map(|i| i as u64));
        // Method-candidate handle (`.^lookup(...).candidates[N].wrap(...)`):
        // ADR-0019 E10's "unwrap method-wrap leak" fix — this handle shape
        // used to carry no way to locate its `Registry::method_wrap_chains`
        // entry, so `.restore()` silently did nothing and still reported
        // success. See `method_wrap_handle_attrs` (`methods_sub.rs`).
        let method_candidate_key = attributes
            .get("wrap-class")
            .and_then(|v| v.as_str().map(str::to_string))
            .zip(
                attributes
                    .get("wrap-method")
                    .and_then(|v| v.as_str().map(str::to_string)),
            )
            .zip(
                attributes
                    .get("wrap-candidate-idx")
                    .and_then(|v| v.as_int().map(|i| i as usize)),
            );
        if let (Some(((cls, meth), idx)), Some(handle_id)) = (method_candidate_key, handle_id) {
            // A second `.restore()` on an already-restored handle is a
            // no-op that answers `False`, not an error (raku: `$h.restore`
            // is `True` once, `False` on every later call).
            return Ok(Value::truth(
                self.registry_mut()
                    .remove_method_wrap(&cls, &meth, idx, handle_id),
            ));
        }
        let sub_id = attributes
            .get("sub-id")
            .and_then(|v| v.as_int().map(|i| i as u64));
        if let (Some(sub_id), Some(handle_id)) = (sub_id, handle_id) {
            let mut removed = false;
            if let Some(chain) =
                crate::runtime::cow_table_mut(&mut self.dispatch.wrap_chains).get_mut(&sub_id)
            {
                let before = chain.len();
                chain.retain(|(hid, _)| *hid != handle_id);
                removed = chain.len() != before;
                if chain.is_empty() {
                    self.cleanup_wrap_name_entries(sub_id);
                }
            }
            // Invalidate light-call caches so the sub re-resolves (see wrap).
            self.invalidate_fn_resolution();
            // A second `.restore()` on an already-restored handle is a
            // no-op that answers `False`, not `True` again (raku).
            return Ok(Value::truth(removed));
        }
        Err(RuntimeError::new(
            "Invalid WrapHandle: missing sub-id or handle-id",
        ))
    
    }
}
