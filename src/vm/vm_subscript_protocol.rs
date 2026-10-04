//! Element assignment through an object's own subscript protocol.

use super::*;

impl Interpreter {
    /// `target[idx] = val` (or `target{idx} = val`) where `target` is an
    /// object that implements `ASSIGN-POS` / `ASSIGN-KEY` itself -- a class
    /// that declares it, or a role mixed into the object supplies it. Calls
    /// that method and answers `Some(result)`; `None` for any other target,
    /// leaving the caller's built-in container paths in place. A role that
    /// only `handles` the method forwards it to an attribute, and the mixin
    /// delegation path owns that write, so it is declined too.
    // Cost: O(1) to decline a non-object target; otherwise one method
    // resolution plus the call.
    pub(crate) fn assign_through_subscript_protocol(
        &mut self,
        target: &Value,
        idx: &Value,
        val: &Value,
        is_positional: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let method = if is_positional {
            "ASSIGN-POS"
        } else {
            "ASSIGN-KEY"
        };
        let object = target.deref_container();
        let object = object.descalarize();
        let dispatches = match object.view() {
            ValueView::Mixin(_, mixins) => {
                self.mixin_composes_method(object, method)
                    && self.delegated_mixin_attr_key(mixins, method).is_none()
            }
            ValueView::Instance { class_name, .. } => {
                self.has_user_method(class_name.as_str(), method)
            }
            _ => false,
        };
        if !dispatches {
            return None;
        }
        let idx_arg = match idx.view() {
            ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
            _ => idx.clone(),
        };
        Some(self.call_method_with_values(object.clone(), method, vec![idx_arg, val.clone()]))
    }
}
