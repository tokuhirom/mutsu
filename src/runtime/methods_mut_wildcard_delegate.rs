use super::*;

impl Interpreter {
    /// The `handles *` delegate (attribute key and current value) that answers
    /// for `method` on an instance of `class_name`, for an lvalue assignment
    /// `$obj.method = v` where the class itself has no such method or accessor.
    /// Regex (`handles /re/`) and method-based (`method inner() handles *`)
    /// delegations are not considered.
    // Cost: O(w * m), w = wildcard-delegating attributes in the MRO, m = cost of one `can` probe.
    pub(super) fn wildcard_delegate_for_assign(
        &mut self,
        class_name: &str,
        target: &Value,
        method: &str,
    ) -> Option<(String, Value)> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        for attr_var in self.collect_wildcard_handles(class_name) {
            if attr_var.contains(":regex:") || attr_var.starts_with('&') {
                continue;
            }
            let attr_key = attr_var.trim_start_matches('!').trim_start_matches('.');
            let Some(delegate) = attributes.as_map().get(attr_key).cloned() else {
                continue;
            };
            if delegate != Value::NIL && self.value_can_method(&delegate, method) {
                return Some((attr_key.to_string(), delegate));
            }
        }
        None
    }
}
