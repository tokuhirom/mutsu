//! `Any`'s index-view methods on an object that supplies its own `list`.
//!
//! Rakudo defines `Any.kv`/`.pairs`/`.antipairs`/`.keys`/`.values` on a
//! defined invocant as `self.list.<method>`, so a class overriding `list`
//! answers them from that list rather than as one opaque item. `ValueList`
//! (`method list() { self.List }`) is the ecosystem case: `Tuple.kv` yields
//! the tuple's own index/value pairs.

use super::*;

impl Interpreter {
    /// `self.list.<method>` for an instance whose class defines `list` but
    /// not `method` itself; `None` when the default does not apply.
    // Cost: O(1) to decline (a memoized method probe); otherwise the user
    // `list` call plus O(n), n = elements of the list it returns.
    // TODO: compile to bytecode -- this reaches the user `list` through
    // `call_method_with_values`, like the neighbouring `iterator` routing of
    // `.map`/`.grep`; a compiled `Any` default would dispatch it directly.
    pub(crate) fn try_any_list_view_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        if !args.is_empty() || !matches!(method, "kv" | "pairs" | "antipairs" | "keys" | "values") {
            return Ok(None);
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return Ok(None);
        };
        let cn = class_name.resolve();
        if !self.has_user_method(&cn, "list") || self.has_user_method(&cn, method) {
            return Ok(None);
        }
        let list = self.call_method_with_values(target.clone(), "list", Vec::new())?;
        // A `list` that hands the instance back unchanged would recurse.
        if matches!(list.view(), ValueView::Instance { .. }) {
            return Ok(None);
        }
        self.call_method_with_values(list, method, Vec::new())
            .map(Some)
    }
}
