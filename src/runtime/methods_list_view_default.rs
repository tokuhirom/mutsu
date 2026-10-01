//! `Any`'s list-derived methods on an object that supplies its own `list`.
//!
//! Rakudo defines `Any.kv`/`.pairs`/`.antipairs`/`.keys`/`.values` and
//! `.elems`/`.Slip`/`.flat`/`.Seq`/`.Array`/`.List`/`.hash` on a
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
        if !args.is_empty()
            || !matches!(
                method,
                "kv" | "pairs"
                    | "antipairs"
                    | "keys"
                    | "values"
                    | "elems"
                    | "Slip"
                    | "flat"
                    | "Seq"
                    | "Array"
                    | "List"
                    | "hash"
            )
        {
            return Ok(None);
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return Ok(None);
        };
        let cn = class_name.resolve();
        if self.has_user_method(&cn, method) {
            return Ok(None);
        }
        if !self.has_user_method(&cn, "list") {
            // `Any.hash` is `self.list.hash`; a plain user class's default
            // `list` is the one-item list of itself, so `.hash` dies with
            // the odd-number-of-elements error. Only for a class whose
            // whole MRO is user-declared: a builtin ancestor owns its `hash`.
            if method == "hash" && self.is_plain_user_class(&cn) {
                let one = Value::array(vec![target.clone()]);
                return self
                    .call_method_with_values(one, "hash", Vec::new())
                    .map(Some);
            }
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

    /// Whether every class in `cn`'s MRO (bar `Any`/`Mu`) is user-declared.
    // Cost: O(d), d = MRO depth.
    fn is_plain_user_class(&mut self, cn: &str) -> bool {
        let mro = self.class_mro(cn);
        let registry = self.registry();
        mro.iter().all(|c| {
            let name = c.resolve();
            matches!(name.as_str(), "Any" | "Mu") || registry.classes.contains_key(name.as_str())
        })
    }
}
