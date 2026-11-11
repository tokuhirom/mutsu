use super::*;

impl Interpreter {
    /// `Pkg.WHO<name> = $v` on a symbol the stash holds *bound* rather than in
    /// a container. A `Proxy` bound there (`Pkg.WHO<x> := Proxy.new(...)`) is
    /// assigned through its STORE, which may refuse the value; a type object
    /// bound there (`Pkg.WHO<x> := SomeClass`, a nested package) is not a
    /// container at all, so the assignment dies as rakudo's does ("Cannot
    /// modify an immutable ..."). The generic element store used to replace
    /// the Proxy and ignore the type object, so Red's read-only
    /// `$model.WHO<ResultSeq>` accepted any assignment.
    ///
    /// `Some(result)` when the store was handled here; `None` leaves every
    /// other entry (a cell, an aggregate, a missing key) to the generic store.
    // Cost: O(1) expected: one stash probe, plus the STORE call for a Proxy.
    pub(super) fn try_stash_bound_entry_store(
        &mut self,
        target: &Value,
        idx: &Value,
        value: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        // A `:=` rebinds the symbol, whatever it held.
        if Self::is_bind_index_value(value) {
            return Ok(None);
        }
        let ValueView::Str(key) = idx.view() else {
            return Ok(None);
        };
        let target = target.descalarize();
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return Ok(None);
        };
        if !crate::value::types::is_stash_class_name(class_name.as_str()) {
            return Ok(None);
        }
        let entry = {
            let map = attributes.as_map();
            let Some(ValueView::Hash(symbols)) = map.get("symbols").map(Value::view) else {
                return Ok(None);
            };
            match symbols.get(key.as_str()) {
                Some(entry) => entry.clone(),
                None => return Ok(None),
            }
        };
        if entry.is_proxy_value() {
            self.store_proxy_lvalue(&entry, value.clone())?;
            return Ok(Some(value.clone()));
        }
        if let ValueView::Package(name) = entry.view() {
            let name = name.resolve();
            return Err(RuntimeError::new(format!(
                "Cannot modify an immutable {name} ({name})"
            )));
        }
        Ok(None)
    }
}
