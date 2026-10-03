//! Assigning to a routine result that is a mutable QuantHash container.
//!
//! `method !values is rw { %!values }` over `has %!values is BagHash` hands
//! back the attribute's `BagHash` itself, so `$obj!values = |%other` is a
//! STORE into that container (CRDT's `G-Counter.copy`), just as `%h = ...` on
//! a `my %h is BagHash` is. An immutable `Set`/`Bag`/`Mix` stays a value and
//! is refused by the caller.

use super::*;

impl Interpreter {
    /// Store `value` into `result` when it is a `SetHash`/`BagHash`/`MixHash`:
    /// coerce `value` the way the container's own type coerces an assignment,
    /// then write it into the existing node so every holder sees it. `None`
    /// when `result` is no mutable QuantHash.
    // Cost: O(n), n = elements of `value` (coercion plus one copy).
    pub(crate) fn store_into_quanthash_lvalue(
        &mut self,
        result: &Value,
        value: Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let coercer = match result.view() {
            ValueView::Set(_, true) => "SetHash",
            ValueView::Bag(_, true) => "BagHash",
            ValueView::Mix(_, true) => "MixHash",
            _ => return None,
        };
        let coerced = match self.try_compiled_method_or_interpret(value, coercer, Vec::new()) {
            Ok(coerced) => coerced,
            Err(err) => return Some(Err(err)),
        };
        // `None` here means `coerced` already is this node (`%!v = %!v`).
        Some(Ok(result
            .store_quanthash_in_place(&coerced)
            .unwrap_or_else(|| result.clone())))
    }
}
