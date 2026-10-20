//! A user object used as a key of a `Str`-keyed hash (#13293).
//!
//! `%h{$obj}` coerces the key with `.Str`. The subscript machinery reads keys
//! with `to_string_value()`, which never runs user code, so an object whose
//! class defines `method Str` was keyed by its default `Class<id>` rendering.

use super::*;

impl Interpreter {
    /// `index` with a user object key replaced by its `.Str`, when `target` is
    /// a plain (not object) `Hash` and the key's class defines its own `Str`.
    // Cost: O(1) for a non-instance key; for an instance one memoized
    // `has_user_method` probe plus the user `Str` call.
    pub(crate) fn hash_key_through_user_str(
        &mut self,
        target: &Value,
        index: Value,
    ) -> Result<Value, RuntimeError> {
        let ValueView::Instance { class_name, .. } = index.view() else {
            return Ok(index);
        };
        let class = class_name.resolve();
        let plain_hash = {
            let probe = target.deref_container();
            matches!(probe.descalarize().view(), ValueView::Hash(arc) if arc.key_type.is_none() && !arc.has_typed_keys())
        };
        if !plain_hash || !self.has_user_method(&class, "Str") {
            return Ok(index);
        }
        self.call_method_with_values(index, "Str", vec![])
    }

    /// The key argument of an associative method (`AT-KEY`, ...) on a
    /// plain hash, coerced as [`Self::hash_key_through_user_str`] does.
    // Cost: O(1) unless the single argument is an instance, then as above.
    pub(crate) fn coerce_assoc_key_arg(
        &mut self,
        target: &Value,
        method: &str,
        args: &mut [Value],
    ) -> Result<(), RuntimeError> {
        let key_args = match method {
            "AT-KEY" | "EXISTS-KEY" | "DELETE-KEY" => 1,
            "ASSIGN-KEY" | "BIND-KEY" => 2,
            _ => return Ok(()),
        };
        if args.len() == key_args && matches!(args[0].view(), ValueView::Instance { .. }) {
            let key = std::mem::replace(&mut args[0], Value::NIL);
            args[0] = self.hash_key_through_user_str(target, key)?;
        }
        Ok(())
    }
}
