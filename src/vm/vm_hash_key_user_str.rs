//! A user object used as a key of a `Str`-keyed hash (#13293).
//!
//! `%h{$obj}` coerces the key with `.Str`. The subscript machinery reads keys
//! with `to_string_value()`, which never runs user code, so an object whose
//! class defines `method Str` was keyed by its default `Class<id>` rendering.

use super::*;

impl Interpreter {
    /// A named hash uses its declaration; a scalar alias uses embedded metadata.
    // Cost: O(d), d = declaration record lookup depth; O(1) for a scalar alias.
    pub(super) fn object_hash_key_type_for_place(
        &mut self,
        name: &str,
        name_sym: Symbol,
        container: Option<&Value>,
    ) -> Option<String> {
        if name.starts_with('%') {
            loan_env!(self, var_hash_key_constraint_sym(name, name_sym))
        } else {
            container.map(Value::deref_container).and_then(|value| {
                match value.view() {
                    ValueView::Hash(hash) => hash.key_type.clone(),
                    _ => None,
                }
            })
        }
    }

    /// Normalize an associative index, then validate its object-hash keys.
    // Cost: O(n * T + S), n = keys, T = key type check, S = user Str call cost.
    pub(crate) fn prepare_hash_subscript_index(
        &mut self,
        target: &Value,
        index: Value,
    ) -> Result<Value, RuntimeError> {
        let index = self.hash_key_through_user_str(target, index)?;
        self.validate_object_hash_index(target, &index)?;
        Ok(index)
    }

    /// The stored form of `idx` for deleting from `target` when `target` is an
    /// object hash (`Hash[V, K]`, `my %h{K}`): its entries are keyed by the
    /// key's WHICH or encoded form, not by its string value, so a delete
    /// through a `$` holding it (no variable metadata, unlike the named op)
    /// must look it up the same way.
    // Cost: O(k) for the key's WHICH/encoding, k = size of the key.
    pub(super) fn object_hash_delete_key(&mut self, target: &Value, idx: Value) -> Value {
        let ValueView::Hash(map) = target.view() else {
            return idx;
        };
        if map.key_type.is_none()
            || matches!(
                idx.view(),
                ValueView::Whatever | ValueView::Nil | ValueView::Array(..) | ValueView::Package(_)
            )
        {
            return idx;
        }
        let which = self.which_key(&idx);
        if map.contains_key(&which) {
            Value::str(which)
        } else {
            Value::str(Value::hash_key_encode(&idx))
        }
    }

    /// Validate scalar or slice keys without inspecting unrelated hash values.
    // Cost: O(n * T), n = subscript keys, T = the key constraint's type check.
    pub(crate) fn validate_object_hash_index(
        &mut self,
        target: &Value,
        index: &Value,
    ) -> Result<(), RuntimeError> {
        let target = target.deref_container();
        let Some(constraint) = self.hash_key_type(target.descalarize()) else {
            return Ok(());
        };
        if matches!(constraint.as_str(), "Any" | "Mu") {
            return Ok(());
        }
        if index.is_range() {
            return Ok(());
        }
        let check = |interp: &mut Self, key: &Value| {
            if interp.type_matches_value(&constraint, key) {
                Ok(())
            } else {
                Err(interp.typecheck_binding_parameter_failure("key", &constraint, key))
            }
        };
        match index.view() {
            ValueView::Array(keys, kind) if !kind.is_itemized() => {
                for key in keys.iter() {
                    check(self, key)?;
                }
                Ok(())
            }
            ValueView::Whatever | ValueView::Nil | ValueView::Sub(..) => Ok(()),
            _ => check(self, index),
        }
    }

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
