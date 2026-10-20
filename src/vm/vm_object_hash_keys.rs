//! Key constraints and key identity shared by associative subscript opcodes.

use super::*;

impl Interpreter {
    /// A named hash uses its declaration; a scalar alias uses embedded metadata.
    // Cost: O(d + k), d = record lookup depth, k = constraint spelling length.
    pub(super) fn object_hash_key_type_for_place(
        &mut self,
        name: &str,
        name_sym: Symbol,
        container: Option<&Value>,
    ) -> Option<String> {
        if name.starts_with('%') {
            loan_env!(self, var_hash_key_constraint_sym(name, name_sym))
        } else {
            container
                .map(Value::deref_container)
                .and_then(|value| match value.view() {
                    ValueView::Hash(hash) => hash.key_type.clone(),
                    _ => None,
                })
        }
    }

    /// Normalize an associative index, then validate its object-hash keys.
    // Cost: O(k + n * T + S), k = constraint length, n = keys,
    // T = key constraint/diagnostic cost, S = user Str call cost.
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
    // Cost: O(k + n * T), k = constraint length, n = subscript keys,
    // T = key constraint/diagnostic cost.
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
}
