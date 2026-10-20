//! Key constraints are checked before any object-hash store changes its map.

use super::*;

impl Interpreter {
    /// Apply the key parameter's constraint from the hash itself.
    // Cost: O(T + k), T = matching/coercion cost, k = constraint spelling length.
    pub(crate) fn prepare_hash_key_arg(
        &mut self,
        hash: &Value,
        key: Value,
    ) -> Result<Value, RuntimeError> {
        let hash = hash.deref_container();
        match self.hash_key_type(hash.descalarize()) {
            Some(constraint) => self.prepare_object_hash_key(&constraint, key),
            None => Ok(key),
        }
    }

    // Cost: O(k + T), k = constraint spelling length, T = matching/coercion cost.
    pub(crate) fn prepare_object_hash_key(
        &mut self,
        constraint: &str,
        key: Value,
    ) -> Result<Value, RuntimeError> {
        if constraint == "Mu" {
            return Ok(key);
        }
        let key = self.coerce_key_for_constraint(constraint, key)?;
        if !self.type_matches_value(constraint, &key) {
            return Err(self.typecheck_binding_parameter_failure("key", constraint, &key));
        }
        Ok(key)
    }

    /// Shared coercion for object-hash and QuantHash key stores. QuantHash
    /// keeps its own validation because numeric allomorphs satisfy its keys.
    // Cost: O(k + T), k = constraint spelling length, T = matching/coercion cost.
    pub(crate) fn coerce_key_for_constraint(
        &mut self,
        constraint: &str,
        key: Value,
    ) -> Result<Value, RuntimeError> {
        // Parentheses in an inline subset belong to its predicate, not to a
        // coercion type. The ordinary matcher resolves and caches the subset.
        if constraint.trim_start().starts_with("subset ::") {
            return Ok(key);
        }
        if let Some((target, _)) = parse_coercion_type(constraint)
            && !self.type_matches_value(target, &key)
        {
            let bare = strip_type_smiley(target).0;
            if matches!(bare, "Int" | "UInt" | "Num" | "Rat" | "FatRat" | "Real" | "Numeric" | "Complex") {
                crate::runtime::utils::check_str_numeric(&key)?;
            }
            return self.try_coerce_value_for_constraint(constraint, key);
        }
        Ok(key)
    }
}
