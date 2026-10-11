//! Key constraints are checked before any object-hash store changes its map.

use super::*;

impl Interpreter {
    // Cost: O(k + T), k = constraint spelling length, T = matching/coercion cost.
    pub(crate) fn prepare_object_hash_key(
        &mut self,
        constraint: &str,
        key: Value,
        variable: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        if matches!(constraint, "Any" | "Mu") {
            return Ok(key);
        }
        let key = self.coerce_key_for_constraint(constraint, key)?;
        if !self.type_matches_value(constraint, &key) {
            return Err(match variable {
                Some(name) => self.type_check_element_failure(name, constraint, &key),
                None => self.type_check_binding_failure(constraint, &key),
            });
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
