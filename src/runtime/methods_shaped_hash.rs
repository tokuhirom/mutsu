//! The subscripts of a shaped hash (#19283; see `value::shaped_hash` for how
//! one is kept): `%h{"a";1}` reads, writes, tests and deletes the entry the
//! key list `("a", 1)` names, as rakudo 2026.11's `Hash::Shaped` does.

use super::*;
use crate::value::shaped_hash::{shaped_hash_dims, storage_key};

impl Interpreter {
    /// The keys of a shaped hash subscript given as one value: a list or a
    /// Seq gives one key per dimension, anything else is a single key.
    // Cost: O(k), k = keys.
    pub(crate) fn shaped_hash_parts(key: &Value) -> Vec<Value> {
        let key = key.deref_container();
        let key = match key.view() {
            ValueView::Scalar(inner) => inner.deref_container(),
            _ => key,
        };
        match key.view() {
            ValueView::Array(items, _) => items.to_vec(),
            ValueView::Seq(_) => value_to_list(&key),
            _ => vec![key],
        }
    }

    /// The storage key and key list of the keys `parts` of the shaped hash of
    /// dimension types `dims`: one key per dimension, each of its type.
    /// `operation` names the subscript in rakudo's dimension errors
    /// (`assign to`, `access`).
    // Cost: O(d), d = dimensions (plus each key's type check).
    pub(crate) fn shaped_hash_key(
        &mut self,
        dims: &[String],
        parts: &[Value],
        operation: &str,
    ) -> Result<(String, Value), RuntimeError> {
        if parts.len() != dims.len() {
            return Err(dimensions_error(operation, parts.len(), dims.len()));
        }
        let mut keys = Vec::with_capacity(parts.len());
        for (i, (part, ty)) in parts.iter().zip(dims).enumerate() {
            let part = part.deref_container();
            if !self.type_matches_value(ty, &part) {
                let repr = self.type_check_got_repr(&part);
                return Err(key_type_error(i + 1, ty, &part, &repr));
            }
            keys.push(part);
        }
        let key = storage_key(&keys);
        Ok((key, Value::array_with_kind(crate::gc::Gc::new(crate::value::ArrayData::new(keys)), crate::value::ArrayKind::List)))
    }

    /// A single-key subscript (`%h<a>`, `%h{$keys}`) of a shaped hash whose
    /// key spec is `spec` (`Str;Int`): the key list it names, with rakudo's
    /// dimension and key type errors.
    // Cost: O(d), d = dimensions.
    pub(crate) fn shaped_hash_single_key(
        &mut self,
        spec: &str,
        key: &Value,
        operation: &str,
    ) -> Result<Value, RuntimeError> {
        let dims: Vec<String> = spec.split(';').map(|d| d.trim().to_string()).collect();
        let parts = Self::shaped_hash_parts(key);
        Ok(self.shaped_hash_key(&dims, &parts, operation)?.1)
    }

    /// `%h{a;b}` on the shaped hash `target`: the value the key list names, or
    /// the value type object when there is no such entry.
    // Cost: O(d), d = dimensions.
    pub(crate) fn shaped_hash_read(
        &mut self,
        target: &Value,
        parts: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let target = target.deref_container();
        let Some(dims) = shaped_hash_dims(&target) else {
            return Ok(None);
        };
        let (key, _) = self.shaped_hash_key(&dims, parts, "access")?;
        let ValueView::Hash(hash) = target.view() else {
            return Ok(None);
        };
        Ok(Some(match hash.map.get(&key) {
            Some(value) => value.deref_container(),
            None => Value::package(Symbol::intern(
                hash.value_type.as_deref().unwrap_or("Any"),
            )),
        }))
    }

    /// `%h{a;b} = value` on the shaped hash `target`. `false` when `target`
    /// is not a shaped hash.
    // Cost: O(d), d = dimensions (plus the value's type check).
    pub(crate) fn shaped_hash_store(
        &mut self,
        target: &mut Value,
        parts: &[Value],
        value: Value,
    ) -> Result<bool, RuntimeError> {
        let Some(dims) = target.with_deref(shaped_hash_dims) else {
            return Ok(false);
        };
        let (key, list) = self.shaped_hash_key(&dims, parts, "assign to")?;
        let value_type = target.with_deref(|v| match v.view() {
            ValueView::Hash(hash) => hash.value_type.clone(),
            _ => None,
        });
        if let Some(of) = value_type
            && !value.is_nil()
            && !self.type_matches_value(&of, &value)
        {
            return Err(self.type_check_assignment_failure("", &of, &value));
        }
        let value = Self::itemize_value_for_element_store(value);
        let mut node = target.deref_container();
        node.with_hash_mut(|hash| {
            let data = crate::value::gc_data_mut(hash);
            Value::hash_insert_through(&mut data.map, key.clone(), value);
            data.original_keys
                .get_or_insert_with(ValueMap::default)
                .insert(key, list);
        });
        Ok(true)
    }

    /// The entries of a hash assigned to a shaped hash variable
    /// (`my %h{Str;Int} = ("a", 1) => 5`), filed under their key lists with
    /// each key and value checked; `original` holds the key objects the
    /// source hash recorded.
    // Cost: O(n·d), n = entries, d = dimensions.
    pub(crate) fn coerce_shaped_hash_entries(
        &mut self,
        source: &crate::value::HashData,
        original: Option<&ValueMap>,
        dims: &[String],
        value_type: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        let mut map = crate::value::user_key_map::with_capacity(source.map.len());
        let mut keys = ValueMap::default();
        for (key, value) in source.map.iter() {
            let key_object = original
                .and_then(|orig| orig.get(key))
                .cloned()
                .unwrap_or_else(|| Value::hash_key_decode(key));
            let parts = Self::shaped_hash_parts(&key_object);
            let (stored, list) = self.shaped_hash_key(dims, &parts, "assign to")?;
            let value = value.deref_container();
            if let Some(of) = value_type
                && !value.is_nil()
                && !self.type_matches_value(of, &value)
            {
                return Err(self.type_check_assignment_failure("", of, &value));
            }
            map.insert(stored.clone(), Self::itemize_value_for_element_store(value));
            keys.insert(stored, list);
        }
        Ok(crate::runtime::utils::set_hash_original_keys(Value::hash(map), keys))
    }

    /// `%h{a;b}:exists` / `:delete` on the shaped hash `target`: whether the
    /// entry exists, and with `delete` its removed value. `None` when
    /// `target` is not a shaped hash.
    // Cost: O(d), d = dimensions.
    pub(crate) fn shaped_hash_exists_or_delete(
        &mut self,
        target: &mut Value,
        parts: &[Value],
        delete: bool,
    ) -> Result<Option<(bool, Value)>, RuntimeError> {
        let Some(dims) = target.with_deref(shaped_hash_dims) else {
            return Ok(None);
        };
        let operation = if delete { "delete from" } else { "access" };
        let (key, _) = self.shaped_hash_key(&dims, parts, operation)?;
        let mut node = target.deref_container();
        let mut found = (false, Value::NIL);
        node.with_hash_mut(|hash| {
            if !delete {
                found = (hash.map.contains_key(&key), Value::NIL);
                return;
            }
            let data = crate::value::gc_data_mut(hash);
            if let Some(old) = data.map.remove(&key) {
                if let Some(keys) = data.original_keys.as_mut() {
                    keys.remove(&key);
                }
                found = (true, old.deref_container());
            }
        });
        Ok(Some(found))
    }
}

/// rakudo's `X::NotEnoughDimensions` / `X::TooManyDimensions` for a shaped
/// hash subscript of `got` keys where `needed` are declared.
// Cost: O(1).
fn dimensions_error(operation: &str, got: usize, needed: usize) -> RuntimeError {
    let (class, message) = if got < needed {
        (
            "X::NotEnoughDimensions",
            format!("Cannot {operation} a {needed} dimension hash with only {got} dimensions"),
        )
    } else {
        (
            "X::TooManyDimensions",
            format!("Cannot {operation} a {needed} dimension hash with {got} dimensions"),
        )
    };
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("operation".to_string(), Value::str(operation.to_string()));
    attrs.insert("aggregate".to_string(), Value::str_from("hash"));
    attrs.insert("got-dimensions".to_string(), Value::int(got as i64));
    attrs.insert("needed-dimensions".to_string(), Value::int(needed as i64));
    attrs.insert("message".to_string(), Value::str(message.clone()));
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(Symbol::intern(class), attrs)));
    err
}

/// rakudo's `X::TypeCheck::Binding` for a key of the wrong type in dimension
/// `dim` (1-based).
// Cost: O(1).
fn key_type_error(dim: usize, expected: &str, got: &Value, repr: &str) -> RuntimeError {
    let got_type = crate::runtime::utils::got_type_name(got);
    let message = format!(
        "Type check failed in binding to 'key of dimension {dim}'; expected {expected} but got {got_type}{}",
        if repr.is_empty() { String::new() } else { format!(" {repr}") }
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("symbol".to_string(), Value::str(format!("key of dimension {dim}")));
    attrs.insert("got".to_string(), got.clone());
    attrs.insert("expected".to_string(), crate::value::expected_type_object(expected));
    attrs.insert("message".to_string(), Value::str(message.clone()));
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::TypeCheck::Binding"),
        attrs,
    )));
    err
}
