//! Shaped hashes (#19283): an object hash declared with a key type per
//! dimension, `my %h{Str;Int}`, as rakudo 2026.11's `Hash::Shaped` makes it.
//!
//! Such a hash is keyed by a `List` holding one key of each dimension's type.
//! It is an object hash whose `key_type` is `List`, and whose
//! `declared_type` is `Hash[V,(T1,T2,...)]`; that spelling is where the
//! dimension types are kept. Each entry is stored under a string joined from
//! the `.WHICH` of each of its keys (rakudo's `shaped-key`), and the key list
//! itself is the entry's original key, which `.keys` and `.pairs` report.

use super::{Value, ValueView};

/// The dimension types of a shaped hash (`["Str", "Int"]`), `None` for any
/// other value.
// Cost: O(n), n = length of the declared type name.
pub(crate) fn shaped_hash_dims(value: &Value) -> Option<Vec<String>> {
    let ValueView::Hash(hash) = value.view() else {
        return None;
    };
    dims_of_declared_type(hash.declared_type.as_deref()?)
}

/// The dimension types a declared type `Hash[V,(T1,T2)]` names.
// Cost: O(n), n = length of `declared`.
pub(crate) fn dims_of_declared_type(declared: &str) -> Option<Vec<String>> {
    let inner = declared.strip_prefix("Hash[")?.strip_suffix(")]")?;
    let at = inner.rfind(",(")?;
    let dims: Vec<String> = inner[at + 2..]
        .split(',')
        .map(|d| d.trim().to_string())
        .collect();
    (dims.len() >= 2).then_some(dims)
}

/// The declared type of a shaped hash of value type `value_type` and the
/// dimensions spelled `spec` (`Str;Int`), or `None` when `spec` has one
/// dimension only.
// Cost: O(n), n = length of `spec`.
pub(crate) fn declared_type_for(value_type: &str, spec: &str) -> Option<String> {
    let dims: Vec<&str> = spec.split(';').map(str::trim).collect();
    (dims.len() >= 2).then(|| format!("Hash[{value_type},({})]", dims.join(",")))
}

/// The storage key of the key list `parts`: each key's `.WHICH`, prefixed
/// with its length so no two lists join to the same string.
// Cost: O(k), k = total length of the parts' WHICH strings.
pub(crate) fn storage_key(parts: &[Value]) -> String {
    let mut key = String::new();
    for part in parts {
        let which = super::which_key::value_which_key(part);
        key.push_str(&which.chars().count().to_string());
        key.push(':');
        key.push_str(&which);
    }
    key
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dims_round_trip_through_the_declared_type() {
        let declared = declared_type_for("Any", "Str;Int").unwrap();
        assert_eq!(declared, "Hash[Any,(Str,Int)]");
        assert_eq!(dims_of_declared_type(&declared).unwrap(), ["Str", "Int"]);
        assert!(declared_type_for("Any", "Str").is_none());
        assert!(dims_of_declared_type("Hash[Int,Str]").is_none());
    }

    #[test]
    fn storage_keys_do_not_collide() {
        let a = storage_key(&[Value::str_from("a|b"), Value::str_from("c")]);
        let b = storage_key(&[Value::str_from("a"), Value::str_from("b|c")]);
        assert_ne!(a, b);
    }
}
