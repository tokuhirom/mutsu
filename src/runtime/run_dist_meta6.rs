//! META6.json decoding for the distributions `$?DISTRIBUTION` resolves to
//! (`run_dist.rs`).

use super::*;
use crate::value::ValueMap;

impl Interpreter {
    pub(super) fn parse_meta6_json(content: &str) -> Option<ValueMap> {
        let json: serde_json::Value = serde_json::from_str(content).ok()?;
        let obj = json.as_object()?;
        let mut meta = ValueMap::default();
        for (key, val) in obj {
            meta.insert(key.clone(), Self::json_to_value(val));
        }
        Some(meta)
    }

    pub(super) fn json_to_value(val: &serde_json::Value) -> Value {
        use serde_json::Value as Json;
        match val {
            Json::Null => Value::NIL,
            Json::Bool(b) => Value::truth(*b),
            Json::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Value::int(i)
                } else if let Some(f) = n.as_f64() {
                    Value::num(f)
                } else {
                    Value::str(n.to_string())
                }
            }
            Json::String(s) => Value::str(s.clone()),
            Json::Array(arr) => Value::array(arr.iter().map(Self::json_to_value).collect()),
            Json::Object(obj) => {
                let mut map = ValueMap::default();
                for (k, v) in obj {
                    map.insert(k.clone(), Self::json_to_value(v));
                }
                Value::hash_with_data(Value::hash_arc(map))
            }
        }
    }

    /// Fill in the identity keys the way Rakudo's
    /// `CompUnit::Repository::Distribution` does for a META6.json-backed
    /// distribution: `ver //= version // ''`, `auth //= authority // author // ''`
    /// and `api //= ''`, so `$?DISTRIBUTION.meta<ver>` is defined for a META6
    /// that only spells `version`.
    // Cost: O(1).
    pub(super) fn normalize_meta_identity(meta: &mut ValueMap) {
        let defined = |meta: &ValueMap, key: &str| meta.get(key).filter(|v| !v.is_nil()).cloned();
        let pick = |meta: &ValueMap, keys: &[&str]| {
            keys.iter()
                .find_map(|k| defined(meta, k))
                .unwrap_or_else(|| Value::str(String::new()))
        };
        let ver = pick(meta, &["ver", "version"]);
        let auth = pick(meta, &["auth", "authority", "author"]);
        let api = pick(meta, &["api"]);
        meta.insert("ver".to_string(), ver);
        meta.insert("auth".to_string(), auth);
        meta.insert("api".to_string(), api);
    }
}
