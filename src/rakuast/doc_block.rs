//! `RakuAST::Doc::Block` (ADR-10723 Stage 4, RakuDoc as `RakuAST::Doc`): the
//! parts of the class that are not a plain stored field.
//!
//! The block keeps its configuration as a `config` list of `key => node`
//! pairs, which is how rakudo's `.raku` prints it (`config => ( :a(...), )`);
//! `.config` answers the `Map` over them. `level` is stored as the integer
//! given and answered as the string rakudo answers (`""` for none), and
//! `delimited` is what is left when the block is not a directive, an
//! abbreviated block or an extended (`for`) one.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The `config` pairs a constructor's `config` argument (a `Map`/`Hash`)
/// stands for, in key order.
// Cost: O(k log k), k = keys.
pub(super) fn config_pairs(config: &Value) -> Option<Vec<Value>> {
    let ValueView::Hash(hash) = config.view() else {
        return None;
    };
    let mut pairs: Vec<(String, Value)> = hash
        .map
        .iter()
        .map(|(key, value)| (key.to_string(), value.clone()))
        .collect();
    pairs.sort_by(|a, b| a.0.cmp(&b.0));
    Some(
        pairs
            .into_iter()
            .map(|(key, value)| Value::pair(key, value))
            .collect(),
    )
}

/// What `node`, a `Doc::Block`, answers for `method` when it is not a field
/// read as stored.
// Cost: O(k), k = config pairs.
pub(super) fn accessor(node: &RakuAstNode, method: &str) -> Option<Value> {
    if node.class != RakuAstClass::DocBlock {
        return None;
    }
    let field = |name: &str| node.fields.iter().find(|f| f.name == Some(name));
    match method {
        "level" => Some(Value::str(match field("level") {
            Some(f) => match &f.value {
                RakuAstFieldValue::Node(v) => v.to_string_value(),
                _ => String::new(),
            },
            None => String::new(),
        })),
        "delimited" => Some(Value::truth(
            !["directive", "abbreviated", "for"]
                .iter()
                .any(|flag| field(flag).is_some()),
        )),
        "config" => {
            let mut map = crate::value::ValueMap::default();
            if let Some(RakuAstFieldValue::List(pairs)) = field("config").map(|f| &f.value) {
                for pair in pairs {
                    if let ValueView::Pair(key, value) = pair.view() {
                        map.insert(key.clone(), value.clone());
                    }
                }
            }
            let mut data = crate::value::HashData::new(map);
            data.declared_type = Some("Map".into());
            Some(Value::hash_bare_values(data))
        }
        _ => None,
    }
}
