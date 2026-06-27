#![allow(clippy::result_large_err)]

use crate::value::{ArrayKind, Value};
use std::sync::Arc;

fn flatten_with_depth(
    value: &Value,
    depth: Option<usize>,
    out: &mut Vec<Value>,
    flatten_arrays: bool,
) {
    if let Some(0) = depth {
        out.push(value.clone());
        return;
    }
    match value {
        Value::Array(items, kind) if *kind == ArrayKind::List || flatten_arrays => {
            let next_depth = depth.map(|d| d.saturating_sub(1));
            for item in items.iter() {
                flatten_with_depth(item, next_depth, out, flatten_arrays);
            }
        }
        Value::Range(..)
        | Value::RangeExcl(..)
        | Value::RangeExclStart(..)
        | Value::RangeExclBoth(..)
        | Value::GenericRange { .. } => out.extend(crate::runtime::utils::value_to_list(value)),
        other => out.push(other.clone()),
    }
}

pub(crate) fn parse_flat_depth(arg: &Value) -> Option<usize> {
    match arg {
        Value::Int(n) => Some((*n).max(0) as usize),
        _ => None,
    }
}

pub(crate) fn is_hammer_pair(arg: &Value) -> bool {
    matches!(arg, Value::Pair(key, val) if key == "hammer" && val.truthy())
}

pub(crate) fn flatten_target(target: &Value, depth: Option<usize>, flatten_arrays: bool) -> Value {
    let mut flat = Vec::new();
    if let Some(items) = target.as_list_items() {
        for item in items.iter() {
            flatten_with_depth(item, depth, &mut flat, flatten_arrays);
        }
    } else {
        flatten_with_depth(target, depth, &mut flat, flatten_arrays);
    }
    Value::Seq(Arc::new(flat))
}
