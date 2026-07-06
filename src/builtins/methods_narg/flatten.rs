use crate::value::{ArrayKind, Value, ValueView};

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
    match value.view() {
        ValueView::Array(items, kind) if kind == ArrayKind::List || flatten_arrays => {
            let next_depth = depth.map(|d| d.saturating_sub(1));
            for item in items.iter() {
                flatten_with_depth(item, next_depth, out, flatten_arrays);
            }
        }
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => out.extend(crate::runtime::utils::value_to_list(value)),
        _ => out.push(value.clone()),
    }
}

pub(crate) fn parse_flat_depth(arg: &Value) -> Option<usize> {
    match arg.view() {
        ValueView::Int(n) => Some(n.max(0) as usize),
        _ => None,
    }
}

pub(crate) fn is_hammer_pair(arg: &Value) -> bool {
    matches!(arg.view(), ValueView::Pair(key, val) if key == "hammer" && val.truthy())
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
    Value::seq(flat)
}
