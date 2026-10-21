//! Shaped (multidimensional) array helpers: the shape a `:shape` array
//! carries, its leaves, and rebuilding it around new leaves. Pure operations
//! on `Value`, kept below the runtime so `Value` itself can ask whether an
//! array is shaped (issue #10779); `runtime::utils` re-exports them.

use super::{ArrayKind, Value, ValueView};

/// Check if an array is a shaped (multidimensional) array.
/// A shaped array is one explicitly created as multidimensional via `:shape`.
pub(crate) fn is_shaped_array(value: &Value) -> bool {
    if let ValueView::Array(_, kind) = value.view()
        && kind == ArrayKind::Shaped
    {
        return true;
    }
    shaped_array_shape(value).is_some()
}

/// Row breaks in a shaped array's gist follow its declared dimensions, even
/// when a one-dimensional array stores another Array as an element.
// Cost: O(d), d = dimensions of a shaped array.
pub(crate) fn shaped_array_has_rows(value: &Value) -> bool {
    shaped_array_shape(value).is_some_and(|shape| shape.len() > 1)
}

// Cost: O(d), d = dimensions, when the array carries its shape (the cached shape is
// checked along the first-child spine only); O(E), E = leaves, the first time a shape
// has to be inferred, which then caches it on the array.
pub(crate) fn shaped_array_shape(value: &Value) -> Option<Vec<usize>> {
    let ValueView::Array(items, kind) = value.view() else {
        return None;
    };
    // Only arrays explicitly created as shaped can be shaped
    if kind != ArrayKind::Shaped {
        return None;
    }

    // The shape is a fixed attribute of the container: element stores go through
    // `assign_array_multidim` / `*-POS`, which keep each level's length, so the
    // cached shape only needs a spine check (each level's length along the first
    // child) to reject a stale shape left on a restructured array. A leaf may
    // legitimately hold an (itemized) Array, so leaves are not inspected.
    fn shape_matches_spine(value: &Value, shape: &[usize]) -> bool {
        let Some((&len, rest)) = shape.split_first() else {
            return false;
        };
        let ValueView::Array(items, ..) = value.view() else {
            return false;
        };
        if items.len() != len {
            return false;
        }
        if rest.is_empty() {
            return true;
        }
        items
            .first()
            .is_some_and(|child| shape_matches_spine(child, rest))
    }

    // A freshly inferred shape has never been validated, so it is checked
    // against every level (O(E)) before it is cached.
    fn shape_matches_full_structure(value: &Value, shape: &[usize]) -> bool {
        let Some((&len, rest)) = shape.split_first() else {
            return false;
        };
        let ValueView::Array(items, ..) = value.view() else {
            return false;
        };
        if items.len() != len {
            return false;
        }
        if rest.is_empty() {
            return items
                .iter()
                .all(|v| !matches!(v.view(), ValueView::Array(..)));
        }
        items
            .iter()
            .all(|child| shape_matches_full_structure(child, rest))
    }

    if items.is_empty() {
        return None;
    }

    // Infer this array's shape from its children's embedded shapes.
    fn infer_shape_from_array(items: &[Value]) -> Option<Vec<usize>> {
        let first = items.first()?;
        let ValueView::Array(first_items, ..) = first.view() else {
            return None;
        };
        let first_shape = first_items.shape.as_ref()?;
        if !items.iter().all(|child| {
            if let ValueView::Array(child_items, ..) = child.view() {
                child_items.shape.as_ref() == Some(first_shape)
            } else {
                false
            }
        }) {
            return None;
        }
        let mut shape = Vec::with_capacity(1 + first_shape.len());
        shape.push(items.len());
        shape.extend_from_slice(first_shape);
        Some(shape)
    }

    // Prefer the shape embedded on this array's `ArrayData`, validated against
    // the current element structure (a stale cached shape after restructuring
    // is rejected and re-inferred).
    if let Some(cached_shape) = &items.shape
        && shape_matches_spine(value, cached_shape)
    {
        return Some(cached_shape.to_vec());
    }
    // A flat (1-dim) shaped array's shape is unambiguously `[len]`. Recover it
    // even when the cached `ArrayData.shape` was dropped crossing a store
    // boundary (the dual-store sync does not always carry it), so a re-assignment
    // (`@arr = ...` after an earlier `@arr = ...`) still sees the array as shaped
    // and refills to its fixed dimension instead of silently shrinking.
    if items
        .iter()
        .all(|v| !matches!(v.view(), ValueView::Array(..)))
    {
        let shape = vec![items.len()];
        mark_shaped_array_items(&items, Some(&shape));
        return Some(shape);
    }
    let inferred_shape = infer_shape_from_array(items.as_ref())?;
    if !shape_matches_full_structure(value, &inferred_shape) {
        return None;
    }
    mark_shaped_array_items(&items, Some(&inferred_shape));
    Some(inferred_shape)
}

pub(crate) fn mark_shaped_array(value: &Value, shape: Option<&[usize]>) {
    let ValueView::Array(items, ..) = value.view() else {
        return;
    };
    mark_shaped_array_items(&items, shape);
}

pub(crate) fn mark_shaped_array_items(
    items: &crate::gc::Gc<crate::value::ArrayData>,
    shape: Option<&[usize]>,
) {
    let Some(shape) = shape else {
        return;
    };
    if items.shape.as_deref() == Some(shape) {
        return;
    }
    // The shape is metadata about this one logical array, shared by every holder
    // of the `Arc` — matching the prior pointer-keyed side-table semantics (any
    // holder of the same pointer saw the shape). Interior mutation preserves
    // "mark after the array Value is already placed" without a write-back through
    // the caller.
    // SAFETY: aliased in-place mutation of a shared container; see
    // `gc_contents_mut`. No borrow into the array is live across this write.
    unsafe {
        crate::value::gc_contents_mut(items).shape = Some(shape.into());
    }
}

/// The leaves of a multi-dimensional shaped array, in row-major order, each
/// level read the way `@a[$i]` reads a slot (`items_with_default`). This is
/// what iterating such an array yields in Rakudo: its iterator walks the
/// leaves, not the first-dimension rows (#14248). `None` for any other value,
/// a one-dimensional shaped array included.
// Cost: O(E), E = leaves.
pub(crate) fn shaped_array_iteration_leaves(value: &Value) -> Option<Vec<Value>> {
    let depth = shaped_array_shape(value)
        .filter(|shape| shape.len() > 1)?
        .len();
    fn walk(value: &Value, depth: usize, out: &mut Vec<Value>) {
        match value.view() {
            ValueView::Array(items, ..) if depth > 0 => {
                for item in items.items_with_default().iter() {
                    walk(item, depth - 1, out);
                }
            }
            _ => out.push(value.clone()),
        }
    }
    let mut out = Vec::new();
    walk(value, depth, &mut out);
    Some(out)
}

/// Collect all leaf values from a shaped (multidimensional) array.
pub(crate) fn shaped_array_leaves(value: &Value) -> Vec<Value> {
    let mut leaves = Vec::new();
    collect_leaves(value, &mut leaves);
    leaves
}

fn collect_leaves(value: &Value, out: &mut Vec<Value>) {
    if let ValueView::Array(items, ..) = value.view() {
        if items
            .iter()
            .any(|v| matches!(v.view(), ValueView::Array(..)))
        {
            for item in items.iter() {
                collect_leaves(item, out);
            }
        } else {
            out.extend(items.iter().cloned());
        }
    } else {
        out.push(value.clone());
    }
}

/// Collect all (index-tuple, leaf-value) pairs from a shaped array.
pub(crate) fn shaped_array_indexed_leaves(value: &Value) -> Vec<(Vec<i64>, Value)> {
    let mut result = Vec::new();
    let mut indices = Vec::new();
    collect_indexed_leaves(value, &mut indices, &mut result);
    result
}

fn collect_indexed_leaves(value: &Value, indices: &mut Vec<i64>, out: &mut Vec<(Vec<i64>, Value)>) {
    if let ValueView::Array(items, ..) = value.view() {
        if items
            .iter()
            .any(|v| matches!(v.view(), ValueView::Array(..)))
        {
            for (i, item) in items.iter().enumerate() {
                indices.push(i as i64);
                collect_indexed_leaves(item, indices, out);
                indices.pop();
            }
        } else {
            for (i, item) in items.iter().enumerate() {
                let mut idx = indices.clone();
                idx.push(i as i64);
                out.push((idx, item.clone()));
            }
        }
    }
}

/// Rebuild a shaped array, replacing its leaf values (in depth-first order) with
/// `new_leaves`, while preserving the nested structure, shape metadata, and array
/// kind. Used to write `.map`/mutation results back into a shaped array without
/// flattening it into an ordinary list. `new_leaves` must have exactly as many
/// elements as the array has leaves; extras are ignored, shortfalls keep the
/// original leaf.
pub(crate) fn replace_shaped_leaves(original: &Value, new_leaves: &[Value]) -> Value {
    let mut iter = new_leaves.iter();
    rebuild_with_leaves(original, &mut iter)
}

fn rebuild_with_leaves<'a, I: Iterator<Item = &'a Value>>(value: &Value, iter: &mut I) -> Value {
    if let ValueView::Array(items, kind) = value.view() {
        let new_items: Vec<Value> = if items
            .iter()
            .any(|v| matches!(v.view(), ValueView::Array(..)))
        {
            items.iter().map(|c| rebuild_with_leaves(c, iter)).collect()
        } else {
            items
                .iter()
                .map(|orig| iter.next().cloned().unwrap_or_else(|| orig.clone()))
                .collect()
        };
        let mut data = crate::value::ArrayData::new(new_items);
        data.shape = items.shape.clone();
        Value::array_with_kind(crate::gc::Gc::new(data), kind)
    } else {
        iter.next().cloned().unwrap_or_else(|| value.clone())
    }
}

/// The index tuple of the `idx`-th leaf (row-major) of an array of `shape`.
// Cost: O(d), d = dimensions.
fn leaf_path(shape: &[usize], idx: usize) -> Option<Vec<usize>> {
    let total = shape.iter().product::<usize>();
    if idx >= total {
        return None;
    }
    let mut rest = idx;
    let mut path = vec![0; shape.len()];
    for (d, &len) in shape.iter().enumerate().rev() {
        path[d] = rest % len.max(1);
        rest /= len.max(1);
    }
    Some(path)
}

/// The `idx`-th leaf, in iteration order, of a multi-dimensional shaped
/// array (see [`shaped_array_iteration_leaves`]). `None` for any other value
/// or an index past the last leaf.
// Cost: O(d), d = dimensions.
pub(crate) fn shaped_array_leaf_at(value: &Value, idx: usize) -> Option<Value> {
    let shape = shaped_array_shape(value).filter(|shape| shape.len() > 1)?;
    let mut cur = value.clone();
    for i in leaf_path(&shape, idx)? {
        let next = match cur.view() {
            ValueView::Array(items, ..) => items.items_with_default().get(i).cloned()?,
            _ => return None,
        };
        cur = next;
    }
    Some(cur)
}

/// A copy of the multi-dimensional shaped array `value` with its `idx`-th
/// leaf, in iteration order, replaced by `leaf`. Each level on the path is a
/// clone of its `ArrayData`, so the element type, default and shape metadata
/// survive. `None` for any other value or an index past the last leaf.
// Cost: O(d * w), d = dimensions, w = the widest level's length (one clone
// per level on the path).
pub(crate) fn with_shaped_array_leaf(value: &Value, idx: usize, leaf: Value) -> Option<Value> {
    let shape = shaped_array_shape(value).filter(|shape| shape.len() > 1)?;
    fn set(value: &Value, path: &[usize], leaf: Value) -> Option<Value> {
        let Some((&i, rest)) = path.split_first() else {
            return Some(leaf);
        };
        let ValueView::Array(items, kind) = value.view() else {
            return None;
        };
        let child = items.items().get(i)?.clone();
        let mut data = crate::value::ArrayData::clone(&items);
        data.live_mut()[i] = set(&child, rest, leaf)?;
        Some(Value::array_with_kind(crate::gc::Gc::new(data), kind))
    }
    set(value, &leaf_path(&shape, idx)?, leaf)
}

/// The List of a multi-dimensional shaped array's leaves, which is what a
/// reader that iterates the array sees (#14248, #14308). `None` for any other
/// value.
// Cost: O(E), E = leaves.
pub(crate) fn shaped_array_leaves_list(value: &Value) -> Option<Value> {
    shaped_array_iteration_leaves(value).map(|leaves| {
        Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(leaves)),
            ArrayKind::List,
        )
    })
}
