use super::*;

/// Check if an array is a shaped (multidimensional) array.
/// A shaped array is one explicitly created as multidimensional via `:shape`.
pub(crate) fn is_shaped_array(value: &Value) -> bool {
    if let Value::Array(_, kind) = value
        && *kind == ArrayKind::Shaped
    {
        return true;
    }
    shaped_array_shape(value).is_some()
}

pub(crate) fn shaped_array_shape(value: &Value) -> Option<Vec<usize>> {
    let Value::Array(items, kind) = value else {
        return None;
    };
    // Only arrays explicitly created as shaped can be shaped
    if *kind != ArrayKind::Shaped {
        return None;
    }

    fn shape_matches_structure(value: &Value, shape: &[usize]) -> bool {
        if shape.is_empty() {
            return false;
        }
        let Value::Array(items, ..) = value else {
            return false;
        };
        if items.len() != shape[0] {
            return false;
        }
        if shape.len() == 1 {
            return items.iter().all(|v| !matches!(v, Value::Array(..)));
        }
        items
            .iter()
            .all(|child| shape_matches_structure(child, &shape[1..]))
    }

    if items.is_empty() {
        return None;
    }

    // Infer this array's shape from its children's embedded shapes.
    fn infer_shape_from_array(items: &[Value]) -> Option<Vec<usize>> {
        let first = items.first()?;
        let Value::Array(first_items, ..) = first else {
            return None;
        };
        let first_shape = first_items.shape.as_ref()?;
        if !items.iter().all(|child| {
            if let Value::Array(child_items, ..) = child {
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
        && shape_matches_structure(value, cached_shape)
    {
        return Some(cached_shape.clone());
    }
    // A flat (1-dim) shaped array's shape is unambiguously `[len]`. Recover it
    // even when the cached `ArrayData.shape` was dropped crossing a store
    // boundary (the dual-store sync does not always carry it), so a re-assignment
    // (`@arr = ...` after an earlier `@arr = ...`) still sees the array as shaped
    // and refills to its fixed dimension instead of silently shrinking.
    if items.iter().all(|v| !matches!(v, Value::Array(..))) {
        return Some(vec![items.len()]);
    }
    let inferred_shape = infer_shape_from_array(items.as_ref())?;
    if !shape_matches_structure(value, &inferred_shape) {
        return None;
    }
    mark_shaped_array_items(items, Some(&inferred_shape));
    Some(inferred_shape)
}

pub(crate) fn mark_shaped_array(value: &Value, shape: Option<&[usize]>) {
    let Value::Array(items, ..) = value else {
        return;
    };
    mark_shaped_array_items(items, shape);
}

pub(crate) fn mark_shaped_array_items(
    items: &Arc<crate::value::ArrayData>,
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
    // `arc_contents_mut`. No borrow into the array is live across this write.
    unsafe {
        crate::value::arc_contents_mut(items).shape = Some(shape.to_vec());
    }
}

/// Collect all leaf values from a shaped (multidimensional) array.
pub(crate) fn shaped_array_leaves(value: &Value) -> Vec<Value> {
    let mut leaves = Vec::new();
    collect_leaves(value, &mut leaves);
    leaves
}

fn collect_leaves(value: &Value, out: &mut Vec<Value>) {
    if let Value::Array(items, ..) = value {
        if items.iter().any(|v| matches!(v, Value::Array(..))) {
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
    if let Value::Array(items, ..) = value {
        if items.iter().any(|v| matches!(v, Value::Array(..))) {
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
    if let Value::Array(items, kind) = value {
        let new_items: Vec<Value> = if items.iter().any(|v| matches!(v, Value::Array(..))) {
            items.iter().map(|c| rebuild_with_leaves(c, iter)).collect()
        } else {
            items
                .iter()
                .map(|orig| iter.next().cloned().unwrap_or_else(|| orig.clone()))
                .collect()
        };
        let mut data = crate::value::ArrayData::new(new_items);
        data.shape = items.shape.clone();
        Value::Array(Arc::new(data), *kind)
    } else {
        iter.next().cloned().unwrap_or_else(|| value.clone())
    }
}

pub(crate) fn values_identical(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Package(name), Value::Int(0)) | (Value::Int(0), Value::Package(name))
            if name.resolve() == "int" =>
        {
            true
        }
        (Value::Array(a, _), Value::Array(b, _)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Seq(a), Value::Seq(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Slip(a), Value::Slip(b)) => {
            // Empty is a singleton semantic value in Raku even when represented
            // by distinct empty Slip allocations.
            (a.is_empty() && b.is_empty()) || std::sync::Arc::ptr_eq(a, b)
        }
        (Value::LazyList(a), Value::LazyList(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Hash(a), Value::Hash(b)) => std::sync::Arc::ptr_eq(a, b),
        (Value::Sub(a), Value::Sub(b)) => {
            if std::sync::Arc::ptr_eq(a, b) {
                return true;
            }
            // Named subs with the same package and name are identical
            // (e.g. &foo === &EXPORT::ALL::foo when both resolve to the same definition)
            let a_name = a.name.resolve();
            let b_name = b.name.resolve();
            !a_name.is_empty() && a_name == b_name && a.package == b.package
        }
        (Value::WeakSub(a), Value::WeakSub(b)) => a.ptr_eq(b),
        (Value::Mixin(a_inner, a_mix), Value::Mixin(b_inner, b_mix)) => {
            a_inner.eqv(b_inner) && a_mix == b_mix
        }
        (Value::Mixin(_, _), _) | (_, Value::Mixin(_, _)) => false,
        (
            Value::Instance {
                class_name: a_class,
                id: a_id,
                attributes: a_attrs,
                ..
            },
            Value::Instance {
                class_name: b_class,
                id: b_id,
                attributes: b_attrs,
                ..
            },
        ) => {
            let a_name = a_class.resolve();
            let b_name = b_class.resolve();
            if a_name == b_name
                && (a_name == "Stash" || a_name == "Supply" || a_name == "IO::Special")
            {
                left.eqv(right)
            } else if a_name == b_name && (a_name == "ObjAt" || a_name == "ValueObjAt") {
                // ObjAt/ValueObjAt instances are === when their WHICH content matches
                let a_val = a_attrs.as_map().get("WHICH").map(|v| v.to_string_value());
                let b_val = b_attrs.as_map().get("WHICH").map(|v| v.to_string_value());
                a_val == b_val
            } else {
                a_id == b_id
            }
        }
        // Junction identity: each junction object is unique
        (Value::Junction { values: a_vals, .. }, Value::Junction { values: b_vals, .. }) => {
            std::sync::Arc::ptr_eq(a_vals, b_vals)
        }
        // Capture identity is NOT structural: a Capture's `.WHICH` keeps the
        // *container* identity of each captured element, so `\($a) === \($b)`
        // is False even when `$a` and `$b` hold equal values, while
        // `\(42) === \(42)` is True (both literal-value elements). This is
        // unlike `eqv`, which deconts and compares structurally.
        (
            Value::Capture {
                positional: ap,
                named: an,
            },
            Value::Capture {
                positional: bp,
                named: bn,
            },
        ) => {
            ap.len() == bp.len()
                && an.len() == bn.len()
                && ap
                    .iter()
                    .zip(bp.iter())
                    .all(|(x, y)| capture_elem_identical(x, y))
                && an
                    .iter()
                    .all(|(k, v)| bn.get(k).is_some_and(|bv| capture_elem_identical(v, bv)))
        }
        _ => left.eqv(right),
    }
}

/// Identity comparison for a single Capture element. Unlike a top-level
/// `===` (which deconts a bound scalar to its value), a Capture retains the
/// container identity of each element: two `ContainerRef` cells are identical
/// only when they are the same `Arc` (i.e. bound to the same container), and a
/// container can never be identical to a plain value. Non-container elements
/// fall back to the normal value identity rules.
fn capture_elem_identical(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::ContainerRef(x), Value::ContainerRef(y)) => std::sync::Arc::ptr_eq(x, y),
        (Value::ContainerRef(_), _) | (_, Value::ContainerRef(_)) => false,
        _ => values_identical(a, b),
    }
}
