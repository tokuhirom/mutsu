use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;
use num_traits::ToPrimitive;

/// Parse a non-negative integer index, returning None for negative or non-numeric.
fn pos_index(v: &Value) -> Option<usize> {
    match v.view() {
        ValueView::Int(i) if i >= 0 => Some(i as usize),
        ValueView::Num(f) if f >= 0.0 => Some(f as usize),
        _ => None,
    }
}

fn make_nonneg_failure() -> Value {
    let mut ex_attrs = std::collections::HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str("Index out of range. Is: negative, should be in 0..^Inf".to_string()),
    );
    let exception = Value::make_instance(Symbol::intern("X::OutOfRange"), ex_attrs);
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// Recursively fetch @target[indices...]; returns Failure for any negative index.
/// A missing slot of a real Array reads as its element default (`Any`), like a
/// single-dim out-of-range read; a List miss (or a non-list link) reads as Nil.
pub(crate) fn multidim_at_pos(target: &Value, indices: &[Value]) -> Value {
    let mut cur = target.clone();
    for idx in indices {
        // Transparently unwrap Scalar containers (used as "bound" markers for BIND-POS).
        cur = cur.into_descalarized();
        let Some(i) = pos_index(idx) else {
            return make_nonneg_failure();
        };
        let is_real_array =
            matches!(cur.view(), crate::value::ValueView::Array(_, kind) if kind.is_real_array());
        let Some(items) = cur.as_list_items() else {
            return Value::NIL;
        };
        cur = items.get(i).cloned().unwrap_or_else(|| {
            if is_real_array {
                Value::package(crate::symbol::wk::any())
            } else {
                Value::NIL
            }
        });
    }
    cur.into_descalarized()
}

pub(crate) fn multidim_exists_pos(target: &Value, indices: &[Value]) -> bool {
    let mut cur = target.clone();
    for idx in indices {
        cur = cur.into_descalarized();
        let Some(i) = pos_index(idx) else {
            return false;
        };
        let Some(items) = cur.as_list_items() else {
            return false;
        };
        if i >= items.len() {
            return false;
        }
        cur = items[i].clone();
    }
    true
}

/// EXISTS-POS for shaped arrays: checks that the leaf element has actually
/// been assigned (is not Nil or the type object Any).
pub(crate) fn shaped_multidim_exists_pos(
    target: &Value,
    indices: &[Value],
    shape: &[usize],
) -> bool {
    let mut cur = target.clone();
    for (dim_idx, idx) in indices.iter().enumerate() {
        cur = cur.into_descalarized();
        let Some(i) = pos_index(idx) else {
            return false;
        };
        if dim_idx < shape.len() && i >= shape[dim_idx] {
            return false;
        }
        let Some(items) = cur.as_list_items() else {
            return false;
        };
        if i >= items.len() {
            return false;
        }
        cur = items[i].clone();
    }
    if indices.len() < shape.len() {
        return true;
    }
    is_assigned_value(&cur)
}

/// Check whether a value represents an assigned (non-default) cell.
fn is_assigned_value(v: &Value) -> bool {
    match v.view() {
        ValueView::Nil => false,
        ValueView::Package(s) if s == "Any" => false,
        _ => true,
    }
}

/// Check that multi-dimensional indices are within bounds for a shaped array.
pub(crate) fn check_shaped_bounds(shape: &[usize], indices: &[Value]) -> Result<(), RuntimeError> {
    for (dim_idx, idx) in indices.iter().enumerate() {
        let Some(i) = pos_index(idx) else {
            continue;
        };
        if dim_idx < shape.len() && i >= shape[dim_idx] {
            return Err(RuntimeError::new(format!(
                "Index {} for dimension {} out of range 0..{}",
                i,
                dim_idx + 1,
                shape[dim_idx]
            )));
        }
    }
    Ok(())
}

/// Create a X::NotEnoughDimensions error.
pub(crate) fn make_not_enough_dimensions_error(
    operation: &str,
    got: usize,
    needed: usize,
) -> RuntimeError {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("operation".to_string(), Value::str(operation.to_string()));
    attrs.insert("got-dimensions".to_string(), Value::int(got as i64));
    attrs.insert("needed-dimensions".to_string(), Value::int(needed as i64));
    attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Not enough dimensions: got {}, needed {}",
            got, needed
        )),
    );
    let mut err = RuntimeError::new("X::NotEnoughDimensions");
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::NotEnoughDimensions"),
        attrs,
    )));
    err
}

/// The array one level of a multi-dimensional `*-POS` walk writes into: the
/// level itself, or the array a `ContainerRef` element cell holds (the cell
/// shares the array's node, so writing into it writes the element).
fn multidim_level(target: &Value) -> Value {
    if target.is_container_ref() {
        let inner = target.deref_container();
        if matches!(inner.view(), ValueView::Array(..)) {
            return inner;
        }
    }
    target.clone()
}

/// The next level down for a multi-dimensional store at slot `i` of `data`:
/// the existing child, or -- past the end -- a fresh Array autovivified into
/// slot `i` (the skipped slots stay holes).
// Cost: O(1) amortized; O(i - e) when growing, i = index, e = elements.
fn multidim_child_for_store(data: &mut crate::value::ArrayData, i: usize) -> Value {
    if i >= data.len() {
        data.store_element(i, Value::real_array(vec![]));
    }
    multidim_level(&data[i])
}

/// `ASSIGN-POS` with several indices: store `value` at the innermost slot,
/// writing through each level's shared node in place (container identity), so
/// every holder of the array -- and of each inner array -- observes it.
/// A bound (`BIND-POS`) innermost slot refuses the assignment.
// Cost: O(d), d = indices (amortized; growing a level is O(i - e) there).
pub(crate) fn multidim_assign_pos(
    target: &Value,
    indices: &[Value],
    value: Value,
) -> Result<(), RuntimeError> {
    assert!(!indices.is_empty());
    if let ValueView::Scalar(_) = target.view() {
        return Err(RuntimeError::assignment_ro(None));
    }
    let ValueView::Array(items, _) = target.view() else {
        return Err(RuntimeError::new(
            "Cannot use multi-dimensional ASSIGN-POS on non-Array",
        ));
    };
    let Some(i) = pos_index(&indices[0]) else {
        return Err(RuntimeError::new("Cannot ASSIGN-POS with a negative index"));
    };
    // SAFETY: audited aliased in-place container write (see
    // value::aliased_mut); no borrow into the node is live across it.
    let data = unsafe { crate::value::gc_contents_mut(&items) };
    if indices.len() == 1 {
        if data
            .get(i)
            .is_some_and(|v| matches!(v.view(), ValueView::Scalar(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        data.store_element(i, value);
        return Ok(());
    }
    let child = multidim_child_for_store(data, i);
    multidim_assign_pos(&child, &indices[1..], value)
}

/// `BIND-POS` with several indices: bind the innermost slot (stored as
/// `Value::scalar(value)`, which marks it bound/immutable), in place through
/// each level's shared node like [`multidim_assign_pos`].
// Cost: O(d), d = indices (amortized; growing a level is O(i - e) there).
pub(crate) fn multidim_bind_pos(
    target: &Value,
    indices: &[Value],
    value: Value,
) -> Result<(), RuntimeError> {
    assert!(!indices.is_empty());
    let ValueView::Array(items, _) = target.view() else {
        return Err(RuntimeError::new(
            "Cannot use multi-dimensional BIND-POS on non-Array",
        ));
    };
    let Some(i) = pos_index(&indices[0]) else {
        return Err(RuntimeError::new("Cannot BIND-POS with a negative index"));
    };
    // SAFETY: audited aliased in-place container write (see
    // value::aliased_mut); no borrow into the node is live across it.
    let data = unsafe { crate::value::gc_contents_mut(&items) };
    if indices.len() == 1 {
        data.store_element(i, Value::scalar(value));
        return Ok(());
    }
    let child = multidim_child_for_store(data, i);
    multidim_bind_pos(&child, &indices[1..], value)
}

/// `DELETE-POS` with several indices: vacate the innermost slot in place
/// (it becomes a hole) and return what it held; `Nil` when the path does not
/// reach an existing slot.
// Cost: O(d), d = indices.
pub(crate) fn multidim_delete_pos(
    target: &Value,
    indices: &[Value],
) -> Result<Value, RuntimeError> {
    assert!(!indices.is_empty());
    let ValueView::Array(items, _) = target.view() else {
        return Err(RuntimeError::new(
            "Cannot use multi-dimensional DELETE-POS on non-Array",
        ));
    };
    let Some(i) = pos_index(&indices[0]) else {
        return Err(RuntimeError::new("Cannot DELETE-POS with a negative index"));
    };
    if i >= items.len() {
        return Ok(Value::NIL);
    }
    // SAFETY: audited aliased in-place container write (see
    // value::aliased_mut); no borrow into the node is live across it.
    let data = unsafe { crate::value::gc_contents_mut(&items) };
    if indices.len() == 1 {
        // The innermost level deletes like the single-dimension form, trailing
        // holes trimmed included (#10926).
        return Ok(Interpreter::delete_pos_in_array_data(data, i));
    }
    let child = multidim_level(&data[i]);
    multidim_delete_pos(&child, &indices[1..])
}

/// Compare two values numerically (like Raku's == operator) for allomorph ACCEPTS.
pub(crate) fn allomorph_numeric_equal(a: &Value, b: &Value) -> bool {
    let a_f = allomorph_val_to_f64(a);
    let b_f = allomorph_val_to_f64(b);
    match (a_f, b_f) {
        (Some(af), Some(bf)) => {
            // Handle Complex: both real and imaginary must match
            if let (ValueView::Complex(ar, ai), ValueView::Complex(br, bi)) = (a.view(), b.view()) {
                return (ar - br).abs() < 1e-15 && (ai - bi).abs() < 1e-15;
            }
            if let ValueView::Complex(ar, ai) = a.view() {
                return (ar - bf).abs() < 1e-15 && ai.abs() < 1e-15;
            }
            if let ValueView::Complex(br, bi) = b.view() {
                return (af - br).abs() < 1e-15 && bi.abs() < 1e-15;
            }
            (af - bf).abs() < 1e-15
        }
        _ => false,
    }
}

fn allomorph_val_to_f64(v: &Value) -> Option<f64> {
    match v.view() {
        ValueView::Int(i) => Some(i as f64),
        ValueView::BigInt(n) => n.to_f64(),
        ValueView::Num(f) => Some(f),
        ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
        ValueView::FatRat(n, d) if d != 0 => Some(n as f64 / d as f64),
        ValueView::Complex(r, _) => Some(r),
        ValueView::Bool(b) => Some(if b { 1.0 } else { 0.0 }),
        ValueView::Mixin(inner, _) => allomorph_val_to_f64(inner),
        _ => None,
    }
}
