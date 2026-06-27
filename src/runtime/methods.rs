use super::*;
use crate::symbol::Symbol;
use num_traits::ToPrimitive;

/// Parse a non-negative integer index, returning None for negative or non-numeric.
fn pos_index(v: &Value) -> Option<usize> {
    match v {
        Value::Int(i) if *i >= 0 => Some(*i as usize),
        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
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
    failure_attrs.insert("handled".to_string(), Value::Bool(false));
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// Recursively fetch @target[indices...]; returns Failure for any negative index,
/// or Nil if the chain runs out of elements.
pub(crate) fn multidim_at_pos(target: &Value, indices: &[Value]) -> Value {
    let mut cur = target.clone();
    for idx in indices {
        // Transparently unwrap Scalar containers (used as "bound" markers for BIND-POS).
        cur = cur.into_descalarized();
        let Some(i) = pos_index(idx) else {
            return make_nonneg_failure();
        };
        let Some(items) = cur.as_list_items() else {
            return Value::Nil;
        };
        cur = items.get(i).cloned().unwrap_or(Value::Nil);
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
    match v {
        Value::Nil => false,
        Value::Package(s) if s == "Any" => false,
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
    attrs.insert("got-dimensions".to_string(), Value::Int(got as i64));
    attrs.insert("needed-dimensions".to_string(), Value::Int(needed as i64));
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

/// Recursively assign value at indices, rebuilding the array chain.
/// If the innermost slot is currently a Scalar (BIND-POS marker), returns an error.
pub(crate) fn multidim_assign_pos(
    target: &Value,
    indices: &[Value],
    value: Value,
) -> Result<Value, RuntimeError> {
    assert!(!indices.is_empty());
    // Unwrap any outer Scalar wrapper.
    if let Value::Scalar(_) = target {
        return Err(RuntimeError::assignment_ro(None));
    }
    let Value::Array(items, arr_kind) = target else {
        return Err(RuntimeError::new(
            "Cannot use multi-dimensional ASSIGN-POS on non-Array",
        ));
    };
    let Some(i) = pos_index(&indices[0]) else {
        return Err(RuntimeError::new("Cannot ASSIGN-POS with a negative index"));
    };
    let mut updated = items.to_vec();
    if indices.len() == 1 {
        // Check for bound slot (Scalar wrapper)
        if let Some(Value::Scalar(_)) = updated.get(i) {
            return Err(RuntimeError::assignment_ro(None));
        }
        if i >= updated.len() {
            updated.resize(i + 1, Value::Package(Symbol::intern("Any")));
        }
        updated[i] = value;
    } else {
        let child = updated
            .get(i)
            .cloned()
            .unwrap_or_else(|| Value::real_array(vec![]));
        let new_child = multidim_assign_pos(&child, &indices[1..], value)?;
        if i >= updated.len() {
            updated.resize(i + 1, Value::real_array(vec![]));
        }
        updated[i] = new_child;
    }
    Ok(Value::Array(
        std::sync::Arc::new(crate::value::ArrayData::new(updated)),
        *arr_kind,
    ))
}

/// Recursively bind value at indices. The innermost slot is stored as
/// Value::Scalar(value) to mark it as bound (immutable).
pub(crate) fn multidim_bind_pos(
    target: &Value,
    indices: &[Value],
    value: Value,
) -> Result<Value, RuntimeError> {
    assert!(!indices.is_empty());
    let Value::Array(items, arr_kind) = target else {
        return Err(RuntimeError::new(
            "Cannot use multi-dimensional BIND-POS on non-Array",
        ));
    };
    let Some(i) = pos_index(&indices[0]) else {
        return Err(RuntimeError::new("Cannot BIND-POS with a negative index"));
    };
    let mut updated = items.to_vec();
    if indices.len() == 1 {
        if i >= updated.len() {
            updated.resize(i + 1, Value::Package(Symbol::intern("Any")));
        }
        updated[i] = Value::Scalar(Box::new(value));
    } else {
        let child = updated
            .get(i)
            .cloned()
            .unwrap_or_else(|| Value::real_array(vec![]));
        let new_child = multidim_bind_pos(&child, &indices[1..], value)?;
        if i >= updated.len() {
            updated.resize(i + 1, Value::real_array(vec![]));
        }
        updated[i] = new_child;
    }
    Ok(Value::Array(
        std::sync::Arc::new(crate::value::ArrayData::new(updated)),
        *arr_kind,
    ))
}

/// Recursively delete the innermost slot. Returns (deleted_value, updated_outer_array).
pub(crate) fn multidim_delete_pos(
    target: &Value,
    indices: &[Value],
) -> Result<(Value, Value), RuntimeError> {
    assert!(!indices.is_empty());
    let Value::Array(items, arr_kind) = target else {
        return Err(RuntimeError::new(
            "Cannot use multi-dimensional DELETE-POS on non-Array",
        ));
    };
    let Some(i) = pos_index(&indices[0]) else {
        return Err(RuntimeError::new("Cannot DELETE-POS with a negative index"));
    };
    let mut updated = items.to_vec();
    let deleted;
    if indices.len() == 1 {
        if i < updated.len() {
            let old = std::mem::replace(&mut updated[i], Value::Nil);
            deleted = match old {
                Value::Scalar(inner) => *inner,
                v => v,
            };
        } else {
            deleted = Value::Nil;
        }
    } else {
        let child = updated
            .get(i)
            .cloned()
            .unwrap_or_else(|| Value::real_array(vec![]));
        let (d, new_child) = multidim_delete_pos(&child, &indices[1..])?;
        if i < updated.len() {
            updated[i] = new_child;
        }
        deleted = d;
    }
    Ok((
        deleted,
        Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(updated)),
            *arr_kind,
        ),
    ))
}

/// Compare two values numerically (like Raku's == operator) for allomorph ACCEPTS.
pub(crate) fn allomorph_numeric_equal(a: &Value, b: &Value) -> bool {
    let a_f = allomorph_val_to_f64(a);
    let b_f = allomorph_val_to_f64(b);
    match (a_f, b_f) {
        (Some(af), Some(bf)) => {
            // Handle Complex: both real and imaginary must match
            if let (Value::Complex(ar, ai), Value::Complex(br, bi)) = (a, b) {
                return (ar - br).abs() < 1e-15 && (ai - bi).abs() < 1e-15;
            }
            if let Value::Complex(ar, ai) = a {
                return (ar - bf).abs() < 1e-15 && ai.abs() < 1e-15;
            }
            if let Value::Complex(br, bi) = b {
                return (af - br).abs() < 1e-15 && bi.abs() < 1e-15;
            }
            (af - bf).abs() < 1e-15
        }
        _ => false,
    }
}

fn allomorph_val_to_f64(v: &Value) -> Option<f64> {
    match v {
        Value::Int(i) => Some(*i as f64),
        Value::BigInt(n) => n.to_f64(),
        Value::Num(f) => Some(*f),
        Value::Rat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
        Value::FatRat(n, d) if *d != 0 => Some(*n as f64 / *d as f64),
        Value::Complex(r, _) => Some(*r),
        Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
        Value::Mixin(inner, _) => allomorph_val_to_f64(inner),
        _ => None,
    }
}
