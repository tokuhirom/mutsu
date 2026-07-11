//! Multi-dimensional array indexing, assignment, and deletion helpers
//! split from `vm_var_ops` (§7-8 file split).
use super::*;
use crate::symbol::Symbol;
use num_traits::Zero;

impl Interpreter {
    pub(super) fn array_depth(value: &Value) -> usize {
        if let Some(shape) = crate::runtime::utils::shaped_array_shape(value)
            && !shape.is_empty()
        {
            return shape.len();
        }
        match value.view() {
            ValueView::Array(items, ..) => {
                let child = items
                    .first()
                    .map(Self::array_depth)
                    .filter(|d| *d > 0)
                    .unwrap_or(0);
                1 + child
            }
            _ => 0,
        }
    }

    pub(super) fn index_to_usize(idx: &Value) -> Option<usize> {
        // A non-finite index (Inf/NaN) has no valid `usize` representation.
        // Rust's `f as usize` saturates Inf to `usize::MAX` rather than failing,
        // which would later index far out of bounds and panic; reject it here so
        // every caller sees `None` (see `@a[Inf] = ...` in S02-types/array.t).
        match idx.view() {
            ValueView::Int(i) if i >= 0 => Some(i as usize),
            ValueView::Num(f) if f >= 0.0 && f.is_finite() => Some(f as usize),
            ValueView::Rat(n, d) if d != 0 => {
                let f = n as f64 / d as f64;
                (f >= 0.0 && f.is_finite()).then_some(f as usize)
            }
            ValueView::FatRat(n, d) if d != 0 => {
                let f = n as f64 / d as f64;
                (f >= 0.0 && f.is_finite()).then_some(f as usize)
            }
            ValueView::BigRat(_, d) if !d.is_zero() => {
                let f = runtime::to_float_value(idx)?;
                (f >= 0.0 && f.is_finite()).then_some(f as usize)
            }
            _ => idx.to_string_value().parse::<usize>().ok(),
        }
    }

    fn not_enough_dimensions_error(operation: &str, got: usize, needed: usize) -> RuntimeError {
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

    pub(super) fn index_array_multidim(
        target: &Value,
        indices: &[Value],
        strict_oob: bool,
    ) -> Result<Value, RuntimeError> {
        if indices.is_empty() {
            return Ok(target.clone());
        }
        // An intermediate level may be a `ContainerRef` element cell (Track B:
        // the celled atomic store boxes top-level elements, and `:=` bindings
        // can nest cells anywhere). Read through it ONLY when the cell holds a
        // container, so the remaining indices land on the inner array; a
        // *terminal* cell is still returned as-is (the empty-indices base case
        // above), and a scalar cell keeps the pre-cell behavior (`Nil` below) —
        // see `multi_dim_index_read` for the aliasing this preserves.
        if target.is_container_ref() {
            let inner = target.deref_container();
            if matches!(inner.view(), ValueView::Array(..) | ValueView::Hash(..)) {
                return Self::index_array_multidim(&inner, indices, strict_oob);
            }
        }
        let head = &indices[0];
        if matches!(head.view(), ValueView::Whatever)
            || matches!(head.view(), ValueView::Num(f) if f.is_infinite() && f > 0.0)
        {
            if indices.len() > 1 && crate::runtime::utils::is_shaped_array(target) {
                return Err(RuntimeError::typed_msg("X::NYI", "Not yet implemented"));
            }
            let ValueView::Array(items, ..) = target.view() else {
                return Ok(Value::NIL);
            };
            let mut out = Vec::with_capacity(items.len());
            for item in items.iter() {
                out.push(Self::index_array_multidim(item, &indices[1..], strict_oob)?);
            }
            return Ok(Value::array(out));
        }
        let Some(i) = Self::index_to_usize(head) else {
            return Ok(Value::NIL);
        };
        let ValueView::Array(items, ..) = target.view() else {
            return Ok(Value::NIL);
        };
        if i >= items.len() {
            if strict_oob {
                return Err(RuntimeError::new("Index out of bounds"));
            }
            return Ok(Value::NIL);
        }
        Self::index_array_multidim(&items[i], &indices[1..], strict_oob)
    }

    pub(super) fn assign_array_multidim(
        target: &mut Value,
        indices: &[Value],
        val: Value,
    ) -> Result<(), RuntimeError> {
        // Write *through* a `ContainerRef` level (Track B element cell or a
        // `:=`-bound element): mutate the cell's inner value under its lock so
        // every snapshot holder observes the write, instead of failing the
        // `with_array_mut` below. Covers both a celled top-level slot and
        // celled intermediate levels (the recursion below re-enters here).
        if let ValueView::ContainerRef(c) = target.view() {
            let cell = c.clone();
            let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
            let mut inner = guard.clone();
            Self::assign_array_multidim(&mut inner, indices, val)?;
            *guard = inner;
            return Ok(());
        }
        let shape = crate::runtime::utils::shaped_array_shape(target);
        let depth = Self::array_depth(target);
        if indices.len() < depth && depth > 1 {
            return Err(Self::not_enough_dimensions_error(
                "assign to",
                indices.len(),
                depth,
            ));
        }
        if indices.is_empty() {
            return Err(RuntimeError::new("Index out of bounds"));
        }
        let Some(i) = Self::index_to_usize(&indices[0]) else {
            return Err(RuntimeError::new("Index out of bounds"));
        };
        let Some(res) = target.with_array_mut(|items, _| {
            if i >= items.len() {
                return Err(RuntimeError::new("Index out of bounds"));
            }
            // Container identity (§3): write through a shared backing node.
            let arr = crate::value::gc_data_mut(items);
            if indices.len() == 1 {
                Value::assign_element_slot(&mut arr[i], val);
            } else {
                Self::assign_array_multidim(&mut arr[i], &indices[1..], val)?;
            }
            if let Some(shape) = shape.as_deref() {
                crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
            }
            Ok(())
        }) else {
            return Err(RuntimeError::new("Index out of bounds"));
        };
        res
    }

    pub(super) fn delete_array_multidim(
        target: &mut Value,
        indices: &[Value],
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        let shape = crate::runtime::utils::shaped_array_shape(target);
        let depth = Self::array_depth(target);
        if indices.len() < depth && depth > 1 {
            return Err(Self::not_enough_dimensions_error(
                "delete from",
                indices.len(),
                depth,
            ));
        }
        let hole_value = || Value::package(Symbol::intern(hole_type));
        if indices.is_empty() {
            return Ok(hole_value());
        }
        let Some(i) = Self::index_to_usize(&indices[0]) else {
            return Ok(hole_value());
        };
        let Some(res) = target.with_array_mut(|items, _| {
            if i >= items.len() {
                return Ok(hole_value());
            }
            // Container identity (§3): write through a shared backing node.
            let arr = crate::value::gc_data_mut(items);
            if indices.len() == 1 {
                let prev = arr[i].clone();
                arr[i] = hole_value();
                if let Some(shape) = shape.as_deref() {
                    crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
                }
                return Ok(prev);
            }
            let deleted = Self::delete_array_multidim(&mut arr[i], &indices[1..], hole_type)?;
            if let Some(shape) = shape.as_deref() {
                crate::runtime::utils::mark_shaped_array_items(items, Some(shape));
            }
            Ok(deleted)
        }) else {
            return Ok(hole_value());
        };
        res
    }
}
