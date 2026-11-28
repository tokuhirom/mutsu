//! Jagged arrays (#19282): a shape that leaves the length of a dimension
//! unset, `my @a[2;*]` / `Array.new(:shape(*, 3))`, as rakudo 2026.11's
//! `Array::Jagged` describes it.
//!
//! A jagged array is an ordinary `Array` of rows (see
//! `value::shaped_array::jagged_array_shape` for how its shape is kept). Each
//! row is the array the remaining dimensions describe: a growable `Array` for
//! a lone `*`, a fixed shaped array, or another jagged array. A first
//! dimension of set length has that many rows, made up front, and refuses to
//! grow or shrink; a `*` one grows as rows are written, each new row of the
//! row shape.

use super::*;
use crate::value::shaped_array::{encode_jagged_shape, jagged_array_shape};

impl Interpreter {
    /// The shape of a `.new(:shape(...))` call when it is jagged: two or more
    /// dimensions, at least one of them `*`. `None` for any other shape.
    // Cost: O(a + d), a = arguments, d = dimensions.
    pub(super) fn jagged_dims_from_new_args(
        &self,
        args: &[Value],
    ) -> Result<Option<Vec<Option<usize>>>, RuntimeError> {
        let Some(shape) = args.iter().find_map(|arg| match arg.view() {
            ValueView::Pair(name, value) if name == "shape" => Some(value.deref_container()),
            _ => None,
        }) else {
            return Ok(None);
        };
        let Some(items) = shape.as_list_items() else {
            return Ok(None);
        };
        if items.len() < 2
            || !items
                .iter()
                .any(|d| matches!(d.deref_container().view(), ValueView::Whatever))
        {
            return Ok(None);
        }
        let mut dims = Vec::with_capacity(items.len());
        for dim in items.iter() {
            let dim = dim.deref_container();
            if matches!(dim.view(), ValueView::Whatever) {
                dims.push(None);
                continue;
            }
            // A dimension of set length is validated as any shape's is.
            let one = [Value::pair("shape".to_string(), dim)];
            match self.shaped_dims_from_new_args(&one)?.as_deref() {
                Some([n]) => dims.push(Some(*n)),
                _ => return Ok(None),
            }
        }
        Ok(Some(dims))
    }

    /// A new jagged array of `shape`, its rows filled from `data`'s items
    /// (each row's own list), with elements of type `of` when given.
    // Cost: O(r + E), r = rows, E = elements supplied.
    pub(super) fn construct_jagged_array(
        &mut self,
        shape: &[Option<usize>],
        data: Option<Value>,
        type_args: &Option<Vec<String>>,
    ) -> Result<Value, RuntimeError> {
        let given = data.map(|d| row_items(&d)).unwrap_or_default();
        let rows = match shape[0] {
            Some(len) => {
                if given.len() > len {
                    return Err(RuntimeError::new(format!(
                        "Index {len} for dimension 1 out of range (must be 0..{})",
                        len - 1
                    )));
                }
                let mut rows = Vec::with_capacity(len);
                for i in 0..len {
                    rows.push(self.jagged_row(&shape[1..], given.get(i).cloned(), type_args)?);
                }
                rows
            }
            None => {
                let mut rows = Vec::with_capacity(given.len());
                for item in given {
                    rows.push(self.jagged_row(&shape[1..], Some(item), type_args)?);
                }
                rows
            }
        };
        let array = Value::real_array(rows);
        crate::runtime::utils::mark_shaped_array(&array, Some(&encode_jagged_shape(shape)));
        Ok(self.tag_jagged_type(array, type_args))
    }

    /// A row of a jagged array whose rows have shape `rest`, holding `data`.
    // Cost: O(E), E = elements of the row.
    pub(super) fn jagged_row(
        &mut self,
        rest: &[Option<usize>],
        data: Option<Value>,
        type_args: &Option<Vec<String>>,
    ) -> Result<Value, RuntimeError> {
        if let [None] = rest {
            let items = data.map(|d| row_items(&d)).unwrap_or_default();
            return Ok(self.tag_jagged_type(Value::real_array(items), type_args));
        }
        let dims: Vec<Value> = rest
            .iter()
            .map(|d| d.map_or(Value::WHATEVER, |n| Value::int(n as i64)))
            .collect();
        let shape = if dims.len() == 1 {
            dims[0].clone()
        } else {
            Value::array(dims)
        };
        let mut args = vec![Value::pair("shape".to_string(), shape)];
        if let Some(data) = data {
            args.push(Value::pair("data".to_string(), data));
        }
        self.try_native_array_construct(Symbol::intern("Array"), "Array", type_args, &args)
    }

    /// Tags an array (a jagged array or one of its rows) with the element
    /// type of a typed declaration (`my Int @a[2;*]`).
    // Cost: O(1).
    fn tag_jagged_type(&mut self, array: Value, type_args: &Option<Vec<String>>) -> Value {
        let Some(of) = type_args.as_ref().and_then(|t| t.first()) else {
            return array;
        };
        let ValueView::Array(items, kind) = array.view() else {
            return array;
        };
        let mut data = (**items).clone();
        data.value_type = Some(of.clone());
        Value::array_with_kind(crate::gc::Gc::new(data), kind)
    }

    /// The array a whole-array assignment `@a = ...` leaves in the jagged
    /// array `current` of `shape`: a jagged array of the same shape and
    /// element type, its rows taken from `assigned`, as its declaration takes
    /// them.
    // Cost: O(r + E), r = rows, E = elements assigned.
    pub(crate) fn refill_jagged_array(
        &mut self,
        current: &Value,
        shape: &[Option<usize>],
        assigned: Value,
    ) -> Result<Value, RuntimeError> {
        let type_args = match current.view() {
            ValueView::Array(items, _) => items.value_type.clone().map(|t| vec![t]),
            _ => None,
        };
        self.construct_jagged_array(shape, Some(assigned), &type_args)
    }

    /// The value a whole-row store `@a[i] = value` puts into the jagged array
    /// `container`: a row of the row shape holding `value`'s items. An index
    /// past a first dimension of set length is rakudo's out-of-range error;
    /// `value` itself for any other container or index.
    // Cost: O(E), E = elements of the row.
    pub(crate) fn jagged_row_store_value(
        &mut self,
        container: &Value,
        index: &Value,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let Some(shape) = container.with_deref(jagged_array_shape) else {
            return Ok(value);
        };
        let Some(i) = index_value(index) else {
            return Ok(value);
        };
        if let Some(len) = shape[0]
            && i >= len
        {
            return Err(RuntimeError::new(format!(
                "Index {i} for dimension 1 out of range (must be 0..{})",
                len - 1
            )));
        }
        // The rows before it a `*` first dimension does not have yet are made
        // of the row shape too (the container shares its node, so this
        // writes through to the array).
        let mut node = container.deref_container();
        self.prepare_jagged_write(&mut node, std::slice::from_ref(index), None)?;
        let type_args = container
            .with_deref(|v| match v.view() {
                ValueView::Array(items, _) => items.value_type.clone(),
                _ => None,
            })
            .map(|t| vec![t]);
        self.jagged_row(&shape[1..], Some(value), &type_args)
    }

    /// Makes a jagged array's rows exist and checks its dimensions of set
    /// length before a multi-index write (`@a[1;2] = 7`): an index past one
    /// of them is the error rakudo raises, and a row a `*` dimension does not
    /// have yet is made of the row shape. A no-op for any other array.
    // Cost: O(d + r), d = indices, r = rows made.
    pub(crate) fn prepare_jagged_write(
        &mut self,
        target: &mut Value,
        indices: &[Value],
        value: Option<&Value>,
    ) -> Result<(), RuntimeError> {
        let Some(shape) = target.with_deref(jagged_array_shape) else {
            return Ok(());
        };
        let type_args = target
            .with_deref(|v| match v.view() {
                ValueView::Array(items, _) => items.value_type.clone(),
                _ => None,
            })
            .map(|t| vec![t]);
        self.prepare_jagged_level(target, &shape, indices, 1, &type_args, value)
    }

    // Cost: O(d + r), d = indices, r = rows made.
    fn prepare_jagged_level(
        &mut self,
        target: &mut Value,
        shape: &[Option<usize>],
        indices: &[Value],
        dim: usize,
        type_args: &Option<Vec<String>>,
        value: Option<&Value>,
    ) -> Result<(), RuntimeError> {
        let (Some(index), Some(&len)) = (indices.first(), shape.first()) else {
            return Ok(());
        };
        let Some(i) = index_value(index) else {
            return Ok(());
        };
        if let Some(len) = len
            && i >= len
        {
            return Err(RuntimeError::new(format!(
                "Index {i} for dimension {dim} out of range (must be 0..{})",
                len - 1
            )));
        }
        if shape.len() == 1 {
            return self.prepare_jagged_leaf(target, i, type_args, value);
        }
        let missing = target
            .with_array_mut(|items, _| items.len())
            .map_or(0, |have| (i + 1).saturating_sub(have));
        let mut new_rows = Vec::with_capacity(missing);
        for _ in 0..missing {
            new_rows.push(self.jagged_row(&shape[1..], None, type_args)?);
        }
        let mut result = Ok(());
        target.with_array_mut(|items, _| {
            let items = crate::value::gc_data_mut(items);
            items.extend(new_rows);
            if let Some(row) = items.get_mut(i) {
                result = self.prepare_jagged_level(
                    row,
                    &shape[1..],
                    &indices[1..],
                    dim + 1,
                    type_args,
                    value,
                );
            }
        });
        result
    }
}

impl Interpreter {
    /// The element write at `i` of a typed jagged array's row `row`: the
    /// value must be of the element type, and the slots before it a `*`
    /// row does not have yet hold the type object, not `Any`.
    // Cost: O(g), g = slots padded.
    fn prepare_jagged_leaf(
        &mut self,
        row: &mut Value,
        i: usize,
        type_args: &Option<Vec<String>>,
        value: Option<&Value>,
    ) -> Result<(), RuntimeError> {
        let Some(of) = type_args.as_ref().and_then(|t| t.first()) else {
            return Ok(());
        };
        if let Some(value) = value
            && !value.is_nil()
            && !self.type_matches_value(of, value)
        {
            return Err(crate::runtime::utils::type_check_assignment_typed_error("", of, value));
        }
        let hole = Value::package(Symbol::intern(crate::runtime::types::strip_type_smiley(of).0));
        row.with_array_mut(|items, kind| {
            if *kind == crate::value::ArrayKind::Shaped {
                return;
            }
            let items = crate::value::gc_data_mut(items);
            while items.len() < i {
                items.push(hole.clone());
            }
        });
        Ok(())
    }
}

/// A plain non-negative integer index, `None` for anything else (a Whatever,
/// a range or list slice), which selects within bounds.
// Cost: O(1).
fn index_value(index: &Value) -> Option<usize> {
    match index.deref_container().view() {
        ValueView::Int(i) if i >= 0 => Some(i as usize),
        _ => None,
    }
}

/// The items a row (or the rows) given as `value` holds: an itemized list or
/// array (`$(1, 2)`, a `Scalar`, an element cell) gives its items, as a STORE
/// into the row takes them.
// Cost: O(n), n = items.
fn row_items(value: &Value) -> Vec<Value> {
    let value = value.deref_container();
    let value = match value.view() {
        ValueView::Scalar(inner) => inner.deref_container(),
        _ => value,
    };
    match value.view() {
        ValueView::Array(items, _) => items.to_vec(),
        _ => value_to_list(&value),
    }
}
