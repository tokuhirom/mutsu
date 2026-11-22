//! Which values the log may hold: those a decode rebuilds exactly as the
//! body left them (ADR-18012 §2.3, "the log is complete by construction").
//!
//! [`crate::value::PortableValue`] encodes more than this. It is the
//! constant-pool form, where a value is only ever read; a value a `BEGIN` body
//! leaves behind is live state the rest of the module goes on using, so every
//! part of it that a decode would not rebuild has to be absent:
//! - only plain data: numbers, strings, ranges, pairs, type objects, enums,
//!   and `Array`/`Hash` containers (with their declared type, constraints and
//!   default) and the scalar cells inside them;
//! - no container state the encoding drops: native (`NativeCall`) storage, a
//!   shape, element-assignment tracking, a bound slice, object-hash keys, a native-typed cell;
//! - no node reached twice. The decode builds each occurrence fresh, so two
//!   lexicals bound to one array, or one array stored at two keys, would come
//!   back as two arrays, and a write through one would no longer show
//!   through the other.

use crate::value::{Value, ValueView};
use std::collections::HashSet;

/// Whether `values` (taken together, so sharing between them counts) decode
/// back to what they are, sharing no container with `others` either (lexicals
/// the log does not hold, of any kind). `Err` names the first part that would
/// not.
// Cost: O(v + o), v = total size of the values, o = the containers reachable
// from `others` (each node visited once).
pub(crate) fn check(values: &[&Value], others: &[&Value]) -> Result<(), String> {
    let mut seen = HashSet::new();
    for other in others {
        collect(other, &mut seen);
    }
    values.iter().try_for_each(|v| walk(v, &mut seen))
}

/// Record every container reachable from `value` through containers, without
/// judging anything else about it.
// Cost: O(o), o = containers reachable from `value`.
fn collect(value: &Value, seen: &mut HashSet<usize>) {
    match value.view() {
        ValueView::Pair(_, v) => collect(v, seen),
        ValueView::ValuePair(k, v) => {
            collect(k, seen);
            collect(v, seen);
        }
        ValueView::ContainerRef(cell) if seen.insert(crate::gc::Gc::as_ptr(&cell) as usize) => {
            if let Ok(inner) = cell.lock().map(|v| v.clone()) {
                collect(&inner, seen);
            }
        }
        ValueView::Array(data, _) if seen.insert(crate::gc::Gc::as_ptr(&data) as usize) => {
            data.iter().for_each(|v| collect(v, seen));
        }
        ValueView::Hash(data) if seen.insert(crate::gc::Gc::as_ptr(&data) as usize) => {
            data.map.values().for_each(|v| collect(v, seen));
        }
        _ => {}
    }
}

// Cost: O(1).
fn first_visit(seen: &mut HashSet<usize>, ptr: usize) -> Result<(), String> {
    if seen.insert(ptr) {
        Ok(())
    } else {
        Err("a container is reached twice".to_string())
    }
}

// Cost: O(v), v = size of `value`. Recursion depth is the nesting depth of a
// value the body built and the encoder already walked recursively.
fn walk(value: &Value, seen: &mut HashSet<usize>) -> Result<(), String> {
    match value.view() {
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Str(_)
        | ValueView::Bool(_)
        | ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Complex(..)
        | ValueView::Package(_)
        | ValueView::Enum { .. }
        | ValueView::Nil => Ok(()),
        ValueView::GenericRange { start, end, .. } => {
            walk(start, seen)?;
            walk(end, seen)
        }
        ValueView::Pair(_, v) => walk(v, seen),
        ValueView::ValuePair(k, v) => {
            walk(k, seen)?;
            walk(v, seen)
        }
        ValueView::ContainerRef(cell) => {
            first_visit(seen, crate::gc::Gc::as_ptr(&cell) as usize)?;
            if cell.is_native_typed() {
                return Err("a native-typed cell".to_string());
            }
            let inner = cell
                .lock()
                .map_err(|_| "a cell that cannot be read".to_string())?
                .clone();
            walk(&inner, seen)
        }
        ValueView::Array(data, _) => {
            first_visit(seen, crate::gc::Gc::as_ptr(&data) as usize)?;
            let dropped = if data.has_native_backing() {
                Some("native storage")
            } else if data.shape.is_some() {
                Some("a shape")
            } else if data.initialized.is_some() {
                Some("element-assignment tracking")
            } else if data.bound_slice {
                Some("a bound slice")
            } else {
                None
            };
            if let Some(what) = dropped {
                return Err(format!("an array with {what}"));
            }
            if let Some(d) = &data.default {
                walk(d, seen)?;
            }
            data.iter().try_for_each(|v| walk(v, seen))
        }
        ValueView::Hash(data) => {
            first_visit(seen, crate::gc::Gc::as_ptr(&data) as usize)?;
            if data.original_keys.is_some() {
                return Err("a hash with state the log does not keep".to_string());
            }
            if let Some(d) = &data.default {
                walk(d, seen)?;
            }
            data.map.values().try_for_each(|v| walk(v, seen))
        }
        _ => Err(format!(
            "a {} value",
            crate::value::what_type_name(value)
        )),
    }
}
