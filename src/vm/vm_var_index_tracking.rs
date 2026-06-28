//! Bound-index / element-share / deleted-index bookkeeping helpers
//! split from `vm_var_ops` (§7-8 file split).
use super::*;
use std::sync::Arc;

impl Interpreter {
    pub(super) fn encode_bound_index(idx: &Value) -> String {
        match idx {
            Value::Array(indices, ..) => indices
                .iter()
                .map(|v| v.to_string_value())
                .collect::<Vec<_>>()
                .join(";"),
            _ => idx.to_string_value(),
        }
    }

    pub(super) fn is_bound_index(&self, var_name: &str, encoded: &str) -> bool {
        let key = format!("__mutsu_bound_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.env().get(&key) {
            map.contains_key(encoded)
        } else {
            false
        }
    }

    pub(super) fn mark_bound_index(&mut self, var_name: &str, encoded: String) {
        let key = format!("__mutsu_bound_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.env_mut().get_mut(&key) {
            Arc::make_mut(map).insert(encoded, Value::Bool(true));
            return;
        }
        let mut map = std::collections::HashMap::new();
        map.insert(encoded, Value::Bool(true));
        self.env_mut().insert(key, Value::hash(map));
    }

    /// Slice 2b (`docs/scalar-array-sharing.md`): mark element `encoded` of
    /// `var_name` as a `=`-reference share (`@aoa[i] = @row`). Such an element
    /// holds a shared `ContainerRef` cell, but unlike a `:=` bind a later
    /// non-share reassignment (`@aoa[i] = 42`) must REPLACE the slot rather than
    /// write through the cell. The guards at the element-write chokepoints
    /// consult this marker to choose replace vs write-through.
    pub(super) fn mark_element_share(&mut self, var_name: &str, encoded: String) {
        let key = format!("__mutsu_elem_share::{}", var_name);
        if let Some(Value::Hash(map)) = self.env_mut().get_mut(&key) {
            Arc::make_mut(map).insert(encoded, Value::Bool(true));
            return;
        }
        let mut map = std::collections::HashMap::new();
        map.insert(encoded, Value::Bool(true));
        self.env_mut().insert(key, Value::hash(map));
        self.array_share_active = true;
    }

    pub(super) fn is_element_share(&self, var_name: &str, encoded: &str) -> bool {
        let key = format!("__mutsu_elem_share::{}", var_name);
        if let Some(Value::Hash(map)) = self.env().get(&key) {
            map.contains_key(encoded)
        } else {
            false
        }
    }

    pub(super) fn clear_element_share(&mut self, var_name: &str, encoded: &str) {
        let key = format!("__mutsu_elem_share::{}", var_name);
        if let Some(Value::Hash(map)) = self.env_mut().get_mut(&key) {
            Arc::make_mut(map).remove(encoded);
        }
    }

    /// Remove a bound-index marker (e.g. after splice breaks the binding).
    pub(super) fn remove_bound_index(&mut self, var_name: &str, encoded: &str) {
        let key = format!("__mutsu_bound_index::{}", var_name);
        if let Some(Value::Hash(map)) = self.env_mut().get_mut(&key) {
            Arc::make_mut(map).remove(encoded);
        }
    }

    pub(super) fn mark_initialized_index(&mut self, var_name: &str, encoded: String) {
        // Assigning a slot clears any deleted-index marker so subsequent
        // `:exists` checks report the slot as present again.
        {
            let deleted_key = format!("__mutsu_deleted_index::{}", var_name);
            if let Some(Value::Hash(map)) = self.env_mut().get_mut(&deleted_key) {
                Arc::make_mut(map).remove(&encoded);
            }
        }
        // Record the assigned index in the array's *embedded* `initialized`
        // set so it travels with the value across scopes/closures/method calls
        // (the former env-name-keyed side table was scoped to the assigning
        // frame and lost on scope exit). Only array indices (plain integers)
        // are tracked — hash existence is determined by key presence.
        let Ok(idx) = encoded.parse::<usize>() else {
            return;
        };
        // Mutate the array in place when it is shared by-ref through a scalar
        // ($) container, mirroring the assignment path's `use_inplace` choice
        // (vm_var_assign_index_named). Using `Arc::make_mut` here would clone a
        // shared array and sever the by-ref alias, so a subsequent `.push` on
        // the original would be lost (t/array-push-byref-coherence).
        let use_inplace = !var_name.starts_with('@');
        if let Some(Value::Array(items, _)) = self.env_root_descended_mut(var_name) {
            let data: &mut crate::value::ArrayData = if use_inplace && Arc::strong_count(items) > 1
            {
                // SAFETY: aliased in-place mutation of a shared array; same
                // contract as the assignment site's `arc_contents_mut` use.
                unsafe { crate::value::arc_contents_mut(items) }
            } else {
                Arc::make_mut(items)
            };
            data.initialized
                .get_or_insert_with(std::collections::HashSet::new)
                .insert(idx);
        }
    }

    /// Mark the given indices as deleted. `:exists` on an array consults
    /// this set so that a slot holding a type-object hole can be reported
    /// as missing even though the slot value is not `Nil`.
    pub(super) fn mark_deleted_indices(&mut self, var_name: &str, idx: &Value) {
        let key = format!("__mutsu_deleted_index::{}", var_name);
        let map = if let Some(Value::Hash(map)) = self.env_mut().get_mut(&key) {
            Arc::make_mut(map)
        } else {
            let m = std::collections::HashMap::new();
            self.env_mut().insert(key.clone(), Value::hash(m));
            match self.env_mut().get_mut(&key) {
                Some(Value::Hash(map)) => Arc::make_mut(map),
                _ => return,
            }
        };
        Self::mark_index_entries(map, idx);
    }

    #[allow(dead_code)]
    pub(super) fn unmark_deleted_indices(&mut self, var_name: &str, idx: &Value) {
        let key = format!("__mutsu_deleted_index::{}", var_name);
        let Some(Value::Hash(map)) = self.env_mut().get_mut(&key) else {
            return;
        };
        let m = Arc::make_mut(map);
        Self::unmark_index_entries(m, idx);
    }

    pub(super) fn is_deleted_index(&self, var_name: &str, idx: i64) -> bool {
        let key = format!("__mutsu_deleted_index::{}", var_name);
        matches!(
            self.env().get(&key),
            Some(Value::Hash(map)) if map.contains_key(&idx.to_string())
        )
    }

    fn mark_index_entries(map: &mut std::collections::HashMap<String, Value>, idx: &Value) {
        match idx {
            Value::Array(items, ..) => {
                for item in items.iter() {
                    Self::mark_index_entries(map, item);
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                let expanded = crate::runtime::utils::value_to_list(idx);
                for item in &expanded {
                    Self::mark_index_entries(map, item);
                }
            }
            Value::Int(i) => {
                map.insert(i.to_string(), Value::Bool(true));
            }
            Value::Num(f) => {
                map.insert((*f as i64).to_string(), Value::Bool(true));
            }
            _ => {
                map.insert(idx.to_string_value(), Value::Bool(true));
            }
        }
    }

    /// Remove deleted indices from the bound-index tracking set.
    /// This must be called after array element deletion to sever bindings.
    pub(super) fn unmark_bound_indices(&mut self, var_name: &str, idx: &Value) {
        let key = format!("__mutsu_bound_index::{}", var_name);
        let Some(Value::Hash(map)) = self.env_mut().get_mut(&key) else {
            return;
        };
        let m = Arc::make_mut(map);
        Self::unmark_index_entries(m, idx);
    }

    /// Remove deleted indices from the initialized-index tracking set.
    /// This must be called after array element deletion and before
    /// `trim_trailing_array_holes` so that deleted slots are recognized
    /// as holes and can be trimmed.
    pub(super) fn unmark_initialized_indices(&mut self, var_name: &str, idx: &Value) {
        // Collect the integer indices addressed by `idx` (scalar, slice, or
        // range) and drop them from the array's embedded `initialized` set so
        // the deleted slots are recognized as holes and can be trimmed.
        let mut to_remove: Vec<usize> = Vec::new();
        Self::collect_usize_indices(idx, &mut to_remove);
        if to_remove.is_empty() {
            return;
        }
        if let Some(Value::Array(items, _)) = self.env_root_descended_mut(var_name)
            && let Some(set) = Arc::make_mut(items).initialized.as_mut()
        {
            for i in to_remove {
                set.remove(&i);
            }
        }
    }

    /// Flatten a subscript index `Value` (scalar / array slice / range) into the
    /// list of non-negative integer array indices it addresses.
    fn collect_usize_indices(idx: &Value, out: &mut Vec<usize>) {
        match idx {
            Value::Array(items, ..) => {
                for item in items.iter() {
                    Self::collect_usize_indices(item, out);
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                for item in &crate::runtime::utils::value_to_list(idx) {
                    Self::collect_usize_indices(item, out);
                }
            }
            Value::Int(i) if *i >= 0 => out.push(*i as usize),
            Value::Num(f) if *f >= 0.0 => out.push(*f as usize),
            _ => {
                if let Ok(i) = idx.to_string_value().parse::<usize>() {
                    out.push(i);
                }
            }
        }
    }

    fn unmark_index_entries(map: &mut std::collections::HashMap<String, Value>, idx: &Value) {
        match idx {
            Value::Array(items, ..) => {
                for item in items.iter() {
                    Self::unmark_index_entries(map, item);
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                let expanded = crate::runtime::utils::value_to_list(idx);
                for item in &expanded {
                    Self::unmark_index_entries(map, item);
                }
            }
            Value::Int(i) => {
                map.remove(&i.to_string());
            }
            Value::Num(f) => {
                map.remove(&(*f as i64).to_string());
            }
            _ => {
                map.remove(&idx.to_string_value());
            }
        }
    }
}
