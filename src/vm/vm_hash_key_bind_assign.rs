//! `%h.BIND-KEY` / `%h.ASSIGN-KEY` element-container helpers for the
//! `CallMethodMut` fast arms in `vm_call_method_mut_ops.rs`.
//!
//! Raku's hash elements are Scalar containers. `BIND-KEY` replaces the
//! element's container with whatever it is handed: a variable's container
//! (writable through either name) or a bare value (no container at all, so a
//! later `ASSIGN-KEY` on that key dies with "Cannot assign to an immutable
//! value"). mutsu models the first as a shared `ContainerRef` cell and the
//! second as a *read-only* cell (`ContainerCell::new_readonly`), so the
//! read-only-ness lives on the entry itself and vanishes with it.

use super::*;

impl Interpreter {
    /// The cell a `%h.BIND-KEY($k, <src>)` installs for key `$k`:
    ///
    /// - a writable source variable (`%h.BIND-KEY($k, $x)`): `$x`'s own cell,
    ///   promoting `$x` to one (recorded in `install` for the caller to write
    ///   back) when it is not boxed yet;
    /// - a value that already is a container (an `is rw` return): that cell;
    /// - anything else — a literal, an expression, or a read-only source such
    ///   as a sigilless `\value` parameter bound to a literal
    ///   (Array::Sparse's `BIND-POS(\value)` forwards exactly that): a
    ///   read-only cell.
    ///
    // Cost: O(1) (one env probe, two readonly-registry probes).
    pub(crate) fn bind_key_source_cell(
        &mut self,
        source_var: Option<&str>,
        value: &Value,
        install: &mut Option<(String, Value)>,
    ) -> crate::gc::Gc<crate::value::ContainerCell> {
        if let Some(var_name) = source_var {
            if let Some(ValueView::ContainerRef(cell)) = self.env().get(var_name).map(Value::view) {
                return cell.clone();
            }
            if !self.name_is_readonly_binding(var_name) {
                let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(value.clone()));
                *install = Some((var_name.to_string(), Value::container_ref(cell.clone())));
                return cell;
            }
        }
        if let ValueView::ContainerRef(cell) = value.view() {
            return cell.clone();
        }
        crate::gc::Gc::new(crate::value::ContainerCell::new_readonly(value.clone()))
    }

    /// Refuse `ASSIGN-KEY` on a key whose entry was bound to a bare value.
    ///
    // Cost: O(1) (one hash probe).
    pub(crate) fn check_assign_key_writable(
        map: &crate::value::HashData,
        key: &str,
    ) -> Result<(), RuntimeError> {
        if let Some(entry) = map.map.get(key)
            && let ValueView::ContainerRef(cell) = entry.view()
            && cell.is_readonly()
        {
            return Err(RuntimeError::immutable_value());
        }
        Ok(())
    }

    /// `%h.ASSIGN-KEY($k, $v)` written into the hash node `target_name` is
    /// bound to, so every alias of that node (`my %s := %!s`, a captured
    /// attribute) sees the store. Returns `Ok(false)` when `target_name` does
    /// not resolve to a hash node, leaving the caller's rebuild path to run.
    ///
    // Cost: O(1) amortized (one hash insert; an object hash also records its
    // key object).
    pub(crate) fn assign_key_in_place(
        &mut self,
        target_name: &str,
        key_arg: &Value,
        value: &Value,
    ) -> Result<bool, RuntimeError> {
        let Some(root) = self.env_root_descended_mut(target_name) else {
            return Ok(false);
        };
        if !matches!(root.view(), ValueView::Hash(..)) {
            return Ok(false);
        }
        root.with_hash_mut(|gc| {
            let data = crate::value::gc_data_mut(gc);
            let object_hash = data.key_type.is_some();
            let key = if object_hash {
                crate::runtime::utils::value_which_key(key_arg)
            } else {
                key_arg.to_string_value()
            };
            Self::check_assign_key_writable(data, &key)?;
            if object_hash {
                data.original_keys
                    .get_or_insert_with(crate::value::ValueMap::default)
                    .insert(key.clone(), key_arg.clone());
            }
            Value::hash_insert_through(&mut data.map, key, value.clone());
            Ok(true)
        })
        .unwrap_or(Ok(false))
    }

    /// Whether `%name{idx} = v` targets an entry bound to a bare value
    /// (`%h.BIND-KEY($k, 42)`), which raku refuses with "Cannot assign to an
    /// immutable value".
    ///
    // Cost: O(1) (one flag load in the common program; else one env probe and
    // one hash probe).
    pub(crate) fn hash_element_is_readonly_bound(&self, var_name: &str, idx: &Value) -> bool {
        if !crate::value::readonly_cells_possible() || !var_name.starts_with('%') {
            return false;
        }
        let Some(container) = self.env().get(var_name).map(Value::deref_container) else {
            return false;
        };
        let ValueView::Hash(map) = container.view() else {
            return false;
        };
        let idx = match idx.view() {
            ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
            _ => idx.clone(),
        };
        let key = if map.key_type.is_some() {
            crate::runtime::utils::value_which_key(&idx)
        } else {
            idx.to_string_value()
        };
        Self::check_assign_key_writable(&map, &key).is_err()
    }
}
