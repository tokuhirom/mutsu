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
    /// Replace an existing scalar held by a metadata-bearing bound element
    /// with the aggregate needed for a chained lvalue, checking the source
    /// cell before the chain mutates it.
    // Cost: O(1) plus the source cell's type check.
    pub(crate) fn prepare_bound_cell_for_chained_store(
        &mut self,
        slot: &mut Value,
        positional: bool,
    ) -> Result<(), RuntimeError> {
        let ValueView::ContainerRef(cell) = slot.view() else {
            return Ok(());
        };
        let cell = cell.clone();
        if crate::value::lookup_cell_constraint(&cell).is_none() && cell.default_value().is_none() {
            return Ok(());
        }
        let current = cell.lock().unwrap().clone();
        let is_aggregate = matches!(
            (current.deref_container().view(), positional),
            (ValueView::Array(..), true) | (ValueView::Hash(..), false)
        );
        if !is_aggregate {
            let aggregate = (if positional {
                Value::real_array_unassigned(Vec::new())
            } else {
                Value::hash(crate::value::ValueMap::default())
            })
            .itemize_for_element_store();
            let aggregate = self.element_store_through_cell(&cell, aggregate)?;
            *cell.lock().unwrap() = aggregate;
        }
        Ok(())
    }

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

    /// The fresh cell a `:=` element bind (`%h<k> := $y`, `@a[0] := $y`)
    /// promotes the not-yet-boxed source variable `source_name` into, holding
    /// its current `value`. The cell becomes the variable's container, so it
    /// carries the variable's declared `of` constraint and `is default`: a
    /// `Nil` stored through any alias of it then resets to that default or
    /// type object rather than `Any` (#11618).
    ///
    // Cost: O(1) (two name-keyed metadata probes).
    pub(crate) fn promote_bind_source_cell(
        &mut self,
        source_name: &str,
        value: Value,
    ) -> crate::gc::Gc<crate::value::ContainerCell> {
        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(value));
        self.register_container_cell_constraint_for_name(
            &Value::container_ref(cell.clone()),
            source_name,
        );
        cell
    }

    /// The metadata-bearing cell at one actual aggregate slot. `positional`
    /// comes from the subscript syntax, so scalar variables holding Arrays or
    /// Hashes use the same lookup as sigiled variables.
    // Cost: O(1) plus index/key conversion.
    pub(crate) fn element_cell_with_metadata(
        &self,
        container: &Value,
        idx: &Value,
        positional: bool,
    ) -> Option<crate::gc::Gc<crate::value::ContainerCell>> {
        if !crate::value::cell_metadata_possible() {
            return None;
        }
        let idx = match idx.view() {
            ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
            ValueView::Array(..) => return None,
            _ => idx.clone(),
        };
        let slot = match container.view() {
            ValueView::Array(items, _) if positional => {
                items.get(Self::index_to_usize(&idx)?).cloned()
            }
            ValueView::Hash(map) if !positional => {
                let key = if map.key_type.is_some() {
                    crate::runtime::utils::value_which_key(&idx)
                } else {
                    idx.to_string_value()
                };
                map.map.get(&key).cloned()
            }
            _ => None,
        }?;
        let ValueView::ContainerRef(cell) = slot.view() else {
            return None;
        };
        (crate::value::lookup_cell_constraint(&cell).is_some() || cell.default_value().is_some())
            .then(|| cell.clone())
    }

    /// The value a plain element assignment of `val` through `cell` stores. An
    /// element `:=`-bound to a variable IS that variable's container, so the
    /// container's own `is default` and `of` constraint decide it, not the
    /// aggregate's: `Nil` resets to the cell's default (or its type object) and
    /// anything else is type-checked against the cell (#11810).
    ///
    // Cost: O(1) plus the type check of `val` against the cell's constraint.
    pub(crate) fn element_store_through_cell(
        &mut self,
        cell: &crate::gc::Gc<crate::value::ContainerCell>,
        val: Value,
    ) -> Result<Value, RuntimeError> {
        let val = if val.is_nil() {
            self.cell_nil_reset_value(cell)
        } else {
            val
        };
        self.coerce_container_cell_store(cell, val)
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

    /// Whether `@name[idx] = v` targets an element bound to a bare value
    /// (`@a.BIND-POS($i, 42)`, #10924) — the positional twin of
    /// [`Self::hash_element_is_readonly_bound`].
    ///
    // Cost: O(1) (one flag load in the common program; else one env probe and
    // one element read).
    pub(crate) fn array_element_is_readonly_bound(&self, var_name: &str, idx: &Value) -> bool {
        if !crate::value::readonly_cells_possible() || !var_name.starts_with('@') {
            return false;
        }
        let Some(container) = self.env().get(var_name).map(Value::deref_container) else {
            return false;
        };
        let ValueView::Array(items, _) = container.view() else {
            return false;
        };
        let idx = match idx.view() {
            ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
            _ => idx.clone(),
        };
        let Some(i) = (match idx.view() {
            ValueView::Int(i) => usize::try_from(i).ok(),
            _ => None,
        }) else {
            return false;
        };
        matches!(
            items.get(i).map(Value::view),
            Some(ValueView::ContainerRef(cell)) if cell.is_readonly()
        )
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
