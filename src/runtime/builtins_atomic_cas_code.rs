//! Code-form CAS on array elements (`cas(@arr[i], &code)` /
//! `cas(@arr[d1;d2], &code)`) over the Track B celled atomic store, plus the
//! typed-constraint enforcement shared by every element-CAS write (roadmap T5:
//! the container's `of`-type must survive celling — `my Int @a` rejects a
//! wrong-typed `cas` swap with X::TypeCheck::Assignment exactly like a plain
//! element assignment, and raku checks the swap value even when the compare
//! fails).

use super::*;

impl Interpreter {
    /// The element-type constraint a CAS write into `name`'s celled atomic
    /// store must satisfy: the declared constraint (`my Int @a` / `my Int %h`),
    /// or the element `value_type` embedded in the container node itself (a
    /// bound alias has no declared constraint on its own name).
    fn atomic_elem_type_constraint(&mut self, name: &str) -> Option<String> {
        if let Some(c) = self.var_type_constraint(name) {
            return Some(c);
        }
        let node = {
            let shared = self.shared_vars.read().unwrap();
            shared
                .get(&format!("__mutsu_atomic_arr::{name}"))
                .or_else(|| shared.get(&format!("__mutsu_atomic_hash::{name}")))
                .or_else(|| shared.get(name))
                .cloned()
        }
        .or_else(|| self.env.get(name).cloned())?;
        match node.view() {
            ValueView::Array(a, _) => a.value_type.clone(),
            ValueView::Hash(h) => h.value_type.clone(),
            _ => None,
        }
    }

    /// Enforce `name`'s element-type constraint on a CAS swap value. Native
    /// int element types (`atomicint`, `int`, ...) accept any Int like the
    /// assignment path's coercion does; `Any`/`Mu` and `Nil` writes pass.
    pub(super) fn check_atomic_elem_type(
        &mut self,
        name: &str,
        new_val: &Value,
    ) -> Result<(), RuntimeError> {
        let Some(constraint) = self.atomic_elem_type_constraint(name) else {
            return Ok(());
        };
        if matches!(constraint.as_str(), "Any" | "Mu") || new_val.is_nil() {
            return Ok(());
        }
        if crate::runtime::native_types::is_native_array_element_type(&constraint) {
            // The assignment path coerces into native elements; only reject
            // values that cannot live in a native slot at all.
            if matches!(new_val.view(), ValueView::Int(_) | ValueView::Num(_)) {
                return Ok(());
            }
        }
        if !self.type_matches_value(&constraint, new_val) {
            return Err(RuntimeError::typecheck_assignment(
                &constraint,
                new_val,
                Some(name),
            ));
        }
        Ok(())
    }

    /// General code-form CAS retry loop over one element cell, shared by the
    /// hash and array forms. The user closure runs OUTSIDE the cell lock (it
    /// re-enters the VM — the Track B re-entrancy rule shared with GC
    /// safepoints, ADR-0001 §3-6): read, compute, type-check, then
    /// compare-and-store under the lock, retrying on interference.
    pub(super) fn cas_cell_code_loop(
        &mut self,
        name: &str,
        cell: &crate::gc::Gc<std::sync::Mutex<Value>>,
        code: &Value,
    ) -> Result<Value, RuntimeError> {
        loop {
            let current = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
            let new_val = {
                let call_args = if let ValueView::Sub(sub) = code.view() {
                    if sub.params.is_empty() {
                        self.env.insert("_".to_string(), current.clone());
                        self.env.insert("$_".to_string(), current.clone());
                        Vec::new()
                    } else {
                        vec![current.clone()]
                    }
                } else {
                    vec![current.clone()]
                };
                self.call_sub_value(code.clone(), call_args, true)?
            };
            self.check_atomic_elem_type(name, &new_val)?;
            let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
            if Self::cas_retry_matches(&current, &guard) {
                *guard = new_val;
                return Ok(Value::NIL);
            }
        }
    }

    /// Code-form CAS on a 1-dim array element: `cas(@arr[idx], &code)`.
    /// Args: [array_name_str, index, code]
    pub(super) fn builtin_cas_array_elem_code(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_cas_array_elem_code requires 3 arguments (array_name, index, code)",
            ));
        }
        let arr_name = args[0].to_string_value();
        let index = match args[1].view() {
            ValueView::Int(i) => i,
            _ => args[1].to_string_value().parse::<i64>().unwrap_or(0),
        };
        let code = args[2].clone();
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        self.init_celled_atomic_store(&atomic_key, &arr_name);
        let cell = self.celled_array_elem(&atomic_key, &arr_name, index);
        let r = self.cas_cell_code_loop(&arr_name, &cell, &code);
        if r.is_ok()
            && let Ok(mut dirty) = self.shared_vars_dirty.write()
        {
            dirty.insert(arr_name.clone());
        }
        r
    }

    /// Code-form CAS on a multidim array element: `cas(@arr[d1;d2], &code)`.
    /// Args: [array_name_str, dimensions_list, code]. Mirrors
    /// `builtin_cas_array_multidim`: the whole read-compute-compare+set cycle
    /// anchors on the top-level slot's cell, with the closure running outside
    /// the lock and the compare against the nested element re-done under it.
    pub(super) fn builtin_cas_array_multidim_code(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 3 {
            return Err(RuntimeError::new(
                "__mutsu_cas_array_multidim_code requires 3 arguments (array_name, dims, code)",
            ));
        }
        let arr_name = args[0].to_string_value();
        let dims: Vec<i64> = match args[1].view() {
            ValueView::Array(elems, ..) => elems
                .iter()
                .map(|v| match v.view() {
                    ValueView::Int(i) => i,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(0),
                })
                .collect(),
            _ => vec![0],
        };
        let code = args[2].clone();
        let atomic_key = format!("__mutsu_atomic_arr::{arr_name}");
        self.init_celled_atomic_store(&atomic_key, &arr_name);
        let cell =
            self.celled_array_elem(&atomic_key, &arr_name, dims.first().copied().unwrap_or(0));
        let inner_dims: &[i64] = if dims.len() > 1 { &dims[1..] } else { &[] };
        if inner_dims.is_empty() {
            let r = self.cas_cell_code_loop(&arr_name, &cell, &code);
            if r.is_ok()
                && let Ok(mut dirty) = self.shared_vars_dirty.write()
            {
                dirty.insert(arr_name.clone());
            }
            return r;
        }
        loop {
            let current = {
                let guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                Self::multidim_get(&guard, inner_dims)
            };
            let new_val = {
                let call_args = if let ValueView::Sub(sub) = code.view() {
                    if sub.params.is_empty() {
                        self.env.insert("_".to_string(), current.clone());
                        self.env.insert("$_".to_string(), current.clone());
                        Vec::new()
                    } else {
                        vec![current.clone()]
                    }
                } else {
                    vec![current.clone()]
                };
                self.call_sub_value(code.clone(), call_args, true)?
            };
            self.check_atomic_elem_type(&arr_name, &new_val)?;
            let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
            if Self::cas_retry_matches(&current, &Self::multidim_get(&guard, inner_dims)) {
                *guard = Self::multidim_set(&guard, inner_dims, new_val);
                if let Ok(mut dirty) = self.shared_vars_dirty.write() {
                    dirty.insert(arr_name.clone());
                }
                return Ok(Value::NIL);
            }
        }
    }
}
