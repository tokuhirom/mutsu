//! `Routine.wrap` and `Routine.unwrap`, the bodies of their rows (ADR-11276
//! §9.166): one per code-object kind, a `&name` handle and a `Sub`.

use super::methods_sub::{method_wrap_handle_attrs, routine_unwrap_error};
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// `wrap`/`unwrap` of a `&name` handle: a method regex's own chain, or a
    /// forwarding `Sub` that re-enters named dispatch.
    // Cost: as `sub_wrap` for a forwarding `Sub`; O(w) for a method regex,
    // w = wrappers in its chain.
    pub(crate) fn routine_handle_wrap(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
        is_regex: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, name, package, &args, is_regex);
        if matches!(method, "wrap" | "unwrap") {
            if is_regex {
                let (name, package) = (name.to_string(), package.to_string());
                if method == "wrap" {
                    let Some(wrapper) = args.first().cloned() else {
                        return Some(Err(RuntimeError::new("wrap requires a wrapper argument")));
                    };
                    self.dispatch.wrap_handle_counter += 1;
                    let handle_id = self.dispatch.wrap_handle_counter;
                    self.registry_mut()
                        .push_method_wrap(&package, &name, 0, handle_id, wrapper);
                    let attrs = method_wrap_handle_attrs(&package, &name, 0, handle_id, target);
                    return Some(Ok(Value::make_instance(
                        Symbol::intern("Routine::WrapHandle"),
                        attrs,
                    )));
                }
                if args.is_empty() {
                    return Some(
                        match self.registry_mut().pop_method_wrap(&package, &name, 0) {
                            Some(_) => Ok(Value::TRUE),
                            None => Err(routine_unwrap_error("Cannot unwrap routine: not wrapped")),
                        },
                    );
                }
                let Some(handle_id) = self.extract_wrap_handle_id(&args[0]) else {
                    return Some(Err(routine_unwrap_error(
                        "Cannot unwrap routine: invalid wrap handle",
                    )));
                };
                return Some(
                    if self
                        .registry_mut()
                        .remove_method_wrap(&package, &name, 0, handle_id)
                    {
                        Ok(Value::TRUE)
                    } else {
                        Err(routine_unwrap_error(
                            "Cannot unwrap routine: handle is not active",
                        ))
                    },
                );
            }
            // Routine is a name-only handle, but wraps are stored against a
            // concrete Sub identity.  Use a forwarding Sub which re-enters
            // named dispatch, so a wrapper installed through `&dir` also
            // affects a later `dir(...)` call.
            let dispatch_name =
                if crate::qualified::is_global_package(crate::qualified::known_symbol(package)) {
                    name.to_string()
                } else {
                    crate::qualified::qualified_text(package, name)
                        .as_str()
                        .to_string()
                };
            let forwarding = self.routine_dispatch_sub(package, name, &dispatch_name);
            let data = match forwarding.view() {
                ValueView::Sub(data) => data.clone(),
                _ => unreachable!("routine forwarding value must be a Sub"),
            };
            return self.sub_wrap(&forwarding, &data, method, args);
        }
        None
    }

    /// `wrap`/`unwrap` of a `Sub`: its chain (keyed by name once wrapped), or
    /// a method candidate's chain in the registry.
    // Cost: O(w), w = wrappers in the chain.
    pub(crate) fn sub_wrap(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, data, &args);
        if method == "wrap" {
            // .wrap(&wrapper) — add a wrapper to this sub's wrap chain
            let wrapper = if let Some(w) = args.first() {
                w.clone()
            } else {
                return Some(Err(RuntimeError::new("wrap requires a wrapper argument")));
            };
            self.dispatch.wrap_handle_counter += 1;
            let handle_id = self.dispatch.wrap_handle_counter;
            // Method candidate wrap: if this Sub came from ^lookup().candidates[N],
            // store the wrap chain in method_wrap_chains instead.
            if let Some(ValueView::Str(cls)) = data.env.get("__mutsu_lookup_class").map(Value::view)
                && let Some(ValueView::Str(meth)) =
                    data.env.get("__mutsu_lookup_method").map(Value::view)
                && let Some(idx) = Self::method_sub_wrap_slot(data)
            {
                let (cls, meth) = (cls.to_string(), meth.to_string());
                self.registry_mut()
                    .push_method_wrap(&cls, &meth, idx, handle_id, wrapper);
                let attrs = method_wrap_handle_attrs(&cls, &meth, idx, handle_id, target);
                return Some(Ok(Value::make_instance(
                    Symbol::intern("Routine::WrapHandle"),
                    attrs,
                )));
            }
            // `&foo` creates a fresh Sub, so the chain is keyed by name once a
            // routine is wrapped (see `routine_wrap_key`).
            let (sub_id, func_name) = self.routine_wrap_key(data);
            crate::runtime::cow_table_mut(&mut self.dispatch.wrap_chains)
                .entry(sub_id)
                .or_default()
                .push((handle_id, wrapper));
            self.note_routine_wrap_chain(sub_id, func_name, target);
            // Return a WrapHandle instance
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("sub-id".to_string(), Value::int(sub_id as i64));
            attrs.insert("handle-id".to_string(), Value::int(handle_id as i64));
            // Store a reference to the wrapped sub for .restore()
            attrs.insert("wrapped-sub".to_string(), target.clone());
            return Some(Ok(Value::make_instance(
                Symbol::intern("Routine::WrapHandle"),
                attrs,
            )));
        }
        if method == "unwrap" {
            // Method candidate unwrap: mirrors the `.wrap()` branch above —
            // if this Sub came from `^lookup(...).candidates[N]`, remove from
            // `Registry::method_wrap_chains` instead of the sub-level
            // `wrap_chains` (ADR-0019 E10; previously this branch was never
            // reached for a candidate Sub, so `.unwrap()` fell through to the
            // sub-level logic below, found nothing under `data.id`, and
            // always errored "not wrapped").
            if let Some(ValueView::Str(cls)) = data.env.get("__mutsu_lookup_class").map(Value::view)
                && let Some(ValueView::Str(meth)) =
                    data.env.get("__mutsu_lookup_method").map(Value::view)
                && let Some(idx) = Self::method_sub_wrap_slot(data)
            {
                let (cls, meth) = (cls.to_string(), meth.to_string());
                if args.is_empty() {
                    return Some(
                        match self.registry_mut().pop_method_wrap(&cls, &meth, idx) {
                            Some(_) => Ok(Value::TRUE),
                            None => Err(routine_unwrap_error("Cannot unwrap routine: not wrapped")),
                        },
                    );
                }
                let Some(handle_id) = self.extract_wrap_handle_id(&args[0]) else {
                    return Some(Err(routine_unwrap_error(
                        "Cannot unwrap routine: invalid wrap handle",
                    )));
                };
                return Some(
                    if self
                        .registry_mut()
                        .remove_method_wrap(&cls, &meth, idx, handle_id)
                    {
                        Ok(Value::TRUE)
                    } else {
                        Err(routine_unwrap_error(
                            "Cannot unwrap routine: invalid wrap handle",
                        ))
                    },
                );
            }
            // Look up original sub_id by name, since &foo creates a fresh Sub each time
            let func_name = data
                .env
                .get("__mutsu_wrap_name")
                .map(Value::to_string_value)
                .unwrap_or_else(|| data.name.resolve());
            let sub_id = self
                .dispatch
                .wrap_sub_names
                .iter()
                .find(|(id, n)| **n == func_name && self.wrap_id_is_in_package(**id, data.package))
                .map(|(id, _)| *id)
                .unwrap_or(data.id);
            if args.is_empty() {
                // unwrap with no args on a never-wrapped sub should error
                if !self.dispatch.wrap_chains.contains_key(&sub_id)
                    || self.dispatch.wrap_chains[&sub_id].is_empty()
                {
                    return Some(Err(routine_unwrap_error(
                        "Cannot unwrap routine: not wrapped",
                    )));
                }
                // Pop the outermost wrapper
                if let Some(chain) =
                    crate::runtime::cow_table_mut(&mut self.dispatch.wrap_chains).get_mut(&sub_id)
                {
                    chain.pop();
                    if chain.is_empty() {
                        self.cleanup_wrap_name_entries(sub_id);
                    }
                }
                // Invalidate light-call caches so the sub re-resolves (see wrap).
                self.invalidate_fn_resolution();
                return Some(Ok(Value::TRUE));
            }
            // Extract handle-id from the WrapHandle argument
            let handle = &args[0];
            let handle_id = self.extract_wrap_handle_id(handle);
            let Some(handle_id) = handle_id else {
                return Some(Err(routine_unwrap_error(
                    "Cannot unwrap routine: invalid wrap handle",
                )));
            };
            let chain =
                crate::runtime::cow_table_mut(&mut self.dispatch.wrap_chains).get_mut(&sub_id);
            if let Some(chain) = chain {
                let before_len = chain.len();
                chain.retain(|(hid, _)| *hid != handle_id);
                if chain.len() == before_len {
                    return Some(Err(routine_unwrap_error(
                        "Cannot unwrap routine: invalid wrap handle",
                    )));
                }
                if chain.is_empty() {
                    self.cleanup_wrap_name_entries(sub_id);
                }
                // Invalidate light-call caches so the sub re-resolves (see wrap).
                self.invalidate_fn_resolution();
                return Some(Ok(Value::TRUE));
            }
            return Some(Err(RuntimeError::new(
                "Cannot unwrap a sub that has not been wrapped",
            )));
        }
        None
    }

}
