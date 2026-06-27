use super::*;
use crate::symbol::Symbol;
use crate::value::InstanceAttrs;
use std::sync::Arc;

impl Interpreter {
    pub(crate) fn call_proxy_callback(
        &mut self,
        callback: &Value,
        args: Vec<Value>,
        instance_attrs: &HashMap<String, Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        if let Value::Sub(data) = callback {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            // Merge captured env. For user variables that already exist in the
            // current env, prefer the current (live) value over the captured
            // snapshot. This approximates Raku's shared-binding closure semantics
            // for Proxy callbacks (FETCH should see changes made by STORE).
            // Internal/special variables always use the captured value so that
            // the callback's lexical context is properly restored.
            for (k, v) in &data.env {
                let is_user_var = !k.starts_with("!")
                    && !k.starts_with(".")
                    && !k.starts_with("*")
                    && !k.starts_with("?")
                    && !k.starts_with("__")
                    && *k != "self"
                    && *k != "_";
                if is_user_var && new_env.contains_key_sym(*k) {
                    // Keep current env value (live binding)
                } else {
                    new_env.insert_sym(*k, v.clone());
                }
            }
            // Override !attr bindings with current instance attributes
            for (attr_name, attr_val) in instance_attrs {
                new_env.insert(format!("!{}", attr_name), attr_val.clone());
                new_env.insert(format!(".{}", attr_name), attr_val.clone());
            }
            self.env = new_env;
            if let Err(e) = self.bind_function_args_values(&data.param_defs, &data.params, &args) {
                self.env = saved_env;
                return Err(e);
            }
            let result = self.run_block(&data.body);
            let implicit_return = self.env.get("_").cloned();
            // Read back !attr changes
            let mut updated_attrs = instance_attrs.clone();
            for attr_name in instance_attrs.keys() {
                if let Some(val) = self.env.get(&format!("!{}", attr_name)) {
                    updated_attrs.insert(attr_name.clone(), val.clone());
                }
            }
            // Propagate outer variable changes (e.g., our variables, closured scalars).
            // Only propagate values that were genuinely modified by the callback body,
            // not values merely loaded from the captured env. This prevents stale
            // captured env entries (e.g. an old instance snapshot) from overwriting
            // current env values.
            let mut restored = saved_env;
            for (k, v) in &self.env {
                if restored.contains_key_sym(*k)
                    && !k.starts_with("!")
                    && !k.starts_with(".")
                    && *k != "self"
                    && *k != "_"
                {
                    // Skip if the value is unchanged from the captured env —
                    // it was merely loaded, not modified by the callback.
                    if let Some(captured_v) = data.env.get_sym(*k)
                        && v == captured_v
                    {
                        continue;
                    }
                    restored.insert_sym(*k, v.clone());
                }
            }
            self.env = restored;
            let value = match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                Err(e) => Err(e),
                Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
            }?;
            Ok((value, updated_attrs))
        } else {
            // Non-sub callback
            let result = self.call_sub_value(callback.clone(), args, false)?;
            Ok((result, instance_attrs.clone()))
        }
    }

    /// Call Proxy FETCH and return the fetched value, propagating attribute updates to the instance.
    pub(crate) fn proxy_fetch(
        &mut self,
        fetcher: &Value,
        target_var: Option<&str>,
        class_name: &str,
        attributes: &HashMap<String, Value>,
        target_id: u64,
    ) -> Result<Value, RuntimeError> {
        let proxy_val = Value::Proxy {
            fetcher: Box::new(fetcher.clone()),
            storer: Box::new(Value::Nil),
            subclass: None,
            decontainerized: false,
        };
        let (result, _updated) = self.call_proxy_callback(fetcher, vec![proxy_val], attributes)?;
        // For FETCH we don't propagate attribute changes (reads shouldn't mutate)
        let _ = target_var;
        let _ = class_name;
        let _ = target_id;
        Ok(result)
    }

    /// Call Proxy STORE with a new value, propagating attribute updates to the instance.
    ///
    /// `attributes` is the (post-method-run) snapshot fed to the STORE callback;
    /// `attrs_cell` is the receiver instance's live shared cell that the updated
    /// attributes are written back into in place (Phase 3 registry-removal).
    pub(crate) fn proxy_store(
        &mut self,
        storer: &Value,
        target_var: Option<&str>,
        class_name: Symbol,
        attributes: &HashMap<String, Value>,
        attrs_cell: &Arc<InstanceAttrs>,
        new_value: Value,
    ) -> Result<Value, RuntimeError> {
        let proxy_val = Value::Proxy {
            fetcher: Box::new(Value::Nil),
            storer: Box::new(storer.clone()),
            subclass: None,
            decontainerized: false,
        };
        let (_result, updated) =
            self.call_proxy_callback(storer, vec![proxy_val, new_value.clone()], attributes)?;
        // Propagate attribute changes back to the instance's live cell.
        if let Some(var_name) = target_var {
            attrs_cell.commit_attrs(updated);
            self.env.insert(
                var_name.to_string(),
                Value::instance_sharing_cell(attrs_cell, class_name, attrs_cell.instance_id()),
            );
        }
        // Assignment returns the assigned value, not the STORE callback's return value
        Ok(new_value)
    }
}
