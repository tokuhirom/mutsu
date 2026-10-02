use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::InstanceAttrs;
use crate::value::ValueView;

impl Interpreter {
    pub(crate) fn call_proxy_callback(
        &mut self,
        callback: &Value,
        args: Vec<Value>,
        instance_attrs: &AttrMap,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        // A Proxy callback is an ordinary escaping closure: its compiled body
        // owns the lexical/upvalue bindings captured at construction.
        if let ValueView::Sub(data) = callback.view()
            && let Some(compiled_code) = &data.compiled_code
        {
            let empty_fns = crate::opcode::CompiledFns::default();
            let compiled_fns = data.compiled_fns.as_deref().unwrap_or(&empty_fns);
            let result = self.call_compiled_closure(&data, compiled_code, args, compiled_fns)?;
            return Ok((result, instance_attrs.clone()));
        }
        // Anything else -- including a named `sub STORE(...)` passed as
        // `:&STORE`, which carries no `compiled_code` of its own -- goes through
        // the ordinary sub-call dispatch. A by-name AST re-run of the body here
        // used to do nothing at all for the named-sub shape (#10811).
        let result = self.call_sub_value(callback.clone(), args, false)?;
        Ok((result, instance_attrs.clone()))
    }

    /// Call Proxy FETCH and return the fetched value, propagating attribute updates to the instance.
    pub(crate) fn proxy_fetch(
        &mut self,
        fetcher: &Value,
        target_var: Option<&str>,
        class_name: &str,
        attributes: &AttrMap,
        target_id: u64,
    ) -> Result<Value, RuntimeError> {
        let proxy_val = Value::proxy_parts(fetcher.clone(), Value::NIL, None, false);
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
        attributes: &AttrMap,
        attrs_cell: &crate::gc::Gc<InstanceAttrs>,
        new_value: Value,
    ) -> Result<Value, RuntimeError> {
        let proxy_val = Value::proxy_parts(Value::NIL, storer.clone(), None, false);
        let (_result, updated) =
            self.call_proxy_callback(storer, vec![proxy_val, new_value.clone()], attributes)?;
        // Propagate attribute changes back to the instance's live cell.
        if let Some(var_name) = target_var {
            // A VM-dispatched callback mutates captured instance cells directly.
            // It returns the input snapshot here because it has no AST-env
            // attribute overlay to merge; committing that unchanged snapshot
            // would roll the just-completed STORE back. The legacy callback path
            // still returns a changed map when it performed an overlay write.
            if &updated != attributes {
                attrs_cell.commit_attrs(updated);
            }
            self.env.insert(
                var_name.to_string(),
                Value::instance_sharing_cell(attrs_cell, class_name, attrs_cell.instance_id()),
            );
        }
        // Assignment returns the assigned value, not the STORE callback's return value
        Ok(new_value)
    }
}
