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
    ///
    /// FETCH is called with the Proxy itself, as in Rakudo, so a Proxy
    /// subclass's FETCH sees its own attributes (a native positional
    /// reference reads its element through them, #11209).
    pub(crate) fn proxy_fetch(
        &mut self,
        proxy: &Value,
        target_var: Option<&str>,
        class_name: &str,
        attributes: &AttrMap,
        target_id: u64,
    ) -> Result<Value, RuntimeError> {
        let ValueView::Proxy {
            fetcher,
            storer,
            subclass,
            ..
        } = proxy.view()
        else {
            return Ok(proxy.clone());
        };
        let fetcher = Value::clone(fetcher);
        // Handed over as its `.VAR` (decontainerized), so binding it to the
        // FETCH's invocant does not FETCH it again.
        let invocant = Value::proxy_parts(
            fetcher.clone(),
            Value::clone(storer),
            subclass.clone(),
            true,
        );
        let (result, _updated) = self.call_proxy_callback(&fetcher, vec![invocant], attributes)?;
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
        let (_result, _unchanged) =
            self.call_proxy_callback(storer, vec![proxy_val, new_value.clone()], attributes)?;
        // Propagate attribute changes back to the instance's live cell.
        if let Some(var_name) = target_var {
            // The callback mutates captured instance cells directly and
            // `call_proxy_callback` hands back the very snapshot it was given,
            // so there is nothing to commit: replaying that (by now stale)
            // snapshot would roll back whatever the STORE body just wrote to
            // this instance. (It used to compare the maps first, but a map
            // holding a `Proxy` never compares equal to its own clone.)
            self.env.insert(
                var_name.to_string(),
                Value::instance_sharing_cell(attrs_cell, class_name, attrs_cell.instance_id()),
            );
        }
        // Assignment returns the assigned value, not the STORE callback's return value
        Ok(new_value)
    }
}
