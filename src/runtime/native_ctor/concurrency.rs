//! Native constructors: concurrency family (lifted from `dispatch_new_unallocated`).

use super::CtorCall;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// `Promise`.
    pub(super) fn ctor_promise(
        &mut self,
        _c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path
            // (`try_native_builtin_construct`).
            let explicit = Self::named_value(&args, "scheduler");
            Ok(Value::promise(
                self.new_bound_promise(Symbol::intern("Promise"), explicit),
            ))
    }

    /// `Channel`.
    pub(super) fn ctor_channel(
        &mut self,
        _c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared with the VM's native fast path
            // (`try_native_builtin_construct`).
            Ok(Value::channel(SharedChannel::new()))
    }

    // A Supply cannot be constructed directly (Rakudo throws
    // X::Supply::New); live supplies come from a Supplier, on-demand
    // ones from `Supply.on-demand` / `supply { }`. Internal builders
    // (1.Supply, Supplier.Supply, Proc streams, ...) assemble their
    // Supply instances directly.
    /// `Supply`.
    pub(super) fn ctor_supply(
        &mut self,
        _c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            Err(RuntimeError::supply_new())
    }

    /// `Supplier`, `Supplier::Preserving`.
    pub(super) fn ctor_supplier(
        &mut self,
        c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            // Shared with the VM's native fast path
            // (`try_native_builtin_construct`).
            let mut attrs = HashMap::new();
            attrs.insert("emitted".to_string(), Value::array(Vec::new()));
            attrs.insert("done".to_string(), Value::FALSE);
            attrs.insert(
                "supplier_id".to_string(),
                Value::int(super::native_methods::next_supplier_id() as i64),
            );
            if class_name.resolve() == "Supplier::Preserving" {
                attrs.insert("preserving".to_string(), Value::TRUE);
            }
            Ok(Value::make_instance(*class_name, attrs))
    }

    /// `ThreadPoolScheduler`, `CurrentThreadScheduler`, `Tap`.
    pub(super) fn ctor_threadpoolscheduler(
        &mut self,
        c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            Ok(Value::make_instance(*class_name, HashMap::new()))
    }

    /// `Cancellation`.
    pub(super) fn ctor_cancellation(
        &mut self,
        _c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            Ok(Self::cancellation_instance())
    }

    /// `FakeScheduler`.
    pub(super) fn ctor_fakescheduler(
        &mut self,
        _c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
            // Shared single implementation with the VM's native fast path.
            Ok(Self::build_native_fakescheduler_value())
    }

    /// `Thread`.
    pub(super) fn ctor_thread(
        &mut self,
        c: &CtorCall<'_>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            // `Thread.new(:&code!, :$app_lifetime = False, :$name = '<anon>')`
            // creates the thread WITHOUT starting it -- `.run` does that.
            // The id is allocated here, not at `.run`: rakudo reports a
            // real `.id` on a not-yet-started Thread.
            let mut code = None;
            let mut thread_name = "<anon>".to_string();
            let mut app_lifetime = false;
            for arg in &args {
                match arg.view() {
                    ValueView::Pair(k, v) => match k.as_str() {
                        "code" => code = Some(v.clone()),
                        "name" => thread_name = v.to_string_value(),
                        "app_lifetime" => app_lifetime = v.truthy(),
                        _ => {}
                    },
                    ValueView::Sub(..) | ValueView::WeakSub(..) => code = Some(arg.clone()),
                    _ => {}
                }
            }
            let Some(code) = code else {
                return Err(RuntimeError::new(
                    "Required named parameter 'code' not passed to Thread.new",
                ));
            };
            Ok(Self::new_thread_object(
                *class_name,
                code,
                thread_name,
                app_lifetime,
            ))
    }

    /// `Lock`, `Lock::Async`, `Lock::Soft`.
    pub(super) fn ctor_lock(
        &mut self,
        c: &CtorCall<'_>,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let class_name = &c.class_name;
            // Shared with the VM's native fast path
            // (`try_native_builtin_construct`).
            let mut attrs = HashMap::new();
            let lock_id = super::native_methods::next_lock_id() as i64;
            attrs.insert("lock-id".to_string(), Value::int(lock_id));
            if class_name.resolve() == "Lock::Async" {
                attrs.insert("async".to_string(), Value::TRUE);
            }
            Ok(Value::make_instance(*class_name, attrs))
    }
}
