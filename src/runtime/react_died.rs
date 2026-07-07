//! Helpers for wrapping errors in `X::React::Died` when a `react` block
//! encounters an unhandled exception from a supply.

use super::subtest::ReactSubscription;
use super::*;
use crate::runtime::native_methods::take_supply_channel;
use crate::symbol::Symbol;
use std::collections::HashMap;

impl Interpreter {
    /// Wrap a `RuntimeError` in `X::React::Died`.
    /// The resulting exception reports as `X::React::Died` and includes
    /// the original error message and backtrace in its gist.
    pub(crate) fn wrap_react_died(inner: RuntimeError) -> RuntimeError {
        let original_message = inner.message.clone();
        let backtrace_str = inner.backtrace().unwrap_or_default().to_string();
        let mut gist = format!(
            "A react block:\n\nDied because of the exception:\n    {}",
            original_message
        );
        if !backtrace_str.is_empty() {
            gist.push_str(&format!("\n{}", backtrace_str));
        }
        let mut attrs = HashMap::new();
        attrs.insert("message".to_string(), Value::str(gist.clone()));
        attrs.insert("original-message".to_string(), Value::str(original_message));
        if let Some(ref ex) = inner.exception {
            attrs.insert("exception".to_string(), *ex.clone());
        }
        let exception = Value::make_instance(Symbol::intern("X::React::Died"), attrs);
        let mut err = RuntimeError::new(gist);
        err.exception = Some(Box::new(exception));
        err
    }

    /// Wrap a `RuntimeError` in `X::React::Died` if not already wrapped.
    pub(crate) fn wrap_react_died_if_needed(err: RuntimeError) -> RuntimeError {
        if let Some(ref ex) = err.exception
            && let ValueView::Instance { class_name, .. } = ex.as_ref().view()
            && class_name.resolve() == "X::React::Died"
        {
            return err;
        }
        Self::wrap_react_died(err)
    }

    /// Check if a value is a subscription registration from `whenever` inside `supply`.
    pub(crate) fn is_supply_subscription_registration(value: &Value) -> bool {
        if let ValueView::Array(items, ..) = value.view()
            && items.len() >= 2
        {
            matches!(
                items[0].view(),
                ValueView::Instance { class_name, .. }
                    if class_name == "Supply"
                        || class_name == "Supplier"
                        || class_name == "Supplier::Preserving"
            ) || matches!(
                items[0].view(),
                ValueView::Promise(_) | ValueView::Channel(_)
            )
        } else {
            false
        }
    }

    /// Convert a subscription registration Value into a ReactSubscription.
    /// Used when `supply { whenever ... { ... } }` creates inner subscriptions
    /// that need to be processed by the outer react's event loop.
    pub(crate) fn value_to_react_subscription(
        &mut self,
        sub_val: &Value,
    ) -> Option<ReactSubscription> {
        let ValueView::Array(items, ..) = sub_val.view() else {
            return None;
        };
        if items.len() < 2 {
            return None;
        }
        let source = &items[0];
        let callback = items[1].clone();
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = source.view()
        {
            if class_name != "Supply" {
                return None;
            }
            let supply_id = self.resolve_supply_channel_id_for_react(&(attributes).as_map());
            let is_lines = matches!(
                attributes.as_map().get("is_lines").map(Value::view),
                Some(ValueView::Bool(true))
            );
            let head_limit = Self::extract_supply_head_limit(&(attributes).as_map());
            if let Some(sid) = supply_id
                && let Some(rx) = take_supply_channel(sid)
            {
                let last_cbs = items
                    .get(2)
                    .and_then(Self::react_value_array_items)
                    .unwrap_or_default();
                let quit_cbs = items
                    .get(3)
                    .and_then(Self::react_value_array_items)
                    .unwrap_or_default();
                return Some(ReactSubscription {
                    receiver: Some(rx),
                    close_callbacks: Self::extract_supply_on_close_cbs(&(attributes).as_map()),
                    last_callbacks: last_cbs,
                    quit_callbacks: quit_cbs,
                    is_lines,
                    head_limit,
                    ..ReactSubscription::new(callback)
                });
            }
            if let Some(ValueView::Int(sid)) =
                attributes.as_map().get("supplier_id").map(Value::view)
            {
                let last_cbs = items
                    .get(2)
                    .and_then(Self::react_value_array_items)
                    .unwrap_or_default();
                let quit_cbs = items
                    .get(3)
                    .and_then(Self::react_value_array_items)
                    .unwrap_or_default();
                return Some(ReactSubscription {
                    supplier_id: Some(sid as u64),
                    close_callbacks: Self::extract_supply_on_close_cbs(&(attributes).as_map()),
                    last_callbacks: last_cbs,
                    quit_callbacks: quit_cbs,
                    is_lines,
                    head_limit,
                    ..ReactSubscription::new(callback)
                });
            }
        }
        None
    }

    // Helpers that duplicate subtest.rs private helpers (to avoid
    // visibility issues).

    fn extract_supply_head_limit(attributes: &HashMap<String, Value>) -> Option<usize> {
        if let Some(ValueView::Int(n)) = attributes.get("head_limit").map(Value::view) {
            Some(n as usize)
        } else {
            None
        }
    }

    fn extract_supply_on_close_cbs(attributes: &HashMap<String, Value>) -> Vec<Value> {
        if let Some(ValueView::Array(callbacks, ..)) =
            attributes.get("on_close_callbacks").map(Value::view)
        {
            callbacks.to_vec()
        } else {
            Vec::new()
        }
    }

    fn react_value_array_items(value: &Value) -> Option<Vec<Value>> {
        if let ValueView::Array(items, ..) = value.view() {
            Some(items.to_vec())
        } else {
            None
        }
    }

    fn resolve_supply_channel_id_for_react(
        &self,
        attributes: &HashMap<String, Value>,
    ) -> Option<u64> {
        if let Some(ValueView::Int(parent_id)) = attributes.get("parent_supply_id").map(Value::view)
        {
            return Some(parent_id as u64);
        }
        if let Some(ValueView::Int(id)) = attributes.get("supply_id").map(Value::view) {
            return Some(id as u64);
        }
        None
    }
}
