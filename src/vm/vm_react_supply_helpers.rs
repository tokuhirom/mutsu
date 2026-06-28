//! react/supply replay + inner-supply drive helpers,
//! split from `vm_react_loop` (§7-8 file split).
use super::*;
use crate::runtime::native_methods::next_supplier_id;
use crate::runtime::subtest::{ReactSubscription, StreamConsumer};

impl Interpreter {
    pub(super) fn extract_head_limit(attributes: &HashMap<String, Value>) -> Option<usize> {
        if let Some(Value::Int(n)) = attributes.get("head_limit") {
            Some(*n as usize)
        } else {
            None
        }
    }

    pub(super) fn extract_supply_on_close_callbacks(
        attributes: &HashMap<String, Value>,
    ) -> Vec<Value> {
        if let Some(Value::Array(callbacks, ..)) = attributes.get("on_close_callbacks") {
            callbacks.to_vec()
        } else {
            Vec::new()
        }
    }

    pub(super) fn run_react_close_callbacks(&mut self, react_subs: &[ReactSubscription]) {
        for sub in react_subs {
            for callback in &sub.close_callbacks {
                let _ = self.call_react_callback(&callback.clone(), Vec::new());
            }
        }
    }

    /// Resolve the supply_id to use for channel lookup.
    /// For a "lines" supply, use the parent_supply_id.
    pub(super) fn resolve_supply_channel_id(
        &self,
        attributes: &HashMap<String, Value>,
    ) -> Option<u64> {
        // If this is a "lines" supply, use the parent's supply_id
        if let Some(Value::Int(parent_id)) = attributes.get("parent_supply_id") {
            return Some(*parent_id as u64);
        }
        // Otherwise use this supply's own supply_id
        if let Some(Value::Int(id)) = attributes.get("supply_id") {
            return Some(*id as u64);
        }
        None
    }

    /// Replay static supply values (non-streaming supplies)
    /// Replay a static supply's values through a `whenever` callback, honouring
    /// the loop-control signals the body may raise: `done` ends the react
    /// (returns `Ok(true)`), `next` skips to the next value, `last` stops the
    /// whenever early. This whenever's LAST phasers are fired here — with the
    /// triggering value as topic when stopped via `last`, otherwise with no
    /// topic on natural completion. Returns `Ok(true)` iff `done` was signalled.
    /// Replay an inner `whenever` subscription registration
    /// `[Supply, callback, last?, quit?]` whose source has no live channel or
    /// supplier id (so `value_to_react_subscription` returned None) — a finite
    /// `Supply.from-list` source, or a chained `supply { ... }` transform.
    /// Returns `Some(true)` if a body signalled react `done`, `Some(false)` after
    /// a normal replay, or `None` if `v` is not such a Supply registration.
    pub(super) fn replay_inner_static_subscription(
        &mut self,
        v: &Value,
    ) -> Result<Option<bool>, RuntimeError> {
        let Value::Array(inner_items, ..) = v else {
            return Ok(None);
        };
        let Some(source @ Value::Instance { class_name, .. }) = inner_items.first() else {
            return Ok(None);
        };
        if class_name != "Supply" {
            return Ok(None);
        }
        let source = source.clone();
        let inner_cb = inner_items.get(1).cloned().unwrap_or(Value::Nil);
        let inner_last = inner_items
            .get(2)
            .and_then(crate::runtime::Interpreter::value_array_items)
            .unwrap_or_default();
        self.drive_inner_supply_to_consumer(&source, &inner_cb, &inner_last)
            .map(Some)
    }

    /// Synchronously drive a finite `Supply` `source`, delivering each value to
    /// `consumer_cb`. Handles two shapes that the polling event loop cannot:
    /// a `values`-backed source (`Supply.from-list`) replayed directly, and a
    /// chained `supply { whenever <upstream> { emit ... } }` transform whose
    /// `whenever` source is itself an on-demand supply (recursed into). For the
    /// on-demand case a streaming consumer is registered so the body's `emit`s
    /// reach `consumer_cb` via `try_stream_emit`; inner registrations are driven
    /// recursively, which is what lets a multi-stage Cro pipeline pass values all
    /// the way through. Returns `true` if any body signalled react `done`.
    fn drive_inner_supply_to_consumer(
        &mut self,
        source: &Value,
        consumer_cb: &Value,
        last_callbacks: &[Value],
    ) -> Result<bool, RuntimeError> {
        let Value::Instance {
            class_name,
            attributes,
            ..
        } = source
        else {
            return Ok(false);
        };
        if class_name != "Supply" {
            return Ok(false);
        }
        // Guard against runaway nesting. A pipeline that reuses the *same*
        // `supply { ... }` transform sub more than once shares one emitter
        // variable name (assigned per parse-site), which a deeper fix to lexical
        // emitter capture would remove; until then a same-sub-twice chain can
        // loop. Bail gracefully instead of overflowing the stack.
        if self.supply_stream_consumers.len() > 256 {
            return Err(RuntimeError::new(
                "supply pipeline nested too deeply (a transform sub reused in the \
                 same pipeline shares an emitter binding)",
            ));
        }
        let attrs = attributes.as_map();
        // A `supply { ... }` block carries both an (empty) `values` array and an
        // `on_demand_callback`; a `Supply.from-list` carries only `values`. Prefer
        // the on-demand body when present so a chained transform is driven, not
        // mistaken for an empty static source.
        let on_demand_cb = match attrs.get("on_demand_callback").cloned() {
            Some(cb) => cb,
            None => {
                if attrs.contains_key("values") {
                    return self.replay_static_supply(&attrs, consumer_cb, last_callbacks);
                }
                // A channel-backed or otherwise live source cannot be replayed here.
                return Ok(false);
            }
        };
        let sid = next_supplier_id();
        self.supply_stream_consumers.push(StreamConsumer {
            supplier_id: sid,
            consumer_cb: consumer_cb.clone(),
            done: false,
        });
        let idx = self.supply_stream_consumers.len() - 1;
        let (res, emitted, _) = loan_env!(self, run_on_demand_body(on_demand_cb, Some(sid)));
        let mut reached = self
            .supply_stream_consumers
            .get(idx)
            .map(|c| c.done)
            .unwrap_or(false);
        if let Err(e) = res
            && !e.is_react_done()
        {
            self.supply_stream_consumers.truncate(idx);
            return Err(crate::runtime::Interpreter::wrap_react_died(e));
        }
        if !reached {
            for v in emitted {
                if crate::runtime::Interpreter::is_supply_subscription_registration(&v) {
                    if let Value::Array(inner, ..) = &v
                        && let Some(inner_src @ Value::Instance { .. }) = inner.first()
                    {
                        let inner_src = inner_src.clone();
                        let inner_cb = inner.get(1).cloned().unwrap_or(Value::Nil);
                        let inner_last = inner
                            .get(2)
                            .and_then(crate::runtime::Interpreter::value_array_items)
                            .unwrap_or_default();
                        if self.drive_inner_supply_to_consumer(
                            &inner_src,
                            &inner_cb,
                            &inner_last,
                        )? {
                            reached = true;
                            break;
                        }
                    }
                } else {
                    // A plain value buffered by the body (no active stream route):
                    // deliver it to the consumer directly.
                    match self.call_react_callback(&consumer_cb.clone(), vec![v]) {
                        Err(e) if e.is_react_done() => {
                            reached = true;
                            break;
                        }
                        other => {
                            other?;
                        }
                    }
                }
            }
        }
        self.supply_stream_consumers.truncate(idx);
        if !reached {
            for cb in last_callbacks {
                match self.call_react_callback(&cb.clone(), Vec::new()) {
                    Err(e) if e.is_react_done() => {
                        reached = true;
                        break;
                    }
                    other => {
                        other?;
                    }
                }
            }
        }
        Ok(reached)
    }

    pub(super) fn replay_static_supply(
        &mut self,
        attributes: &HashMap<String, Value>,
        callback: &Value,
        last_callbacks: &[Value],
    ) -> Result<bool, RuntimeError> {
        let mut last_topic: Option<Value> = None;
        if let Some(Value::Array(values, ..)) = attributes.get("values") {
            for v in values.iter() {
                match self.call_react_callback(&callback.clone(), vec![v.clone()]) {
                    Ok(_) => {}
                    Err(e) if e.is_react_done() => return Ok(true),
                    Err(e) if e.is_next() => continue,
                    Err(e) if e.is_last() => {
                        last_topic = Some(v.clone());
                        break;
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        for cb in last_callbacks {
            let args = match &last_topic {
                Some(v) => vec![v.clone()],
                None => Vec::new(),
            };
            match self.call_react_callback(&cb.clone(), args) {
                Err(e) if e.is_react_done() => return Ok(true),
                other => {
                    other?;
                }
            }
        }
        Ok(false)
    }
}
