//! Driving the actions a live supplier emission produces, at every hop of a
//! combinator chain.
//!
//! A live `Supply` combinator (`map`, `grep`, `produce`, `batch`, `flat`,
//! `zip`, ...) builds a *derived supplier* and registers a tap on its source
//! that forwards into it. So an emit into the head of a chain produces actions
//! on the head's supplier, one of which re-emits into the next supplier, whose
//! own actions must then be run, and so on.
//!
//! Every re-emit site used to inline its own loop of the shape
//!
//! ```ignore
//! supplier_emit(dsid, v.clone());
//! for a in supplier_emit_callbacks(dsid, &v) {
//!     if let SupplierEmitAction::Call(tap, emitted, delay) = a { /* ... */ }
//! }
//! ```
//!
//! which handled exactly one action kind and dropped the other twelve. A
//! derived supplier whose own tap was itself a combinator therefore received
//! nothing at all: `$s.Supply.map(...).map(...)`, `.map(...).produce(...)` and
//! `.grep(...).produce(...)` all emitted silently into the void, and a `react
//! whenever` on such a chain waited forever on a value that was computed and
//! then discarded one hop upstream.
//!
//! [`Interpreter::handle_supply_forward`] is the single re-emit primitive now,
//! and it drives the *complete* action set through
//! [`Interpreter::drive_supplier_emit_actions`], recursing naturally for as
//! many stages as the chain has.

use super::*;
use crate::runtime::native_methods::{
    SupplierEmitAction, ZipAction, supplier_done, supplier_emit, supplier_emit_callbacks,
    supplier_produce_update_acc, supplier_unique_mark_seen, take_supplier_done_callbacks,
    zip_buffer_value, zip_latest_buffer_value, zip_latest_state_info, zip_state_info,
};

impl Interpreter {
    /// Re-emit `value` into `downstream_supplier_id` and run everything its
    /// taps ask for. This is the one hop of a combinator chain.
    pub(in crate::runtime) fn handle_supply_forward(
        &mut self,
        downstream_supplier_id: u64,
        value: Value,
    ) -> Result<(), RuntimeError> {
        supplier_emit(downstream_supplier_id, value.clone());
        let actions = supplier_emit_callbacks(downstream_supplier_id, &value);
        self.drive_supplier_emit_actions(downstream_supplier_id, actions)
    }

    /// Run every action `supplier_id`'s taps produced for one emission.
    ///
    /// Actions that re-emit into a further supplier go back through
    /// [`Interpreter::handle_supply_forward`], so a chain of any length is
    /// driven to its end rather than one hop.
    pub(in crate::runtime) fn drive_supplier_emit_actions(
        &mut self,
        supplier_id: u64,
        actions: Vec<SupplierEmitAction>,
    ) -> Result<(), RuntimeError> {
        for action in actions {
            match action {
                SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                    Self::sleep_for_supply_delay(delay_seconds);
                    self.call_supply_tap(tap, vec![emitted], true)?;
                }
                SupplierEmitAction::UniqueCheck {
                    downstream_supplier_id,
                    value,
                    as_fn,
                    with_fn,
                    tap_index,
                } => {
                    let key = if let Some(f) = as_fn {
                        self.call_sub_value(f, vec![value.clone()], true)?
                    } else {
                        value.clone()
                    };
                    let is_dup =
                        self.supplier_unique_check_seen(supplier_id, tap_index, &key, &with_fn)?;
                    if !is_dup {
                        supplier_unique_mark_seen(supplier_id, tap_index, key);
                        self.handle_supply_forward(downstream_supplier_id, value)?;
                    }
                }
                SupplierEmitAction::ClassifyCheck { value, tap_index } => {
                    self.handle_classify_emit(supplier_id, tap_index, value)?;
                }
                SupplierEmitAction::HeadLimitReached { supplier_id: sid } => {
                    let deferred_promises =
                        crate::runtime::native_methods::supplier_done_deferred(sid);
                    for done_cb in take_supplier_done_callbacks(sid) {
                        let _ = self.invoke_done_callback(done_cb);
                    }
                    for (promise, result) in deferred_promises {
                        promise.keep(result, String::new(), String::new());
                    }
                }
                SupplierEmitAction::ProduceCall {
                    callback,
                    callable,
                    value,
                    accumulator,
                    delay_seconds,
                    tap_index,
                    downstream_supplier_id,
                } => {
                    let new_acc = if let Some(acc) = accumulator {
                        self.call_sub_value(callable, vec![acc, value], false)?
                    } else {
                        value
                    };
                    supplier_produce_update_acc(supplier_id, tap_index, new_acc.clone());
                    // `Supply.produce` forwards each running value into the
                    // derived supply it handed back; a `reduce` tap shares this
                    // accumulator but has neither, and emits once at done.
                    if let Some(dsid) = downstream_supplier_id {
                        self.handle_supply_forward(dsid, new_acc)?;
                    } else if !callback.is_nil() {
                        Self::sleep_for_supply_delay(delay_seconds);
                        self.call_sub_value(callback, vec![new_acc], true)?;
                    }
                }
                SupplierEmitAction::StartCall {
                    callable,
                    value,
                    output_supplier_id,
                } => {
                    self.run_start_call_in_thread(callable, value, output_supplier_id);
                }
                SupplierEmitAction::BatchEmit {
                    downstream_supplier_id,
                    batch,
                } => {
                    self.handle_supply_forward(downstream_supplier_id, Value::array(batch))?;
                }
                SupplierEmitAction::FlatEmit {
                    downstream_supplier_id,
                    items,
                } => {
                    for item in items {
                        self.handle_supply_forward(downstream_supplier_id, item)?;
                    }
                }
                SupplierEmitAction::ZipBuffer {
                    zip_state_id,
                    source_index,
                    value,
                } => {
                    if let ZipAction::Emit(tuple_val) =
                        zip_buffer_value(zip_state_id, source_index, value)
                    {
                        let (output_sid, with_fn) = zip_state_info(zip_state_id);
                        let emit_val = self.apply_zip_with(with_fn, tuple_val);
                        self.handle_supply_forward(output_sid, emit_val)?;
                    }
                }
                SupplierEmitAction::ZipLatestBuffer {
                    zip_latest_state_id,
                    source_index,
                    value,
                } => {
                    if let ZipAction::Emit(tuple_val) =
                        zip_latest_buffer_value(zip_latest_state_id, source_index, value)
                    {
                        let (output_sid, with_fn) = zip_latest_state_info(zip_latest_state_id);
                        let emit_val = self.apply_zip_with(with_fn, tuple_val);
                        self.handle_supply_forward(output_sid, emit_val)?;
                    }
                }
                SupplierEmitAction::Migrate {
                    value,
                    master_supplier_id,
                    downstream_supplier_id,
                    tap_index,
                } => {
                    self.handle_supply_migrate(
                        value,
                        master_supplier_id,
                        downstream_supplier_id,
                        tap_index,
                    )?;
                }
                SupplierEmitAction::ForwardEmit {
                    downstream_supplier_id,
                    value,
                } => {
                    self.handle_supply_forward(downstream_supplier_id, value)?;
                }
                SupplierEmitAction::TransformCall {
                    downstream_supplier_id,
                    callable,
                    mode,
                    value,
                } => {
                    self.handle_supply_transform_emit(
                        downstream_supplier_id,
                        callable,
                        mode,
                        value,
                    )?;
                }
            }
        }
        Ok(())
    }

    /// `zip`/`zip-latest`'s optional `:with` combiner applied to one buffered
    /// tuple. Without it the tuple itself is what the zip Supply emits.
    pub(in crate::runtime) fn apply_zip_with(
        &mut self,
        with_fn: Option<Value>,
        tuple_val: Value,
    ) -> Value {
        let Some(f) = with_fn else {
            return tuple_val;
        };
        match tuple_val.view() {
            ValueView::Array(items, ..) => {
                let items = items.to_vec();
                self.call_sub_value(f, items, false).unwrap_or(tuple_val)
            }
            _ => tuple_val,
        }
    }

    /// Emit `value` into `downstream_supplier_id`, drive its taps, then finish
    /// it: the shape `done`-time flushes (batch buffers, `reduce` results) need,
    /// where one last value is delivered and the derived supply then ends.
    pub(in crate::runtime) fn forward_and_finish_supply(
        &mut self,
        downstream_supplier_id: u64,
        value: Value,
    ) -> Result<(), RuntimeError> {
        self.handle_supply_forward(downstream_supplier_id, value)?;
        supplier_done(downstream_supplier_id);
        for done_cb in take_supplier_done_callbacks(downstream_supplier_id) {
            let _ = self.invoke_done_callback(done_cb);
        }
        Ok(())
    }
}
