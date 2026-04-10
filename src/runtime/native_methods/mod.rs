// Native method dispatch submodules, split from the original native_methods.rs
mod concurrency;
mod encoding;
mod proc;
mod scheduler;
mod socket_async;
mod socket_helpers;
mod socket_inet;
pub(crate) mod state;
pub(crate) mod state_lock;
pub(crate) mod state_scheduler;
pub(crate) mod state_supplier;
mod system;

// Re-export public items from state submodules so that
// `crate::runtime::native_methods::X` paths continue to work.

// Re-export pub(crate) items accessed from outside `runtime` module
pub(crate) use state::{
    AsyncSocketConnState, SupplyEvent, split_supply_chunks_into_lines, take_supply_channel,
};
pub(crate) use state_lock::{acquire_lock, current_thread_id, lock_runtime_by_id, release_lock};

// Re-export pub(in crate::runtime) items accessed from sibling `runtime` modules
pub(in crate::runtime) use state::{
    allocate_async_listen_port, get_supply_collected_output, get_supply_taps,
    lookup_async_listener, next_async_socket_id, next_supplier_id, next_supply_id, proc_stdin_map,
    register_async_connection, register_promise_combinator_sources, register_supply_tap,
    set_supply_collected_output, supplier_done, supplier_done_deferred, supplier_emit,
    supplier_id_from_attrs, supplier_quit, supplier_register_promise, supplier_snapshot,
    supply_channel_map, supply_channel_map_pub, take_promise_combinator_sources,
    update_async_connection,
};
pub(in crate::runtime) use state_lock::next_lock_id;
pub(in crate::runtime) use state_scheduler::{
    fake_scheduler_cue_counter, fake_scheduler_init, next_fake_scheduler_id,
};
pub(in crate::runtime) use state_supplier::{
    SupplierEmitAction, flush_supplier_batch_taps, flush_supplier_line_taps, get_classify_state,
    get_classify_sub_supplier_ids, get_start_output_supplier_ids, register_supplier_batch_tap,
    register_supplier_classify_tap, register_supplier_done_callback, register_supplier_elems_tap,
    register_supplier_lines_tap, register_supplier_produce_tap, register_supplier_quit_callback,
    register_supplier_start_tap, register_supplier_tap, register_supplier_tap_with_head_limit,
    register_supplier_unique_tap, supplier_emit_callbacks, supplier_produce_update_acc,
    supplier_tap_count, supplier_unique_get_seen, supplier_unique_mark_seen,
    take_supplier_done_callbacks, take_supplier_quit_callbacks, update_classify_state,
};

use super::*;
use std::time::Duration;

impl Interpreter {
    pub(super) fn resolve_supply_delay_seconds(
        &self,
        arg: Option<&Value>,
    ) -> Result<f64, RuntimeError> {
        let Some(value) = arg else {
            return Err(RuntimeError::new(
                "Supply.delayed requires a delay argument",
            ));
        };
        let delay = value.to_f64();
        if delay.is_nan() {
            return Err(RuntimeError::new("Supply.delayed requires a numeric delay"));
        }
        if delay.is_infinite() {
            return Err(RuntimeError::new("Supply.delayed requires a finite delay"));
        }
        Ok(delay.max(0.0))
    }

    pub(super) fn supply_delay_seconds(attributes: &HashMap<String, Value>) -> f64 {
        attributes
            .get("delay_seconds")
            .map(Value::to_f64)
            .filter(|value| value.is_finite() && *value > 0.0)
            .unwrap_or(0.0)
    }

    pub(super) fn sleep_for_supply_delay(delay_seconds: f64) {
        if delay_seconds > 0.0 {
            std::thread::sleep(Duration::from_secs_f64(delay_seconds));
        }
    }

    pub(super) fn resolve_supply_tail_count(
        &mut self,
        arg: Option<&Value>,
        total_len: usize,
    ) -> Result<usize, RuntimeError> {
        let Some(value) = arg else {
            return Ok(1);
        };
        let parsed = match value {
            Value::Int(i) => *i,
            Value::BigInt(i) => {
                let text = i.to_string();
                if text.starts_with('-') || text == "0" {
                    return Ok(0);
                }
                return Ok(total_len);
            }
            Value::Whatever => return Ok(total_len),
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => return Ok(total_len),
            Value::Num(f) if f.is_infinite() && f.is_sign_negative() => return Ok(0),
            Value::Num(f) if f.is_nan() => {
                return Err(RuntimeError::new("Cannot use NaN as a tail count"));
            }
            Value::Num(f) => *f as i64,
            Value::Str(s) => {
                let trimmed = s.trim();
                if trimmed.eq_ignore_ascii_case("inf") {
                    return Ok(total_len);
                }
                if trimmed.eq_ignore_ascii_case("-inf") {
                    return Ok(0);
                }
                match trimmed.parse::<f64>() {
                    Ok(f) if f.is_infinite() && f.is_sign_positive() => return Ok(total_len),
                    Ok(f) if f.is_infinite() && f.is_sign_negative() => return Ok(0),
                    Ok(f) if f.is_nan() => {
                        return Err(RuntimeError::new("Cannot use NaN as a tail count"));
                    }
                    Ok(f) => f as i64,
                    Err(_) => {
                        return Err(RuntimeError::new(format!(
                            "Cannot use '{}' as a tail count",
                            s
                        )));
                    }
                }
            }
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                let computed =
                    self.eval_call_on_value(value.clone(), vec![Value::Int(total_len as i64)])?;
                return self.resolve_supply_tail_count(Some(&computed), total_len);
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Cannot use '{}' as a tail count",
                    value.to_string_value()
                )));
            }
        };
        Ok(parsed.clamp(0, total_len as i64) as usize)
    }

    /// Alias for `decode_with_encoding` used by supply methods.
    pub(super) fn decode_bytes_for_supply(
        &self,
        bytes: &[u8],
        encoding_name: &str,
    ) -> Result<String, RuntimeError> {
        self.decode_with_encoding(bytes, encoding_name)
    }

    pub(super) fn translate_newlines_for_decode_native(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\r', "\n"),
            NewlineMode::Crlf => input.replace("\r\n", "\n"),
        }
    }

    pub(super) fn supply_chunk_to_bytes(&self, chunk: &Value, encoding_name: &str) -> Vec<u8> {
        let normalized = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();
        match chunk {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if {
                let cn = class_name.resolve();
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
            } =>
            {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    return items
                        .iter()
                        .map(|v| match v {
                            Value::Int(i) => *i as u8,
                            _ => 0,
                        })
                        .collect();
                }
                Vec::new()
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "utf16" => {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    let use_be = normalized == "utf-16be";
                    let mut bytes = Vec::with_capacity(items.len() * 2);
                    for item in items.iter() {
                        let unit = match item {
                            Value::Int(i) => *i as u16,
                            _ => 0u16,
                        };
                        let pair = if use_be {
                            unit.to_be_bytes()
                        } else {
                            unit.to_le_bytes()
                        };
                        bytes.extend_from_slice(&pair);
                    }
                    return bytes;
                }
                Vec::new()
            }
            Value::Array(items, ..) => items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect(),
            Value::Int(i) => vec![*i as u8],
            Value::Str(s) => s.as_bytes().to_vec(),
            other => other.to_string_value().into_bytes(),
        }
    }

    /// Dispatch a mutable native instance method.
    /// Returns (result_value, updated_attributes).
    pub(super) fn call_native_instance_method_mut(
        &mut self,
        class_name: &str,
        attributes: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        let dispatch_class = if matches!(
            class_name,
            "Promise" | "Channel" | "Supply" | "Supplier" | "Proc::Async"
        ) {
            Some(class_name.to_string())
        } else {
            self.class_mro(class_name).into_iter().find(|candidate| {
                matches!(
                    candidate.as_str(),
                    "Promise" | "Channel" | "Supply" | "Supplier" | "Proc::Async"
                )
            })
        };
        match dispatch_class.as_deref().unwrap_or(class_name) {
            "Promise" => self.native_promise_mut(attributes, method, args),
            "Channel" => self.native_channel_mut(attributes, method, args),
            "Supply" => self.native_supply_mut(attributes, method, args),
            "Supplier" | "Supplier::Preserving" => {
                self.native_supplier_mut(attributes, method, args)
            }
            "Proc::Async" => self.native_proc_async_mut(attributes, method, args),
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on '{}'",
                method, class_name
            ))),
        }
    }

    /// Dispatch an immutable native instance method.
    pub(super) fn call_native_instance_method(
        &mut self,
        class_name: &str,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let dispatch_class = if matches!(
            class_name,
            "IO::Path"
                | "IO::Handle"
                | "IO::Special"
                | "IO::Socket::INET"
                | "IO::Socket::Async"
                | "IO::Socket::Async::Listener"
                | "IO::Pipe"
                | "Lock"
                | "Lock::Async"
                | "Lock::ConditionVariable"
                | "Distro"
                | "Kernel"
                | "Perl"
                | "Compiler"
                | "Promise"
                | "Promise::Vow"
                | "Channel"
                | "Thread"
                | "Proc::Async"
                | "Proc"
                | "Supply"
                | "Supplier"
                | "Tap"
                | "ThreadPoolScheduler"
                | "CurrentThreadScheduler"
                | "FakeScheduler"
                | "Cancellation"
                | "Encoding::Builtin"
                | "Encoding::Encoder"
                | "Encoding::Decoder"
                | "VM"
        ) {
            Some(class_name.to_string())
        } else {
            self.class_mro(class_name).into_iter().find(|candidate| {
                matches!(
                    candidate.as_str(),
                    "IO::Path"
                        | "IO::Handle"
                        | "IO::Special"
                        | "IO::Socket::INET"
                        | "IO::Socket::Async"
                        | "IO::Socket::Async::Listener"
                        | "IO::Pipe"
                        | "Lock"
                        | "Lock::Async"
                        | "Lock::ConditionVariable"
                        | "Distro"
                        | "Kernel"
                        | "Perl"
                        | "Compiler"
                        | "Promise"
                        | "Promise::Vow"
                        | "Channel"
                        | "Thread"
                        | "Proc::Async"
                        | "Proc"
                        | "Supply"
                        | "Supplier"
                        | "Tap"
                        | "ThreadPoolScheduler"
                        | "CurrentThreadScheduler"
                        | "FakeScheduler"
                        | "Cancellation"
                        | "Encoding::Builtin"
                        | "Encoding::Encoder"
                        | "Encoding::Decoder"
                        | "VM"
                )
            })
        };
        match dispatch_class.as_deref().unwrap_or(class_name) {
            "IO::Path" => self.native_io_path(attributes, method, args),
            "IO::Handle" => self.native_io_handle(attributes, method, args),
            "IO::Special" => self.native_io_special(attributes, method, args),
            "IO::Socket::INET" => self.native_socket_inet(attributes, method, args),
            "IO::Socket::Async" => self.native_socket_async(attributes, method, args),
            "IO::Socket::Async::Listener" => {
                self.native_socket_async_listener(attributes, method, args)
            }
            "IO::Pipe" => self.native_io_pipe(attributes, method, &args),
            "Lock" | "Lock::Async" => self.native_lock(attributes, method, args),
            "Lock::ConditionVariable" => self.native_condition_variable(attributes, method, args),
            "Distro" => self.native_distro(attributes, method),
            "Kernel" => self.native_kernel(attributes, method, args),
            "Perl" => Ok(self.native_perl(attributes, method)),
            "Compiler" => Ok(self.native_perl(attributes, method)),
            "Promise" => self.native_promise(attributes, method, args),
            "Promise::Vow" => self.native_promise_vow(attributes, method, args),
            "Channel" => Ok(self.native_channel(attributes, method)),
            "Thread" => self.native_thread(attributes, method),
            "Proc::Async" => self.native_proc_async(attributes, method, args),
            "Proc" => Ok(self.native_proc(attributes, method)),
            "Supply" => self.native_supply(attributes, method, args),
            "Supplier" | "Supplier::Preserving" => self.native_supplier(attributes, method, args),
            "Tap" => self.native_tap(attributes, method),
            "ThreadPoolScheduler" => self.native_scheduler(attributes, method, args, false),
            "CurrentThreadScheduler" => self.native_scheduler(attributes, method, args, true),
            "FakeScheduler" => self.native_fake_scheduler(attributes, method, args),
            "Cancellation" => self.native_cancellation(attributes, method),
            "Encoding::Builtin" => Ok(Self::native_encoding_builtin(attributes, method, &args)),
            "Encoding::Encoder" => Self::native_encoding_encoder(attributes, method, &args),
            "Encoding::Decoder" => Ok(Self::native_encoding_decoder(attributes, method, &args)),
            "VM" => self.native_vm(attributes, method),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on '{}'",
                method, class_name
            ))),
        }
    }
}
