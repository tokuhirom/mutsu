use super::*;
use std::process::ChildStdin;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc;
use std::time::Duration;

type StdinMap = std::sync::Mutex<HashMap<u32, Arc<std::sync::Mutex<Option<ChildStdin>>>>>;

pub(super) fn proc_stdin_map() -> &'static StdinMap {
    static MAP: OnceLock<StdinMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

type SupplyTapsMap = std::sync::Mutex<HashMap<u64, Vec<Value>>>;

fn supply_taps_map() -> &'static SupplyTapsMap {
    static MAP: OnceLock<SupplyTapsMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

type CancellationMap = std::sync::Mutex<HashMap<u64, Arc<AtomicBool>>>;

fn cancellation_map() -> &'static CancellationMap {
    static MAP: OnceLock<CancellationMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Supply channel registry: supply_id -> Receiver for streaming data from
/// Proc::Async stdout/stderr reader threads.
type SupplyChannelMap = std::sync::Mutex<HashMap<u64, mpsc::Receiver<SupplyEvent>>>;

pub(super) fn supply_channel_map() -> &'static SupplyChannelMap {
    static MAP: OnceLock<SupplyChannelMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Events sent through supply channels
#[derive(Debug, Clone)]
pub(crate) enum SupplyEvent {
    Emit(Value),
    Done,
    Quit(Value),
}

/// Take a receiver from the supply channel registry (can only be consumed once)
pub(crate) fn take_supply_channel(supply_id: u64) -> Option<mpsc::Receiver<SupplyEvent>> {
    if let Ok(mut map) = supply_channel_map().lock() {
        map.remove(&supply_id)
    } else {
        None
    }
}

/// Public access to the supply channel map for signal registration
pub(super) fn supply_channel_map_pub()
-> &'static std::sync::Mutex<HashMap<u64, mpsc::Receiver<SupplyEvent>>> {
    supply_channel_map()
}

#[derive(Debug, Default)]
struct SupplierRuntimeState {
    emitted: Vec<Value>,
    done: bool,
    quit_reason: Option<Value>,
    pending_promises: Vec<SharedPromise>,
}

type SupplierStateMap = std::sync::Mutex<HashMap<u64, SupplierRuntimeState>>;

fn supplier_state_map() -> &'static SupplierStateMap {
    static MAP: OnceLock<SupplierStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

pub(super) fn supplier_id_from_attrs(attributes: &HashMap<String, Value>) -> Option<u64> {
    match attributes.get("supplier_id") {
        Some(Value::Int(id)) if *id > 0 => Some(*id as u64),
        _ => None,
    }
}

pub(super) fn supplier_snapshot(supplier_id: u64) -> (Vec<Value>, bool, Option<Value>) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        (state.emitted.clone(), state.done, state.quit_reason.clone())
    } else {
        (Vec::new(), false, None)
    }
}

pub(crate) fn split_supply_chunks_into_lines(chunks: &[Value], chomp: bool) -> Vec<Value> {
    let mut combined = String::new();
    for chunk in chunks {
        combined.push_str(&chunk.to_string_value());
    }
    crate::builtins::split_lines_with_chomp(&combined, chomp)
        .into_iter()
        .map(Value::Str)
        .collect()
}

fn take_complete_lines_from_buffer(buffer: &mut String, chomp: bool, flush: bool) -> Vec<String> {
    let bytes = buffer.as_bytes();
    let mut out = Vec::new();
    let mut start = 0usize;
    let mut i = 0usize;
    while i < bytes.len() {
        let sep_len = if bytes[i] == b'\n' {
            1
        } else if bytes[i] == b'\r' {
            if i + 1 < bytes.len() {
                if bytes[i + 1] == b'\n' { 2 } else { 1 }
            } else if flush {
                1
            } else {
                break;
            }
        } else {
            i += 1;
            continue;
        };
        let end = if chomp { i } else { i + sep_len };
        out.push(buffer[start..end].to_string());
        i += sep_len;
        start = i;
    }
    let remaining = buffer[start..].to_string();
    *buffer = remaining;
    if flush && !buffer.is_empty() {
        out.push(std::mem::take(buffer));
    }
    out
}

pub(super) fn supplier_register_promise(supplier_id: u64, promise: SharedPromise) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if let Some(reason) = state.quit_reason.clone() {
            promise.break_with(reason, String::new(), String::new());
        } else if state.done {
            let result = state.emitted.last().cloned().unwrap_or(Value::Nil);
            promise.keep(result, String::new(), String::new());
        } else {
            state.pending_promises.push(promise);
        }
    }
}

pub(super) fn supplier_emit(supplier_id: u64, value: Value) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.emitted.push(value);
    }
}

pub(super) fn supplier_done(supplier_id: u64) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.done = true;
        let result = state.emitted.last().cloned().unwrap_or(Value::Nil);
        let pending = std::mem::take(&mut state.pending_promises);
        for promise in pending {
            promise.keep(result.clone(), String::new(), String::new());
        }
    }
}

pub(super) fn supplier_quit(supplier_id: u64, reason: Value) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.quit_reason = Some(reason.clone());
        let pending = std::mem::take(&mut state.pending_promises);
        for promise in pending {
            promise.break_with(reason.clone(), String::new(), String::new());
        }
    }
}

#[derive(Clone)]
struct SupplierTapSubscription {
    callback: Value,
    line_mode: bool,
    line_chomp: bool,
    line_buffer: String,
    delay_seconds: f64,
}

#[derive(Clone, Default)]
struct SupplierSubscriptions {
    taps: Vec<SupplierTapSubscription>,
    done_callbacks: Vec<Value>,
}

type SupplierSubscriptionsMap = std::sync::Mutex<HashMap<u64, SupplierSubscriptions>>;

fn supplier_subscriptions_map() -> &'static SupplierSubscriptionsMap {
    static MAP: OnceLock<SupplierSubscriptionsMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

#[derive(Debug, Default)]
struct LockState {
    owner: Option<String>,
    recursion: u64,
}

#[derive(Debug, Default)]
struct LockRuntime {
    state: std::sync::Mutex<LockState>,
    lock_cv: std::sync::Condvar,
    condvars: std::sync::Mutex<HashMap<u64, Arc<std::sync::Condvar>>>,
}

type LockStateMap = std::sync::Mutex<HashMap<u64, Arc<LockRuntime>>>;

fn lock_state_map() -> &'static LockStateMap {
    static MAP: OnceLock<LockStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

pub(super) fn next_supply_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn next_supplier_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn next_lock_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = lock_state_map().lock() {
        map.entry(id)
            .or_insert_with(|| Arc::new(LockRuntime::default()));
    }
    id
}

fn next_cancellation_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = cancellation_map().lock() {
        map.insert(id, Arc::new(AtomicBool::new(false)));
    }
    id
}

fn cancellation_state(id: u64) -> Option<Arc<AtomicBool>> {
    cancellation_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&id).cloned())
}

fn lock_runtime_by_id(id: u64) -> Option<Arc<LockRuntime>> {
    lock_state_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&id).cloned())
}

fn current_thread_key() -> String {
    format!("{:?}", std::thread::current().id())
}

fn acquire_lock(runtime: &LockRuntime, me: &str) -> Result<(), RuntimeError> {
    let mut state = runtime
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
    loop {
        match &state.owner {
            None => {
                state.owner = Some(me.to_string());
                state.recursion = 1;
                return Ok(());
            }
            Some(owner) if owner == me => {
                state.recursion += 1;
                return Ok(());
            }
            Some(_) => {
                state = runtime
                    .lock_cv
                    .wait(state)
                    .map_err(|_| RuntimeError::new("Lock wait failed"))?;
            }
        }
    }
}

fn release_lock(runtime: &LockRuntime, me: &str) -> Result<(), RuntimeError> {
    let mut state = runtime
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
    match &state.owner {
        Some(owner) if owner == me => {
            if state.recursion > 1 {
                state.recursion -= 1;
            } else {
                state.recursion = 0;
                state.owner = None;
                runtime.lock_cv.notify_one();
            }
            Ok(())
        }
        _ => Err(RuntimeError::new(
            "Cannot unlock a Lock not owned by current thread",
        )),
    }
}

fn ensure_condition(runtime: &LockRuntime, cond_id: u64) -> Option<Arc<std::sync::Condvar>> {
    runtime.condvars.lock().ok().map(|mut map| {
        map.entry(cond_id)
            .or_insert_with(|| Arc::new(std::sync::Condvar::new()))
            .clone()
    })
}

fn next_condition_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn register_supply_tap(supply_id: u64, tap: Value) {
    if let Ok(mut map) = supply_taps_map().lock() {
        map.entry(supply_id).or_default().push(tap);
    }
}

pub(super) fn get_supply_taps(supply_id: u64) -> Vec<Value> {
    if let Ok(map) = supply_taps_map().lock() {
        map.get(&supply_id).cloned().unwrap_or_default()
    } else {
        Vec::new()
    }
}

pub(super) fn register_supplier_tap(supplier_id: u64, tap: Value, delay_seconds: f64) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: tap,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds,
            });
    }
}

pub(super) fn register_supplier_lines_tap(
    supplier_id: u64,
    tap: Value,
    chomp: bool,
    delay_seconds: f64,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: tap,
                line_mode: true,
                line_chomp: chomp,
                line_buffer: String::new(),
                delay_seconds,
            });
    }
}

pub(super) fn supplier_emit_callbacks(
    supplier_id: u64,
    emitted_value: &Value,
) -> Vec<(Value, Value, f64)> {
    let mut callbacks = Vec::new();
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in &mut subs.taps {
            if tap.line_mode {
                tap.line_buffer.push_str(&emitted_value.to_string_value());
                for line in
                    take_complete_lines_from_buffer(&mut tap.line_buffer, tap.line_chomp, false)
                {
                    callbacks.push((tap.callback.clone(), Value::Str(line), tap.delay_seconds));
                }
            } else {
                callbacks.push((
                    tap.callback.clone(),
                    emitted_value.clone(),
                    tap.delay_seconds,
                ));
            }
        }
    }
    callbacks
}

pub(super) fn flush_supplier_line_taps(supplier_id: u64) -> Vec<(Value, Value)> {
    let mut callbacks = Vec::new();
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in &mut subs.taps {
            if tap.line_mode {
                for line in
                    take_complete_lines_from_buffer(&mut tap.line_buffer, tap.line_chomp, true)
                {
                    callbacks.push((tap.callback.clone(), Value::Str(line)));
                }
            }
        }
    }
    callbacks
}

pub(super) fn take_supplier_done_callbacks(supplier_id: u64) -> Vec<Value> {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        if let Some(subs) = map.get_mut(&supplier_id) {
            std::mem::take(&mut subs.done_callbacks)
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    }
}

pub(super) fn register_supplier_done_callback(supplier_id: u64, done_cb: Value) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .done_callbacks
            .push(done_cb);
    }
}

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

    pub(super) fn translate_newlines_for_decode_native(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\r', "\n"),
            NewlineMode::Crlf => input.replace("\r\n", "\n"),
        }
    }

    pub(super) fn decode_bytes_for_supply(
        &self,
        bytes: &[u8],
        encoding_name: &str,
    ) -> Result<String, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "ascii" => Ok(bytes
                .iter()
                .map(|b| if *b <= 0x7F { *b as char } else { '\u{FFFD}' })
                .collect()),
            "iso-8859-1" => Ok(bytes.iter().map(|b| *b as char).collect()),
            "utf-16" | "utf-16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16be byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            _ => {
                if let Some(enc) = encoding_rs::Encoding::for_label(encoding.as_bytes()) {
                    let (decoded, _used_encoding, _had_errors) = enc.decode(bytes);
                    return Ok(decoded.into_owned());
                }
                Ok(String::from_utf8_lossy(bytes).into_owned())
            }
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
            } if class_name == "Buf" || class_name == "Blob" || class_name == "utf8" => {
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
        match class_name {
            "Promise" => self.native_promise_mut(attributes, method, args),
            "Channel" => self.native_channel_mut(attributes, method, args),
            "Supply" => self.native_supply_mut(attributes, method, args),
            "Supplier" => self.native_supplier_mut(attributes, method, args),
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
        match class_name {
            "IO::Path" => self.native_io_path(attributes, method, args),
            "IO::Handle" => self.native_io_handle(attributes, method, args),
            "IO::Socket::INET" => self.native_socket_inet(attributes, method, args),
            "IO::Pipe" => self.native_io_pipe(attributes, method),
            "Lock" | "Lock::Async" => self.native_lock(attributes, method, args),
            "Lock::ConditionVariable" => self.native_condition_variable(attributes, method, args),
            "Distro" => self.native_distro(attributes, method),
            "Perl" => Ok(self.native_perl(attributes, method)),
            "Compiler" => Ok(self.native_perl(attributes, method)),
            "Promise" => self.native_promise(attributes, method, args),
            "Channel" => Ok(self.native_channel(attributes, method)),
            "Proc::Async" => Ok(self.native_proc_async(attributes, method)),
            "Proc" => Ok(self.native_proc(attributes, method)),
            "Supply" => self.native_supply(attributes, method, args),
            "Supplier" => self.native_supplier(attributes, method, args),
            "Tap" => self.native_tap(method),
            "ThreadPoolScheduler" | "CurrentThreadScheduler" => {
                self.native_scheduler(attributes, method, args)
            }
            "Cancellation" => self.native_cancellation(attributes, method),
            "Encoding::Builtin" => Ok(Self::native_encoding_builtin(attributes, method)),
            "Encoding::Encoder" => Ok(Self::native_encoding_encoder(attributes, method, &args)),
            "Encoding::Decoder" => Ok(Self::native_encoding_decoder(attributes, method, &args)),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on '{}'",
                method, class_name
            ))),
        }
    }

    fn cancellation_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "cancellation-id".to_string(),
            Value::Int(next_cancellation_id() as i64),
        );
        Value::make_instance("Cancellation".to_string(), attrs)
    }

    fn scheduler_times_arg(args: &[Value]) -> Result<Option<usize>, RuntimeError> {
        let Some(value) = Self::named_value(args, "times") else {
            return Ok(None);
        };
        let count = match value {
            Value::Int(i) => i,
            Value::Num(f) if f.is_finite() => f as i64,
            Value::Bool(b) => i64::from(b),
            Value::Str(s) => s.trim().parse::<i64>().map_err(|_| {
                RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    s
                ))
            })?,
            other => {
                return Err(RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    other.to_string_value()
                )));
            }
        };
        Ok(Some(count.max(0) as usize))
    }

    fn native_tap(&self, method: &str) -> Result<Value, RuntimeError> {
        match method {
            "cancel" => Ok(Value::Nil),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Tap",
                method
            ))),
        }
    }

    fn native_cancellation(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" => {
                if let Some(Value::Int(id)) = attributes.get("cancellation-id")
                    && *id > 0
                    && let Some(flag) = cancellation_state(*id as u64)
                {
                    flag.store(true, Ordering::Relaxed);
                }
                Ok(Value::Nil)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Cancellation",
                method
            ))),
        }
    }

    fn native_scheduler(
        &mut self,
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cue" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let times = Self::scheduler_times_arg(&args)?.unwrap_or(1);
                let cancellation = Self::cancellation_instance();
                let cancellation_id = match &cancellation {
                    Value::Instance { attributes, .. } => {
                        match attributes.get("cancellation-id").cloned() {
                            Some(Value::Int(id)) if id > 0 => id as u64,
                            _ => 0,
                        }
                    }
                    _ => 0,
                };
                let cancel_flag = cancellation_state(cancellation_id);
                for _ in 0..times {
                    if cancel_flag
                        .as_ref()
                        .is_some_and(|flag| flag.load(Ordering::Relaxed))
                    {
                        break;
                    }
                    self.call_sub_value(callback.clone(), Vec::new(), true)?;
                }
                Ok(cancellation)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Scheduler",
                method
            ))),
        }
    }

    fn native_lock(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "protect" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => {
                        return Err(RuntimeError::new(
                            "Lock.protect called on Lock without lock-id",
                        ));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.protect could not find lock state"))?;
                let me = current_thread_key();
                acquire_lock(&lock, &me)?;
                let code = args.first().cloned().unwrap_or(Value::Nil);
                let result = self.call_sub_value(code, Vec::new(), false);
                let _ = release_lock(&lock, &me);
                result
            }
            "lock" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => {
                        return Err(RuntimeError::new("Lock.lock called on invalid Lock"));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.lock could not find lock state"))?;
                let me = current_thread_key();
                acquire_lock(&lock, &me)?;
                Ok(Value::Nil)
            }
            "unlock" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => {
                        return Err(RuntimeError::new("Lock.unlock called on invalid Lock"));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.unlock could not find lock state"))?;
                let me = current_thread_key();
                release_lock(&lock, &me)?;
                Ok(Value::Nil)
            }
            "condition" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => return Err(RuntimeError::new("Lock.condition called on invalid Lock")),
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.condition could not find lock state"))?;
                let cond_id = next_condition_id();
                let _ = ensure_condition(&lock, cond_id).ok_or_else(|| {
                    RuntimeError::new("Lock.condition failed to create condition")
                })?;
                let mut attrs = HashMap::new();
                attrs.insert("lock-id".to_string(), Value::Int(lock_id as i64));
                attrs.insert("cond-id".to_string(), Value::Int(cond_id as i64));
                Ok(Value::make_instance(
                    "Lock::ConditionVariable".to_string(),
                    attrs,
                ))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Lock",
                method
            ))),
        }
    }

    fn native_condition_variable(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let lock_id = match attributes.get("lock-id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid lock-id")),
        };
        let cond_id = match attributes.get("cond-id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid cond-id")),
        };
        let lock = lock_runtime_by_id(lock_id)
            .ok_or_else(|| RuntimeError::new("Condition variable lock state not found"))?;
        let cond = ensure_condition(&lock, cond_id)
            .ok_or_else(|| RuntimeError::new("Condition variable state not found"))?;
        match method {
            "signal" => {
                cond.notify_one();
                Ok(Value::Nil)
            }
            "signal_all" => {
                cond.notify_all();
                Ok(Value::Nil)
            }
            "wait" => {
                let maybe_test = args.first().cloned();
                let me = current_thread_key();
                let mut state = lock
                    .state
                    .lock()
                    .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
                match &state.owner {
                    Some(owner) if owner == &me => {}
                    _ => {
                        return Err(RuntimeError::new(
                            "Condition.wait requires the current thread to hold the lock",
                        ));
                    }
                }
                if let Some(test) = maybe_test.clone()
                    && self.call_sub_value(test, Vec::new(), false)?.truthy()
                {
                    return Ok(Value::Nil);
                }
                let held_recursion = state.recursion;
                state.owner = None;
                state.recursion = 0;
                lock.lock_cv.notify_one();
                loop {
                    state = cond
                        .wait(state)
                        .map_err(|_| RuntimeError::new("Condition wait failed"))?;
                    while state.owner.is_some() && state.owner.as_deref() != Some(&me) {
                        state = lock
                            .lock_cv
                            .wait(state)
                            .map_err(|_| RuntimeError::new("Lock reacquire wait failed"))?;
                    }
                    state.owner = Some(me.clone());
                    state.recursion = held_recursion.max(1);
                    drop(state);

                    let predicate_ok = if let Some(test) = maybe_test.clone() {
                        self.call_sub_value(test, Vec::new(), false)?.truthy()
                    } else {
                        true
                    };
                    if predicate_ok {
                        return Ok(Value::Nil);
                    }
                    state = lock
                        .state
                        .lock()
                        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
                    state.owner = None;
                    state.recursion = 0;
                    lock.lock_cv.notify_one();
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Lock::ConditionVariable",
                method
            ))),
        }
    }

    // --- Promise mutable ---

    fn native_promise_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "keep" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                attrs.insert("result".to_string(), value);
                attrs.insert("status".to_string(), Value::Str("Kept".to_string()));
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Promise",
                method
            ))),
        }
    }

    // --- Channel mutable ---

    fn native_channel_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "send" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                match attrs.get_mut("queue") {
                    Some(Value::Array(items, ..)) => Arc::make_mut(items).push(value),
                    _ => {
                        attrs.insert("queue".to_string(), Value::array(vec![value]));
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "receive" => {
                let mut value = Value::Nil;
                if let Some(Value::Array(items, ..)) = attrs.get_mut("queue")
                    && !items.is_empty()
                {
                    value = Arc::make_mut(items).remove(0);
                }
                Ok((value, attrs))
            }
            "close" => {
                attrs.insert("closed".to_string(), Value::Bool(true));
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Channel",
                method
            ))),
        }
    }

    // --- Supply immutable ---

    // --- Proc::Async mutable ---

    // --- Promise immutable ---

    fn native_promise(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "result" => Ok(attributes.get("result").cloned().unwrap_or(Value::Nil)),
            "status" => Ok(attributes
                .get("status")
                .cloned()
                .unwrap_or(Value::Str("Planned".to_string()))),
            "then" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                let status = attributes
                    .get("status")
                    .cloned()
                    .unwrap_or(Value::Str("Planned".to_string()));
                if matches!(status, Value::Str(ref s) if s == "Kept") {
                    let value = attributes.get("result").cloned().unwrap_or(Value::Nil);
                    let result = self.call_sub_value(block, vec![value], false)?;
                    Ok(self.make_promise_instance("Kept", result))
                } else {
                    Ok(self.make_promise_instance("Planned", Value::Nil))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Promise",
                method
            ))),
        }
    }

    // --- Channel immutable ---

    fn native_channel(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "closed" => attributes
                .get("closed")
                .cloned()
                .unwrap_or(Value::Bool(false)),
            _ => Value::Nil,
        }
    }

    // --- Proc::Async immutable ---

    fn native_proc_async(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "command" => attributes
                .get("cmd")
                .cloned()
                .unwrap_or(Value::array(Vec::new())),
            "started" => attributes
                .get("started")
                .cloned()
                .unwrap_or(Value::Bool(false)),
            "stdout" => attributes.get("stdout").cloned().unwrap_or(Value::Nil),
            "stderr" => attributes.get("stderr").cloned().unwrap_or(Value::Nil),
            _ => Value::Nil,
        }
    }

    // --- Proc immutable ---

    fn native_proc(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "exitcode" => attributes.get("exitcode").cloned().unwrap_or(Value::Nil),
            "signal" => attributes.get("signal").cloned().unwrap_or(Value::Int(0)),
            "command" => attributes
                .get("command")
                .cloned()
                .unwrap_or(Value::array(Vec::new())),
            "pid" => attributes.get("pid").cloned().unwrap_or(Value::Nil),
            "Numeric" | "Int" => attributes
                .get("exitcode")
                .cloned()
                .unwrap_or(Value::Int(-1)),
            "Bool" => {
                let exitcode = match attributes.get("exitcode") {
                    Some(Value::Int(c)) => *c,
                    _ => -1,
                };
                Value::Bool(exitcode == 0)
            }
            "Str" | "gist" => {
                let exitcode = match attributes.get("exitcode") {
                    Some(Value::Int(c)) => *c,
                    _ => -1,
                };
                Value::Str(exitcode.to_string())
            }
            _ => Value::Nil,
        }
    }

    // --- Distro ---

    fn native_distro(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" | "auth" | "desc" | "release" | "path-sep" | "is-win" | "version"
            | "signature" => Ok(attributes.get(method).cloned().unwrap_or(Value::Nil)),
            "gist" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let v = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            Value::version_parts_to_string(parts)
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                Ok(Value::Str(format!("{} ({})", n, v)))
            }
            "Str" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Str(n))
            }
            "raku" | "perl" => {
                let release = attributes
                    .get("release")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_sep = attributes
                    .get("path-sep")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes
                    .get("auth")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let ver = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            format!("v{}", Value::version_parts_to_string(parts))
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                let desc = attributes
                    .get("desc")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Str(format!(
                    "Distro.new(release => \"{}\", path-sep => \"{}\", name => \"{}\", auth => \"{}\", version => {}, signature => Blob, desc => \"{}\")",
                    release, path_sep, n, auth, ver, desc
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Distro",
                method
            ))),
        }
    }

    // --- Perl ---

    fn native_perl(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "compiler" => {
                let mut compiler_attrs = HashMap::new();
                compiler_attrs.insert("name".to_string(), Value::Str("mutsu".to_string()));
                compiler_attrs.insert(
                    "auth".to_string(),
                    Value::Str("github.com/tokuhirom".to_string()),
                );
                compiler_attrs.insert(
                    "version".to_string(),
                    Value::Version {
                        parts: vec![
                            crate::value::VersionPart::Num(0),
                            crate::value::VersionPart::Num(1),
                            crate::value::VersionPart::Num(0),
                        ],
                        plus: false,
                        minus: false,
                    },
                );
                compiler_attrs.insert(
                    "signature".to_string(),
                    Value::make_instance("Blob".to_string(), {
                        let mut a = HashMap::new();
                        a.insert("values".to_string(), Value::array(vec![Value::Int(0)]));
                        a
                    }),
                );
                compiler_attrs.insert(
                    "desc".to_string(),
                    Value::Str("mutsu Raku interpreter".to_string()),
                );
                compiler_attrs.insert("release".to_string(), Value::Str("0.1.0".to_string()));
                compiler_attrs.insert("codename".to_string(), Value::Str("mutsu".to_string()));
                compiler_attrs.insert("id".to_string(), Value::Str(String::new()));
                Value::make_instance("Compiler".to_string(), compiler_attrs)
            }
            "backend" => Value::Str("mutsu".to_string()),
            "gist" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let version = attributes
                    .get("version")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(format!("{} ({})", name, version))
            }
            "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(name)
            }
            "raku" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(format!("{}.new(...)", name))
            }
            _ => attributes.get(method).cloned().unwrap_or(Value::Nil),
        }
    }

    // --- IO::Socket::INET ---

    fn native_socket_inet(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let handle_id = attributes.get("handle").and_then(|v| {
            if let Value::Int(i) = v {
                Some(*i as usize)
            } else {
                None
            }
        });
        match method {
            "getpeername" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if state.closed {
                    return Err(RuntimeError::new("Socket is closed"));
                }
                if let Some(ref stream) = state.socket {
                    let addr = stream
                        .peer_addr()
                        .map_err(|e| RuntimeError::new(format!("getpeername failed: {}", e)))?;
                    Ok(Value::Str(addr.to_string()))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "close" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                state.closed = true;
                state.socket = None;
                Ok(Value::Bool(true))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::INET",
                method
            ))),
        }
    }

    fn native_encoding_builtin(attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "name" => attributes
                .get("name")
                .cloned()
                .unwrap_or(Value::Str(String::new())),
            "alternative-names" => attributes
                .get("alternative-names")
                .cloned()
                .unwrap_or_else(|| Value::array(Vec::new())),
            "encoder" => {
                // Return a stub encoder object that supports encode-chars
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::Str(enc_name));
                Value::make_instance("Encoding::Encoder".to_string(), attrs)
            }
            "decoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::Str(enc_name));
                Value::make_instance("Encoding::Decoder".to_string(), attrs)
            }
            "gist" | "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(format!("Encoding::Builtin<{}>", name))
            }
            "WHAT" => Value::Package("Encoding::Builtin".to_string()),
            _ => Value::Nil,
        }
    }

    fn native_encoding_encoder(
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Value {
        match method {
            "encode-chars" => {
                // Stub: encode the string as UTF-8 bytes and return a Buf
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let bytes: Vec<Value> = input.bytes().map(|b| Value::Int(b as i64)).collect();
                Value::array(bytes)
            }
            "WHAT" => Value::Package("Encoding::Encoder".to_string()),
            _ => Value::Nil,
        }
    }

    fn native_encoding_decoder(
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Value {
        match method {
            "decode-chars" => {
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::Str(input)
            }
            "WHAT" => Value::Package("Encoding::Decoder".to_string()),
            _ => Value::Nil,
        }
    }

    /// Background event loop for Supply.act on live supplies (e.g., signal).
    /// Receives events from the channel and calls the callback.
    /// If the callback calls `exit`, terminates the entire process.
    pub(super) fn run_supply_act_loop(
        interp: &mut Interpreter,
        rx: &std::sync::mpsc::Receiver<SupplyEvent>,
        cb: &Value,
        delay_seconds: f64,
    ) {
        use std::io::Write;
        while let Ok(SupplyEvent::Emit(value)) = rx.recv() {
            Self::sleep_for_supply_delay(delay_seconds);
            let _ = interp.call_sub_value(cb.clone(), vec![value], true);
            // Flush stdout
            if !interp.output.is_empty() {
                print!("{}", interp.output);
                let _ = std::io::stdout().flush();
                interp.output.clear();
            }
            // Flush stderr
            if !interp.stderr_output.is_empty() {
                eprint!("{}", interp.stderr_output);
                let _ = std::io::stderr().flush();
                interp.stderr_output.clear();
            }
            // If the callback called exit, terminate the process
            if interp.halted {
                std::process::exit(interp.exit_code as i32);
            }
        }
    }
}
