use crate::runtime::*;
use crate::symbol::Symbol;
use std::sync::atomic::Ordering;

use super::state::*;
use super::state_lock::*;
use super::state_scheduler::*;

impl Interpreter {
    fn cancellation_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "cancellation-id".to_string(),
            Value::Int(next_cancellation_id() as i64),
        );
        Value::make_instance(Symbol::intern("Cancellation"), attrs)
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

    pub(in crate::runtime) fn native_tap(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" | "close" => {
                if let Some(Value::Int(listener_id)) = attributes.get("listener-id") {
                    close_async_listener(*listener_id as u64);
                }
                Ok(Value::Nil)
            }
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::Promise(SharedPromise::new()))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or(Value::Promise(SharedPromise::new()))),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Tap",
                method
            ))),
        }
    }

    pub(in crate::runtime) fn native_cancellation(
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

    pub(in crate::runtime) fn native_scheduler(
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

    pub(in crate::runtime) fn native_fake_scheduler(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let sched_id = match attributes.get("scheduler_id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => {
                return Err(RuntimeError::new(
                    "FakeScheduler called without scheduler_id",
                ));
            }
        };
        match method {
            "cue" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let delay = Self::named_value(&args, "at")
                    .or_else(|| Self::named_value(&args, "in"))
                    .map(|v| v.to_f64())
                    .unwrap_or(0.0);
                let every = Self::named_value(&args, "every").map(|v| v.to_f64());
                fake_scheduler_cue(sched_id, callback, every, delay);
                Ok(Self::cancellation_instance())
            }
            "progress-by" => {
                let duration = args.first().map(|v| v.to_f64()).unwrap_or(0.0);
                let (callbacks, counter_values) = fake_scheduler_progress_by(sched_id, duration);
                for cb in callbacks {
                    let _ = self.call_sub_value(cb, Vec::new(), true);
                }
                if !counter_values.is_empty() {
                    return Ok(Value::array(
                        counter_values.into_iter().map(Value::Int).collect(),
                    ));
                }
                Ok(Value::Nil)
            }
            "time" => {
                let (_, _) = fake_scheduler_progress_by(sched_id, 0.0);
                Ok(Value::Num(0.0))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on FakeScheduler",
                method
            ))),
        }
    }
}
