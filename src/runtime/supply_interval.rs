//! `Supply.interval($seconds, $delay?, :$scheduler)`: a supply that ticks
//! `0, 1, 2, ...` every `$seconds` (ADR-11276 §9.196, a class-method row).

use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;

impl Interpreter {
    /// `Supply.interval`: driven by the scheduler a `:scheduler` (or a user
    /// `$*SCHEDULER`) names, else by the process-wide shared interval timer.
    // Cost: O(1) to build the supply; each tick is O(1).
    pub(crate) fn dispatch_supply_interval(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let seconds = args.first().map_or(1.0, |value| {
            if let Some(i) = value.as_int() {
                i as f64
            } else if let Some(n) = value.as_num() {
                n
            } else {
                value.to_string_value().parse::<f64>().unwrap_or(1.0)
            }
        });
        let period_secs = if seconds.is_finite() && seconds > 0.0 {
            seconds
        } else {
            0.001 // minimum timer resolution
        };
        // Second argument: initial delay before first emission (default 0)
        let initial_delay = args.get(1).map_or(0.0, |value| {
            if let Some(i) = value.as_int() {
                i as f64
            } else if let Some(n) = value.as_num() {
                n
            } else {
                value.to_string_value().parse::<f64>().unwrap_or(0.0)
            }
        });
        let initial_delay = if initial_delay.is_finite() && initial_delay >= 0.0 {
            initial_delay
        } else {
            0.0
        };

        // Check for :scheduler named argument
        // An explicit `:scheduler` wins. Otherwise, Rakudo uses a
        // user-defined dynamic `$*SCHEDULER`; built-in schedulers keep
        // the shared timer path used below.
        // A `CurrentThreadScheduler` cannot honour `:every`, so ticking
        // through it makes the tap die exactly as Rakudo's does.
        let scheduler = Self::named_value(&args, "scheduler")
            .or_else(|| self.user_scheduler())
            .or_else(|| self.current_thread_scheduler());

        if let Some(sched) = scheduler {
            // Scheduler-driven `Supply.interval`: the scheduler owns the
            // clock, so tapping this Supply cues a tick block on it
            // (see `cue_scheduler_interval`) rather than starting a
            // timer. Back it with a supplier so the tap registers and
            // the cued ticks emit through the ordinary path.
            let mut attrs = HashMap::new();
            attrs.insert("values".to_string(), Value::array(Vec::new()));
            attrs.insert("taps".to_string(), Value::array(Vec::new()));
            attrs.insert("live".to_string(), Value::FALSE);
            attrs.insert("scheduler".to_string(), sched);
            attrs.insert("scheduler_interval".to_string(), Value::num(period_secs));
            attrs.insert("scheduler_delay".to_string(), Value::num(initial_delay));
            attrs.insert(
                "supplier_id".to_string(),
                Value::int(super::native_methods::next_supplier_id() as i64),
            );
            return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
        }

        let supply_id = super::native_methods::next_supply_id();
        let (tx, rx) = super::native_methods::supply_channel::supply_event_channel();
        if let Ok(mut map) = super::native_methods::supply_channel_map_pub().lock() {
            map.insert(supply_id, rx);
        }

        // Ticks are driven by the process-wide shared interval timer
        // (one deadline-heap thread), not a sleep-loop thread per
        // interval instance. The entry dies when the receiver drops.
        // A refused timer driver thread is a catchable X::AdHoc
        // (#9401).
        if let Err(e) = super::native_methods::interval_timer::register_interval(
            std::time::Duration::from_secs_f64(period_secs),
            std::time::Duration::from_secs_f64(initial_delay),
            tx,
        ) {
            super::native_methods::discard_supply_channel(supply_id);
            return Err(crate::runtime::builtins_system::refused_thread_error(e));
        }

        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("supply_id".to_string(), Value::int(supply_id as i64));
        attrs.insert("live".to_string(), Value::FALSE);
        return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
    }
}
