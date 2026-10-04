//! Core methods rakudo answers on a type object itself, which core modules
//! such as `Telemetry` (#9824) call without an instance:
//!
//! - `Kernel.cpu-cores` / `.free-memory` / `.total-memory`: rakudo implements
//!   them without touching an attribute. The attribute readers
//!   (`Kernel.name`, ...) still refuse on the type object, as in rakudo.
//! - `Thread.usage`: the process-wide thread counters, a native int list of
//!   started, aborted, completed, joined, yields and the highest thread id
//!   (`runtime::thread_usage`).
//! - `ThreadPoolScheduler.usage`: the type object's answer is an all-zero
//!   row of the scheduler's ten usage columns; an instance answers with the
//!   pool's figures (`native_scheduler`).

use super::*;

/// How many columns `ThreadPoolScheduler.usage` reports: supervisor, then
/// workers / queued / completed for general, timer and affinity workers.
pub(crate) const SCHEDULER_USAGE_COLUMNS: usize = 10;

impl Interpreter {
    /// Dispatch one of the type-object methods above. `None` for any other
    /// invocant or method.
    // Cost: as the native `Kernel` method (one syscall or one /proc read);
    // O(1) for the usage rows.
    pub(crate) fn try_core_type_object_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "cpu-cores" | "free-memory" | "total-memory" | "usage"
        ) {
            return None;
        }
        let ValueView::Package(name) = target.view() else {
            return None;
        };
        let class = name.resolve();
        let (clean_args, _) = self.sanitize_call_args(args);
        if !clean_args.is_empty() {
            return None;
        }
        match (class.as_str(), method) {
            ("Kernel", "cpu-cores" | "free-memory" | "total-memory") => Some(self.native_kernel(
                &crate::value::AttrMap::new(),
                method,
                Vec::new(),
            )),
            ("Thread", "usage") => Some(Ok(Self::native_int_row(
                &crate::runtime::thread_usage::thread_usage(),
            ))),
            ("ThreadPoolScheduler", "usage") => Some(Ok(Self::native_int_row(
                &[0; SCHEDULER_USAGE_COLUMNS],
            ))),
            _ => None,
        }
    }

    /// A native int list (`nqp::list_i`) holding `row`.
    // Cost: O(n), n = elements.
    pub(crate) fn native_int_row(row: &[i64]) -> Value {
        Value::nqp_typed_list(
            row.iter().map(|n| Value::int(*n)).collect(),
            crate::value::NqpElemKind::Int,
        )
    }
}
