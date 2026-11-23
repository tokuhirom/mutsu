//! The `:control` supplier of `Supply.throttle`: its `limit:N` directive.

use super::native_methods::*;

/// A push sink on a throttle's `:control` supplier, registered by the thread
/// that builds the throttle, before the worker that waits on it is submitted.
///
/// The `limit:N` directive is usually emitted right after `throttle` returns
/// (`$control.emit("limit: 2")`), and a live `Supplier` with no taps keeps no
/// history to replay. Registering from the worker raced that emit: under load
/// the worker started after it, missed the directive and held every job for
/// its whole wait (`roast/S17-supply/throttle.t` tests 9-10 in parallel CI,
/// #18181).
pub(super) struct ControlLimitSink {
    ctrl_id: u64,
    waker: crate::value::waker::ReactWaker,
    sink_id: u64,
}

impl ControlLimitSink {
    // Cost: O(r), r = directives the control supplier replays.
    pub(super) fn register(ctrl_id: u64) -> Self {
        let waker = crate::value::waker::ReactWaker::new();
        let sink_id = supplier_sink_register(ctrl_id, 0, &waker, true);
        Self {
            ctrl_id,
            waker,
            sink_id,
        }
    }

    /// Block until a `limit:N` directive arrives (or the 30 s bound elapses)
    /// and return `N`, `0` on timeout. Later emits wake the wait at once.
    // Cost: O(e), e = events drained; the wait itself is bounded at 30 s.
    pub(super) fn wait_limit(self) -> i64 {
        let mut current_limit: i64 = 0;
        let start = crate::thread_compat::Instant::now();
        while current_limit == 0 && start.elapsed() < std::time::Duration::from_secs(30) {
            for (_, event, _) in self.waker.drain() {
                if let crate::value::waker::SinkEvent::Emit(val) = event
                    && let Some(rest) = val.to_string_value().strip_prefix("limit:")
                    && let Ok(n) = rest.trim().parse::<i64>()
                {
                    current_limit = n;
                }
            }
            if current_limit == 0 {
                self.waker.wait_activity(std::time::Duration::from_millis(100));
            }
        }
        supplier_sink_unregister(self.ctrl_id, self.sink_id);
        current_limit
    }
}
