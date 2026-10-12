/// Signal watching infrastructure for Raku's `signal()` function.
///
/// Uses a self-pipe pattern: signal handlers write the signal number to a pipe,
/// and a reader thread picks it up and sends it through the Supply channel.
// Only the `unix` self-pipe implementation emits Supply events.
#[cfg(unix)]
use crate::runtime::native_methods::SupplyEvent;
use crate::value::Value;

#[cfg(unix)]
#[path = "signal_watcher_unix.rs"]
mod unix_impl;

#[cfg(unix)]
pub(super) use unix_impl::{register_signal, register_signal_queue};
#[cfg(unix)]
pub(crate) use unix_impl::sweep_retired_registrations;

/// No-op stub for non-Unix platforms (e.g., WASM).
#[cfg(not(unix))]
pub(super) fn register_signal(
    _signum: i32,
    _supply_id: u64,
    _tx: crate::runtime::native_methods::supply_channel::SupplySender,
    _value: Value,
) -> std::io::Result<()> {
    // Signal handling is not available on non-Unix platforms
    Ok(())
}

/// Non-Unix platforms (e.g., WASM) have no signals to watch.
#[cfg(not(unix))]
pub(super) fn register_signal_queue(
    _signum: i32,
    _sink: crate::runtime::nqp_ops_async::QueueSink,
) -> std::io::Result<()> {
    Ok(())
}

/// No-op stub for non-Unix platforms (e.g., WASM).
#[cfg(not(unix))]
pub(crate) fn sweep_retired_registrations() {}
