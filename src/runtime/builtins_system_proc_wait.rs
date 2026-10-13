//! Waiting for a child process on the main program's thread, where a worker's
//! pending `exit` has to be able to end the wait (ADR-11617, #13092).

use crate::value::RuntimeError;

/// Wait for `child` to exit. On the main program's thread (the one that adopts
/// a worker's `exit`) the wait gives up when such an `exit` is pending, and the
/// child is left running, as a Raku `exit` leaves it; the caller unwinds. A
/// blocking `waitpid` cannot be woken, so that thread polls `try_wait` instead,
/// starting at 50 µs and doubling up to `exit_handover::POLL`. Every other
/// thread waits as before.
// Cost: O(1) plus the wait; the adopter wakes O(log(POLL / 50 µs)) times, then
// once per POLL.
pub(crate) fn wait_child(
    child: &mut std::process::Child,
) -> Result<std::io::Result<std::process::ExitStatus>, RuntimeError> {
    // The browser build runs no child processes and has no worker threads.
    #[cfg(not(target_arch = "wasm32"))]
    if crate::exit_handover::is_adopter_thread() {
        let mut pause = std::time::Duration::from_micros(50);
        loop {
            match child.try_wait() {
                Ok(Some(status)) => return Ok(Ok(status)),
                Ok(None) => {}
                Err(e) => return Ok(Err(e)),
            }
            if crate::exit_handover::requested() {
                return Err(RuntimeError::exit_handover_pending());
            }
            std::thread::park_timeout(pause);
            pause = (pause * 2).min(crate::exit_handover::POLL);
        }
    }
    Ok(child.wait())
}
