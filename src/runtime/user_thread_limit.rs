//! Fault injection for a process thread limit (`ulimit -u`), which an OS
//! limit cannot drive deterministically: the ecosystem sandbox sets
//! `ulimit -u 400`, but a root process (every local container) is exempt from
//! `RLIMIT_NPROC`, and the limit counts every thread the user owns, other
//! processes' included.
//!
//! `MUTSU_USER_THREAD_LIMIT=N` refuses a user-code thread (a pool worker, a
//! `Thread.start` thread) as the OS would with `EAGAIN` once `N` of them are
//! alive. Read once; unset means no limit. It only ever refuses more than the
//! OS does, so it cannot widen anything.

use std::sync::OnceLock;
use std::sync::atomic::{AtomicUsize, Ordering};

/// User-code threads currently alive (counted only while a limit is set).
static LIVE: AtomicUsize = AtomicUsize::new(0);

fn limit() -> Option<usize> {
    static LIMIT: OnceLock<Option<usize>> = OnceLock::new();
    *LIMIT.get_or_init(|| {
        std::env::var("MUTSU_USER_THREAD_LIMIT")
            .ok()
            .and_then(|v| v.trim().parse().ok())
    })
}

/// A slot under the injected limit, held by the thread it was taken for and
/// released when that thread's body returns or unwinds.
pub(crate) struct Slot(bool);

impl Drop for Slot {
    fn drop(&mut self) {
        if self.0 {
            LIVE.fetch_sub(1, Ordering::AcqRel);
        }
    }
}

/// Take a slot for a new user-code thread, or `Err` when the injected limit is
/// reached. Without a limit, an uncounted slot.
// Cost: O(1).
pub(crate) fn take_slot() -> Result<Slot, std::io::Error> {
    let Some(limit) = limit() else {
        return Ok(Slot(false));
    };
    let taken = LIVE.try_update(Ordering::AcqRel, Ordering::Acquire, |n| {
        (n < limit).then_some(n + 1)
    });
    match taken {
        Ok(_) => Ok(Slot(true)),
        Err(_) => Err(std::io::Error::from(std::io::ErrorKind::WouldBlock)),
    }
}
