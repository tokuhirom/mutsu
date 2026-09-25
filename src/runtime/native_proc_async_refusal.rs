//! How `Proc::Async.start` reports an OS-refused service thread (#9401).
//!
//! `start` needs up to four helper threads (`proc-in` or the stdin writer,
//! `proc-wait`, and the `proc-out`/`proc-err` readers it spawns). The child is
//! already running when one of them is refused, so the refusal must not leave
//! it behind: it is killed and reaped, and the start promise the caller holds
//! is broken with the same catchable `X::AdHoc` a refused user-code thread
//! raises (ADR-0123).
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

/// The `X::AdHoc` a broken start promise carries for a refused thread.
pub(super) fn refused_thread_exception(e: std::io::Error) -> Value {
    let message = crate::runtime::builtins_system::refused_thread_error(e)
        .message
        .to_string();
    let mut attrs = HashMap::new();
    attrs.insert("payload".to_string(), Value::str(message.clone()));
    attrs.insert("message".to_string(), Value::str(message));
    Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
}

/// Kill and reap the child `pid` whose `proc-wait` thread was refused. The
/// `Child` handle went down with the refused thread's closure, and dropping a
/// `Child` neither kills nor reaps it, so this goes by pid.
pub(super) fn kill_and_reap(pid: u32) {
    #[cfg(feature = "native")]
    // SAFETY: plain syscalls on a pid this process spawned and has not reaped
    // (its only waiter was the refused thread), so it cannot have been reused.
    unsafe {
        libc::kill(pid as i32, libc::SIGKILL);
        let mut status = 0;
        libc::waitpid(pid as i32, &mut status, 0);
    }
    #[cfg(not(feature = "native"))]
    let _ = pid;
}
