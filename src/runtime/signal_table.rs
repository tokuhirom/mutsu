//! The process signals, as MoarVM's `nqp::getsignals` lists them.
//!
//! Rakudo builds every signal-facing type from that one op: the `Signal`
//! enum, `Kernel.signals` and `Kernel.signal`. mutsu keeps the list here once
//! and derives each of them, and `nqp::getsignals` itself, from it.
//!
//! The order is MoarVM's fixed name list; a signal this platform does not
//! have is listed with number 0 (`SIGEMT` on Linux, `SIGSTKFLT` on macOS).

/// `(name, number)` for every signal MoarVM knows, in its order.
#[cfg(not(target_os = "macos"))]
pub(crate) const SIGNALS: [(&str, i64); 35] = [
    ("SIGHUP", 1),
    ("SIGINT", 2),
    ("SIGQUIT", 3),
    ("SIGILL", 4),
    ("SIGTRAP", 5),
    ("SIGABRT", 6),
    ("SIGEMT", 0),
    ("SIGFPE", 8),
    ("SIGKILL", 9),
    ("SIGBUS", 7),
    ("SIGSEGV", 11),
    ("SIGSYS", 31),
    ("SIGPIPE", 13),
    ("SIGALRM", 14),
    ("SIGTERM", 15),
    ("SIGURG", 23),
    ("SIGSTOP", 19),
    ("SIGTSTP", 20),
    ("SIGCONT", 18),
    ("SIGCHLD", 17),
    ("SIGTTIN", 21),
    ("SIGTTOU", 22),
    ("SIGIO", 29),
    ("SIGXCPU", 24),
    ("SIGXFSZ", 25),
    ("SIGVTALRM", 26),
    ("SIGPROF", 27),
    ("SIGWINCH", 28),
    ("SIGINFO", 0),
    ("SIGUSR1", 10),
    ("SIGUSR2", 12),
    ("SIGTHR", 0),
    ("SIGSTKFLT", 16),
    ("SIGPWR", 30),
    ("SIGBREAK", 0),
];

/// `(name, number)` for every signal MoarVM knows, in its order.
#[cfg(target_os = "macos")]
pub(crate) const SIGNALS: [(&str, i64); 35] = [
    ("SIGHUP", 1),
    ("SIGINT", 2),
    ("SIGQUIT", 3),
    ("SIGILL", 4),
    ("SIGTRAP", 5),
    ("SIGABRT", 6),
    ("SIGEMT", 7),
    ("SIGFPE", 8),
    ("SIGKILL", 9),
    ("SIGBUS", 10),
    ("SIGSEGV", 11),
    ("SIGSYS", 12),
    ("SIGPIPE", 13),
    ("SIGALRM", 14),
    ("SIGTERM", 15),
    ("SIGURG", 16),
    ("SIGSTOP", 17),
    ("SIGTSTP", 18),
    ("SIGCONT", 19),
    ("SIGCHLD", 20),
    ("SIGTTIN", 21),
    ("SIGTTOU", 22),
    ("SIGIO", 23),
    ("SIGXCPU", 24),
    ("SIGXFSZ", 25),
    ("SIGVTALRM", 26),
    ("SIGPROF", 27),
    ("SIGWINCH", 28),
    ("SIGINFO", 29),
    ("SIGUSR1", 30),
    ("SIGUSR2", 31),
    ("SIGTHR", 0),
    ("SIGSTKFLT", 0),
    ("SIGPWR", 0),
    ("SIGBREAK", 0),
];

/// The number of the signal called `name`, with or without its `SIG`
/// prefix; 0 for a name this platform does not have.
// Cost: O(s), s = SIGNALS.len() (a constant 35).
pub(crate) fn signal_number(name: &str) -> i64 {
    let bare = name.strip_prefix("SIG").unwrap_or(name);
    SIGNALS
        .iter()
        .find(|(n, _)| &n[3..] == bare)
        .map_or(0, |(_, num)| *num)
}

/// The signal names indexed by number (`Kernel.signals`): slot `n` holds the
/// name of signal `n`, `None` where no signal has that number (slot 0 always).
// Cost: O(s), s = SIGNALS.len().
pub(crate) fn names_by_number() -> Vec<Option<&'static str>> {
    let max = SIGNALS.iter().map(|(_, n)| *n).max().unwrap_or(0) as usize;
    let mut slots = vec![None; max + 1];
    for (name, num) in SIGNALS {
        if num > 0 {
            slots[num as usize] = Some(name);
        }
    }
    slots
}
