/// Signal watching infrastructure for Raku's `signal()` function.
///
/// Uses a self-pipe pattern: signal handlers write the signal number to a pipe,
/// and a reader thread picks it up and sends it through the Supply channel.
use crate::runtime::native_methods::SupplyEvent;
use crate::value::Value;
use std::sync::mpsc;
use std::sync::{Mutex, OnceLock};

/// A registered signal watcher: supply_id, sender, and the Value to emit.
struct SignalRegistration {
    #[allow(dead_code)]
    supply_id: u64,
    tx: mpsc::Sender<SupplyEvent>,
    value: Value,
}

/// Global registry of signal watchers per signal number.
type SignalRegistry = Mutex<std::collections::HashMap<i32, Vec<SignalRegistration>>>;

fn signal_registry() -> &'static SignalRegistry {
    static REG: OnceLock<SignalRegistry> = OnceLock::new();
    REG.get_or_init(|| Mutex::new(std::collections::HashMap::new()))
}

/// Self-pipe for communicating signals from the handler to the reader thread.
static SIGNAL_PIPE: OnceLock<(i32, i32)> = OnceLock::new();

fn get_signal_pipe() -> (i32, i32) {
    *SIGNAL_PIPE.get_or_init(|| {
        let mut fds = [0i32; 2];
        unsafe {
            libc::pipe(fds.as_mut_ptr());
            // Make write end non-blocking so signal handler never blocks
            let flags = libc::fcntl(fds[1], libc::F_GETFL);
            libc::fcntl(fds[1], libc::F_SETFL, flags | libc::O_NONBLOCK);
        }
        // Start the reader thread
        let read_fd = fds[0];
        std::thread::spawn(move || signal_reader_thread(read_fd));
        (fds[0], fds[1])
    })
}

/// Signal handler: writes the signal number byte to the pipe.
/// This is async-signal-safe (only calls write).
extern "C" fn signal_handler(signum: libc::c_int) {
    if let Some(&(_, write_fd)) = SIGNAL_PIPE.get() {
        let byte = signum as u8;
        unsafe {
            libc::write(write_fd, &byte as *const u8 as *const libc::c_void, 1);
        }
    }
}

/// Reader thread: reads signal numbers from the pipe and dispatches to registered supplies.
fn signal_reader_thread(read_fd: i32) {
    let mut buf = [0u8; 64];
    loop {
        let n = unsafe { libc::read(read_fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
        if n <= 0 {
            // EINTR or error - just retry
            std::thread::sleep(std::time::Duration::from_millis(1));
            continue;
        }
        for &signum in &buf[..n as usize] {
            dispatch_signal(signum as i32);
        }
    }
}

/// Dispatch a received signal to all registered supplies.
fn dispatch_signal(signum: i32) {
    if let Ok(registry) = signal_registry().lock()
        && let Some(registrations) = registry.get(&signum)
    {
        for reg in registrations {
            let _ = reg.tx.send(SupplyEvent::Emit(reg.value.clone()));
        }
    }
}

/// Register a signal handler that emits to a Supply channel.
pub(super) fn register_signal(
    signum: i32,
    supply_id: u64,
    tx: mpsc::Sender<SupplyEvent>,
    value: Value,
) {
    // Ensure signal pipe is set up
    get_signal_pipe();

    // Register the watcher
    if let Ok(mut registry) = signal_registry().lock() {
        let entry = registry.entry(signum).or_default();
        entry.push(SignalRegistration {
            supply_id,
            tx,
            value,
        });
    }

    // Install the signal handler (idempotent - reinstalling is fine)
    unsafe {
        let mut sa: libc::sigaction = std::mem::zeroed();
        sa.sa_sigaction = signal_handler as usize;
        sa.sa_flags = libc::SA_RESTART;
        libc::sigemptyset(&mut sa.sa_mask);
        libc::sigaction(signum, &sa, std::ptr::null_mut());
    }
}
