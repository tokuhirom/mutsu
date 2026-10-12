//! The Unix self-pipe signal watcher behind `signal()` and `nqp::signal`.

use super::*;
use crate::runtime::Interpreter;
use crate::runtime::nqp_ops_async::QueueSink;
use std::sync::{Mutex, OnceLock};

/// A registered signal watcher: where a delivered signal goes.
enum SignalRegistration {
    /// A `signal()` Supply: emit `value` (the `Signal` enum value) to it.
    Supply {
        supply_id: u64,
        tx: crate::runtime::native_methods::supply_channel::SupplySender,
        value: Value,
    },
    /// An `nqp::signal` handle (ADR-11502 §2.4): push `[schedulee,
    /// signum]` onto its `ConcBlockingQueue`.
    Queue(QueueSink),
}

impl SignalRegistration {
    /// Whether nothing can receive from this registration any more.
    // Cost: O(1).
    fn is_retired(&self) -> bool {
        match self {
            SignalRegistration::Supply { tx, .. } => tx.is_retired(),
            SignalRegistration::Queue(sink) => sink.is_cancelled(),
        }
    }

    /// The supply id whose channel the registration feeds, if any.
    // Cost: O(1).
    fn supply_id(&self) -> Option<u64> {
        match self {
            SignalRegistration::Supply { supply_id, .. } => Some(*supply_id),
            SignalRegistration::Queue(_) => None,
        }
    }

    /// Deliver signal `signum`.
    // Cost: O(1) (one channel send).
    fn deliver(&self, signum: i32) {
        match self {
            SignalRegistration::Supply { tx, value, .. } => {
                let _ = tx.send(SupplyEvent::Emit(value.clone()));
            }
            SignalRegistration::Queue(sink) => {
                sink.push(vec![Interpreter::boot_int(signum as i64)]);
            }
        }
    }
}

/// Everything the watcher holds for one signal number: the live
/// registrations, plus the disposition that was in force before the first
/// of them replaced it, so the last one to go can put it back.
struct SignalSlot {
    registrations: Vec<SignalRegistration>,
    previous_action: libc::sigaction,
}

/// Global registry of signal watchers per signal number.
type SignalRegistry = Mutex<std::collections::HashMap<i32, SignalSlot>>;

fn signal_registry() -> &'static SignalRegistry {
    static REG: OnceLock<SignalRegistry> = OnceLock::new();
    REG.get_or_init(|| Mutex::new(std::collections::HashMap::new()))
}

/// Self-pipe for communicating signals from the handler to the reader thread.
static SIGNAL_PIPE: OnceLock<(i32, i32)> = OnceLock::new();

/// Whether this process has ever registered a signal watcher. The teardown
/// sweep is called from every react loop exit and every `Tap.close`, so the
/// overwhelmingly common "this program never called `signal()`" case must
/// not even take the registry lock.
static EVER_REGISTERED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

fn get_signal_pipe() -> (i32, i32) {
    *SIGNAL_PIPE.get_or_init(|| {
        let fds = crate::runtime::cloexec_pipe::cloexec_pipe().unwrap_or([-1; 2]);
        unsafe {
            // Make write end non-blocking so signal handler never blocks
            let flags = libc::fcntl(fds[1], libc::F_GETFL);
            libc::fcntl(fds[1], libc::F_SETFL, flags | libc::O_NONBLOCK);
        }
        (fds[0], fds[1])
    })
}

/// Start the process-lifetime reader thread on first use. Registered as a
/// GC mutator (`try_spawn_gc_helper_thread`): `dispatch_signal` clones
/// registered `Value`s (potential Gc nodes) but never runs user VM code,
/// so the default stack suffices; the blocking pipe read is a quiescent
/// safe region so the daemon never stalls a stop-the-world.
///
/// A refused spawn (#9401) is reported to the caller and leaves the reader
/// unstarted, so the next `signal()` tries again instead of the process
/// panicking.
fn ensure_reader_thread(read_fd: i32) -> std::io::Result<()> {
    use std::sync::atomic::Ordering;
    static STARTED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    static STARTING: Mutex<()> = Mutex::new(());
    if STARTED.load(Ordering::Acquire) {
        return Ok(());
    }
    let _guard = STARTING.lock().unwrap_or_else(|e| e.into_inner());
    if STARTED.load(Ordering::Acquire) {
        return Ok(());
    }
    crate::runtime::builtins_system::try_spawn_gc_helper_thread("signal-rd", move || {
        signal_reader_thread(read_fd)
    })?;
    STARTED.store(true, Ordering::Release);
    Ok(())
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
        let n = crate::gc::block_quiescent(|| unsafe {
            libc::read(read_fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len())
        });
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
    let mut orphaned = Vec::new();
    if let Ok(mut registry) = signal_registry().lock() {
        // A send to a retired supply is a no-op anyway, so sweep first and
        // deliver only to what is still tapped.
        sweep_locked(&mut registry, &mut orphaned);
        if let Some(slot) = registry.get(&signum) {
            for reg in &slot.registrations {
                reg.deliver(signum);
            }
        }
    }
    release_orphaned_channels(&orphaned);
}

/// Drop every registration whose supply can no longer deliver to anyone,
/// and hand a signal back to its previous disposition once it has lost its
/// last live registration.
///
/// Rakudo's `signal()` Supply arms the handler at tap time and disarms it
/// when the last tap goes: with no tap in force a `SIGTERM` kills the
/// process, as it would with no `signal()` call at all. mutsu arms at
/// *creation* time instead, so without this sweep both the registration and
/// the handler survived every tap that ever existed — 1200 dead entries
/// walked on each delivered signal in `roast/S17-procasync/stress.t`, and a
/// signal quietly swallowed by a supply nothing could receive from.
///
/// Supply ids whose last registration went away are collected into
/// `orphaned`; their channel entries are released outside the registry lock
/// by [`release_orphaned_channels`].
fn sweep_locked(
    registry: &mut std::collections::HashMap<i32, SignalSlot>,
    orphaned: &mut Vec<u64>,
) {
    registry.retain(|&signum, slot| {
        slot.registrations.retain(|reg| {
            if reg.is_retired() {
                orphaned.extend(reg.supply_id());
                false
            } else {
                true
            }
        });
        if slot.registrations.is_empty() {
            // SAFETY: `previous_action` is what `sigaction` handed back
            // when this slot installed our handler.
            unsafe {
                libc::sigaction(signum, &slot.previous_action, std::ptr::null_mut());
            }
            false
        } else {
            true
        }
    });
    // A `signal(SIGINT, SIGTERM)` supply has one registration per signal
    // number: it is only orphaned once every one of them is gone.
    orphaned.retain(|sid| {
        !registry
            .values()
            .any(|slot| {
                slot.registrations
                    .iter()
                    .any(|reg| reg.supply_id() == Some(*sid))
            })
    });
}

/// Drop the supply-channel registry entries of supplies the sweep orphaned.
/// Taken outside the registry lock: `discard_supply_channel` takes the
/// supply-channel map's lock, and nothing else in this module orders the
/// two.
fn release_orphaned_channels(orphaned: &[u64]) {
    for &supply_id in orphaned {
        crate::runtime::native_methods::discard_supply_channel(supply_id);
    }
}

/// Register a signal handler that emits to a Supply channel.
pub(in crate::runtime) fn register_signal(
    signum: i32,
    supply_id: u64,
    tx: crate::runtime::native_methods::supply_channel::SupplySender,
    value: Value,
) -> std::io::Result<()> {
    register(
        signum,
        SignalRegistration::Supply {
            supply_id,
            tx,
            value,
        },
    )
}

/// Register a signal handler that pushes onto an `nqp::signal` queue
/// (ADR-11502 §2.4). It shares the registry, the handler and the
/// disposition restore with [`register_signal`]; it retires once its
/// task is cancelled.
pub(in crate::runtime) fn register_signal_queue(
    signum: i32,
    sink: QueueSink,
) -> std::io::Result<()> {
    register(signum, SignalRegistration::Queue(sink))
}

fn register(signum: i32, registration: SignalRegistration) -> std::io::Result<()> {
    // Ensure the signal pipe and its reader thread are set up before the
    // handler is installed, so a refused reader thread leaves the signal's
    // default disposition in place.
    let (read_fd, _) = get_signal_pipe();
    ensure_reader_thread(read_fd)?;
    register_watcher(signum, registration);
    Ok(())
}

/// The registry-and-disposition half of [`register_signal`], split out so a
/// unit test can exercise it without `get_signal_pipe` registering the
/// reader thread as a GC mutator for the rest of the process (a parked
/// mutator changes what the collector's own tests measure).
fn register_watcher(signum: i32, registration: SignalRegistration) {
    EVER_REGISTERED.store(true, std::sync::atomic::Ordering::Relaxed);

    // Register the watcher
    let mut orphaned = Vec::new();
    if let Ok(mut registry) = signal_registry().lock() {
        // Retire what the previous `signal()` calls left behind, so the
        // registry stays bounded by the number of *live* taps.
        sweep_locked(&mut registry, &mut orphaned);
        let slot = registry.entry(signum).or_insert_with(|| SignalSlot {
            registrations: Vec::new(),
            // Install the handler and remember what it displaced. Done
            // only for a signal we are not already watching, so a second
            // `signal()` call cannot record our own handler as the
            // disposition to restore.
            // SAFETY: `sa` is a zeroed sigaction carrying a valid handler,
            // and `previous` is a live, writable sigaction.
            previous_action: unsafe {
                let mut sa: libc::sigaction = std::mem::zeroed();
                let mut previous: libc::sigaction = std::mem::zeroed();
                sa.sa_sigaction = signal_handler as *const () as usize;
                sa.sa_flags = libc::SA_RESTART;
                libc::sigemptyset(&mut sa.sa_mask);
                libc::sigaction(signum, &sa, &mut previous);
                previous
            },
        });
        slot.registrations.push(registration);
    }
    release_orphaned_channels(&orphaned);
}

/// Retire the registrations of supplies that have lost their last tap.
///
/// Called from `Tap.close`, which is the one teardown mutsu observes
/// synchronously: it makes the disposition go back promptly there rather
/// than at whatever later moment the next `signal()` call or delivered
/// signal happens to sweep.
pub(crate) fn sweep_retired_registrations() {
    if !EVER_REGISTERED.load(std::sync::atomic::Ordering::Relaxed) {
        return;
    }
    let mut orphaned = Vec::new();
    if let Ok(mut registry) = signal_registry().lock() {
        sweep_locked(&mut registry, &mut orphaned);
    }
    release_orphaned_channels(&orphaned);
}

/// How many live registrations this signal number has. Test-only: the
/// registry is otherwise entirely internal.
#[cfg(test)]
fn live_registrations(signum: i32) -> usize {
    signal_registry()
        .lock()
        .map(|registry| {
            registry
                .get(&signum)
                .map(|slot| slot.registrations.len())
                .unwrap_or(0)
        })
        .unwrap_or(0)
}

#[cfg(test)]
#[path = "signal_watcher_unix_tests.rs"]
mod tests;
