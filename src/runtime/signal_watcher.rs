/// Signal watching infrastructure for Raku's `signal()` function.
///
/// Uses a self-pipe pattern: signal handlers write the signal number to a pipe,
/// and a reader thread picks it up and sends it through the Supply channel.
// Only the `unix` self-pipe implementation emits Supply events.
#[cfg(unix)]
use crate::runtime::native_methods::SupplyEvent;
use crate::value::Value;

#[cfg(unix)]
mod unix_impl {
    use super::*;
    use std::sync::{Mutex, OnceLock};

    /// A registered signal watcher: supply_id, sender, and the Value to emit.
    struct SignalRegistration {
        supply_id: u64,
        tx: crate::runtime::native_methods::supply_channel::SupplySender,
        value: Value,
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
            let mut fds = [0i32; 2];
            unsafe {
                libc::pipe(fds.as_mut_ptr());
                // Make write end non-blocking so signal handler never blocks
                let flags = libc::fcntl(fds[1], libc::F_GETFL);
                libc::fcntl(fds[1], libc::F_SETFL, flags | libc::O_NONBLOCK);
            }
            // Start the reader thread. Registered as a GC mutator
            // (`spawn_gc_helper_thread`): `dispatch_signal` clones registered
            // `Value`s (potential Gc nodes) but never runs user VM code, so
            // the default stack suffices; the blocking pipe read is a
            // quiescent safe region so the daemon never stalls a
            // stop-the-world.
            let read_fd = fds[0];
            crate::runtime::builtins_system::spawn_gc_helper_thread("signal-rd", move || {
                signal_reader_thread(read_fd)
            });
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
                    let _ = reg.tx.send(SupplyEvent::Emit(reg.value.clone()));
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
                if reg.tx.is_retired() {
                    orphaned.push(reg.supply_id);
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
                .any(|slot| slot.registrations.iter().any(|reg| reg.supply_id == *sid))
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
    ) {
        // Ensure signal pipe is set up
        get_signal_pipe();
        register_watcher(signum, supply_id, tx, value);
    }

    /// The registry-and-disposition half of [`register_signal`], split out so a
    /// unit test can exercise it without `get_signal_pipe` registering the
    /// reader thread as a GC mutator for the rest of the process (a parked
    /// mutator changes what the collector's own tests measure).
    fn register_watcher(
        signum: i32,
        supply_id: u64,
        tx: crate::runtime::native_methods::supply_channel::SupplySender,
        value: Value,
    ) {
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
            slot.registrations.push(SignalRegistration {
                supply_id,
                tx,
                value,
            });
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
    mod tests {
        use super::*;
        use crate::runtime::native_methods::supply_channel::supply_event_channel;

        /// A slot built without touching the process's dispositions: the
        /// zeroed `previous_action` is `SIG_DFL` with no flags, which is what
        /// an unwatched signal already has.
        fn slot(registrations: Vec<SignalRegistration>) -> SignalSlot {
            SignalSlot {
                registrations,
                previous_action: unsafe { std::mem::zeroed() },
            }
        }

        #[test]
        fn the_sweep_drops_retired_registrations_and_keeps_live_ones() {
            let (live_tx, live_template) = supply_event_channel();
            let _live_tap = live_template.subscribe();
            let (dead_tx, dead_template) = supply_event_channel();
            drop(dead_template.subscribe());

            let mut registry = std::collections::HashMap::new();
            registry.insert(
                libc::SIGUSR2,
                slot(vec![
                    SignalRegistration {
                        supply_id: 1,
                        tx: live_tx,
                        value: Value::int(1),
                    },
                    SignalRegistration {
                        supply_id: 2,
                        tx: dead_tx,
                        value: Value::int(2),
                    },
                ]),
            );

            let mut orphaned = Vec::new();
            sweep_locked(&mut registry, &mut orphaned);

            assert_eq!(orphaned, vec![2]);
            let slot = registry.get(&libc::SIGUSR2).expect("slot kept");
            assert_eq!(slot.registrations.len(), 1);
            assert_eq!(slot.registrations[0].supply_id, 1);
        }

        #[test]
        fn a_signal_that_loses_its_last_registration_leaves_the_registry() {
            let (tx, template) = supply_event_channel();
            drop(template.subscribe());

            let mut registry = std::collections::HashMap::new();
            registry.insert(
                libc::SIGUSR2,
                slot(vec![SignalRegistration {
                    supply_id: 7,
                    tx,
                    value: Value::int(7),
                }]),
            );

            let mut orphaned = Vec::new();
            sweep_locked(&mut registry, &mut orphaned);

            assert!(registry.is_empty());
            assert_eq!(orphaned, vec![7]);
        }

        #[test]
        fn an_untapped_supply_is_left_alone() {
            // `signal(SIGTERM)` that nobody has tapped *yet* must keep its
            // registration: its first tap may still be coming.
            let (tx, _template) = supply_event_channel();

            let mut registry = std::collections::HashMap::new();
            registry.insert(
                libc::SIGUSR2,
                slot(vec![SignalRegistration {
                    supply_id: 3,
                    tx,
                    value: Value::int(3),
                }]),
            );

            let mut orphaned = Vec::new();
            sweep_locked(&mut registry, &mut orphaned);

            assert!(orphaned.is_empty());
            assert_eq!(registry[&libc::SIGUSR2].registrations.len(), 1);
        }

        #[test]
        fn a_real_registration_is_gone_once_its_last_tap_is() {
            // End to end over the process-global registry, including the
            // sigaction install and restore: `roast/S17-procasync/stress.t`
            // runs 1200 of these cycles and every one of them used to stay.
            let (tx, template) = supply_event_channel();
            let tap = template.subscribe();
            register_watcher(libc::SIGUSR2, 4242, tx, Value::int(0));
            assert_eq!(live_registrations(libc::SIGUSR2), 1);

            drop(tap);
            drop(template);
            sweep_retired_registrations();
            assert_eq!(live_registrations(libc::SIGUSR2), 0);
        }

        #[test]
        fn a_multi_signal_supply_is_orphaned_only_once_every_signum_is_gone() {
            // `signal(SIGINT, SIGTERM)` registers the same supply id twice;
            // discarding its channel while the other registration still lives
            // would cut off a tap that is still running.
            let (retired_tx, retired_template) = supply_event_channel();
            drop(retired_template.subscribe());
            let (live_tx, live_template) = supply_event_channel();
            let _live_tap = live_template.subscribe();

            let mut registry = std::collections::HashMap::new();
            registry.insert(
                libc::SIGUSR1,
                slot(vec![SignalRegistration {
                    supply_id: 9,
                    tx: retired_tx,
                    value: Value::int(9),
                }]),
            );
            registry.insert(
                libc::SIGUSR2,
                slot(vec![SignalRegistration {
                    supply_id: 9,
                    tx: live_tx,
                    value: Value::int(9),
                }]),
            );

            let mut orphaned = Vec::new();
            sweep_locked(&mut registry, &mut orphaned);

            assert!(orphaned.is_empty(), "channel still serves the live signal");
            assert!(!registry.contains_key(&libc::SIGUSR1));
            assert!(registry.contains_key(&libc::SIGUSR2));
        }
    }
}

#[cfg(unix)]
pub(super) use unix_impl::register_signal;
#[cfg(unix)]
pub(crate) use unix_impl::sweep_retired_registrations;

/// No-op stub for non-Unix platforms (e.g., WASM).
#[cfg(not(unix))]
pub(super) fn register_signal(
    _signum: i32,
    _supply_id: u64,
    _tx: crate::runtime::native_methods::supply_channel::SupplySender,
    _value: Value,
) {
    // Signal handling is not available on non-Unix platforms
}

/// No-op stub for non-Unix platforms (e.g., WASM).
#[cfg(not(unix))]
pub(crate) fn sweep_retired_registrations() {}
