//! The async-socket read reactor (#12865): one thread that `poll(2)`s every
//! open `IO::Socket::Async` read Supply and reads a socket only when it is
//! readable.
//!
//! It replaces one polling thread per connection, which cost an OS thread and
//! 20 wake-ups a second for as long as a connection stayed open, so a
//! keep-alive HTTP client or server piled up hundreds of them. `poll(2)` is
//! used rather than epoll/kqueue because it is the same call on every Unix
//! release target and the set is rebuilt from a plain `Vec`; a wake costs
//! O(n) over the watched sockets, which stays far below the per-connection
//! threads it replaces at the connection counts mutsu programs reach.
//!
//! Registration hands a [`SocketReader`] to the reactor through a queue and
//! writes a byte to a self-pipe so a blocked `poll` picks it up at once. A
//! reader whose taps have all gone is dropped (closing its socket clone)
//! without reading: when its socket next becomes readable, or at the next
//! sweep, which runs every [`SWEEP_INTERVAL_MS`] while any reader is
//! registered. Reads use `MSG_DONTWAIT` per call instead of switching the
//! socket to non-blocking mode, because that flag is shared with the
//! writer's clone of the same socket.

use super::socket_reader::{ReadOutcome, SocketReader};
use crate::runtime::RuntimeError;
use std::os::unix::io::{AsRawFd, RawFd};
use std::sync::{Arc, Mutex};

/// How often a reactor with registered readers wakes to drop readers whose
/// taps have all gone, so their sockets are closed promptly even when no
/// more data arrives.
const SWEEP_INTERVAL_MS: libc::c_int = 250;

struct Shared {
    incoming: Mutex<Vec<SocketReader>>,
    wake_write: RawFd,
}

static REACTOR: Mutex<Option<Arc<Shared>>> = Mutex::new(None);

/// Hand `reader` to the reactor, starting the reactor thread on first use.
// Cost: O(1) amortized.
pub(super) fn register(reader: SocketReader) -> Result<(), RuntimeError> {
    let shared = {
        let mut slot = REACTOR
            .lock()
            .map_err(|_| RuntimeError::new("socket reactor lock poisoned"))?;
        match slot.as_ref() {
            Some(shared) => Arc::clone(shared),
            None => {
                let shared = start_reactor()?;
                *slot = Some(Arc::clone(&shared));
                shared
            }
        }
    };
    if let Ok(mut incoming) = shared.incoming.lock() {
        incoming.push(reader);
    }
    wake(shared.wake_write);
    Ok(())
}

fn wake(fd: RawFd) {
    let byte = 1u8;
    // SAFETY: `fd` is the reactor's own pipe write end, open for the life of
    // the process; writing one byte from a valid stack buffer. A full pipe
    // (EAGAIN) already guarantees a pending wake-up, so the result is ignored.
    unsafe {
        libc::write(fd, (&byte as *const u8).cast(), 1);
    }
}

/// A close-on-exec, non-blocking pipe: `(read end, write end)`.
fn wake_pipe() -> std::io::Result<(RawFd, RawFd)> {
    let mut fds = [0 as libc::c_int; 2];
    // SAFETY: `fds` is a valid two-element buffer for `pipe` to fill.
    if unsafe { libc::pipe(fds.as_mut_ptr()) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    for fd in fds {
        // SAFETY: `fd` was just returned by `pipe` and is owned here.
        unsafe {
            libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC);
            let flags = libc::fcntl(fd, libc::F_GETFL);
            libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }
    }
    Ok((fds[0], fds[1]))
}

fn start_reactor() -> Result<Arc<Shared>, RuntimeError> {
    let (wake_read, wake_write) =
        wake_pipe().map_err(crate::runtime::builtins_system::refused_thread_error)?;
    let shared = Arc::new(Shared {
        incoming: Mutex::new(Vec::new()),
        wake_write,
    });
    let thread_shared = Arc::clone(&shared);
    // Registered spawn: it emits freshly built `Gc` values (Buf instances)
    // whose drops on a failed send must not race a cycle scan; `poll` is a
    // quiescent safe region.
    let spawned = crate::runtime::builtins_system::try_spawn_gc_helper_thread(
        "sock-conn",
        move || run_reactor(&thread_shared, wake_read),
    );
    if let Err(e) = spawned {
        // SAFETY: both ends were created above and nothing else holds them.
        unsafe {
            libc::close(wake_read);
            libc::close(wake_write);
        }
        return Err(crate::runtime::builtins_system::refused_thread_error(e));
    }
    Ok(shared)
}

fn run_reactor(shared: &Shared, wake_read: RawFd) {
    let mut readers: Vec<SocketReader> = Vec::new();
    let mut pollfds: Vec<libc::pollfd> = Vec::new();
    let mut buf = [0u8; 4096];
    loop {
        if let Ok(mut incoming) = shared.incoming.lock() {
            readers.append(&mut incoming);
        }
        readers.retain(|r| !r.is_retired());
        pollfds.clear();
        pollfds.push(libc::pollfd {
            fd: wake_read,
            events: libc::POLLIN,
            revents: 0,
        });
        pollfds.extend(readers.iter().map(|r| libc::pollfd {
            fd: r.fd,
            events: libc::POLLIN,
            revents: 0,
        }));
        let timeout = if readers.is_empty() {
            -1
        } else {
            SWEEP_INTERVAL_MS
        };
        let ready = crate::gc::block_quiescent(|| {
            // SAFETY: `pollfds` is a live, correctly sized array of `pollfd`
            // whose fds are owned by `readers` and the wake pipe for the call.
            unsafe { libc::poll(pollfds.as_mut_ptr(), pollfds.len() as libc::nfds_t, timeout) }
        });
        if ready <= 0 {
            continue;
        }
        if pollfds[0].revents != 0 {
            drain_wake_pipe(wake_read);
        }
        // `pollfds[i + 1]` describes `readers[i]`; walk back to front so a
        // finished reader can be swap-removed without disturbing the pairs
        // still to visit.
        for i in (0..readers.len()).rev() {
            if pollfds[i + 1].revents == 0 {
                continue;
            }
            let reader = &mut readers[i];
            // A chunk that arrives while no tap listens stays in the kernel
            // buffer for the next tap.
            if reader.is_retired() {
                readers.swap_remove(i);
                continue;
            }
            let read = recv_dontwait(reader.fd, &mut buf);
            if matches!(&read, Err(e) if super::socket_reader::is_would_block(e)) {
                continue;
            }
            if reader.handle_read(read, &buf) == ReadOutcome::Finished {
                readers.swap_remove(i);
            }
        }
    }
}

fn drain_wake_pipe(fd: RawFd) {
    let mut sink = [0u8; 64];
    // SAFETY: `fd` is the reactor's non-blocking pipe read end; `sink` is a
    // valid buffer of the length passed.
    while unsafe { libc::read(fd, sink.as_mut_ptr().cast(), sink.len()) } > 0 {}
}

/// One read that never blocks, without touching the socket's shared
/// `O_NONBLOCK` flag.
fn recv_dontwait(fd: RawFd, buf: &mut [u8]) -> std::io::Result<usize> {
    loop {
        // SAFETY: `fd` is owned by a live `SocketReader`; `buf` is a valid
        // writable buffer of the length passed.
        let n = unsafe { libc::recv(fd, buf.as_mut_ptr().cast(), buf.len(), libc::MSG_DONTWAIT) };
        if n >= 0 {
            return Ok(n as usize);
        }
        let err = std::io::Error::last_os_error();
        if err.kind() != std::io::ErrorKind::Interrupted {
            return Err(err);
        }
    }
}
