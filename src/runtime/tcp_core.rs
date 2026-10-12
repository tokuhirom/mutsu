//! The TCP half shared by `IO::Socket::Async` (`native_methods::socket_async`,
//! `methods_collection_ops::socket_thread`) and the socket ops of the
//! Asynchronous `nqp::` family (`nqp_ops_socket`, ADR-11502 §2.4.4): binding
//! a listener, its accept loop, and an outbound connect. What happens to an
//! accepted connection (a Supply emit, or a queue push) stays with the caller.

use std::net::{SocketAddr, TcpListener, TcpStream, ToSocketAddrs};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

/// Why a listener could not be bound.
pub(crate) enum TcpBindError {
    /// The host resolves to no address.
    Resolve,
    Io(std::io::Error),
}

/// Bind a listener on `host:port` (port 0: one the OS picks).
///
/// The host is resolved to ONE address and exactly that is bound.
/// `TcpListener::bind` on a `&str` walks every address the host resolves to
/// and takes the first that succeeds — and `localhost` resolves to both
/// `[::1]` and `127.0.0.1`. So re-listening on a port whose previous listener
/// was still bound to the first address quietly bound the *other* one instead
/// of reporting the conflict: two listeners for one `localhost:port`
/// coexisted, a client reached whichever its own resolution picked (so a
/// *stopped* server kept answering), and a third round finally failed with
/// EADDRINUSE once both stacks were taken. Binding a single resolved address
/// makes a genuinely-busy port a real error, which is retried briefly in case
/// a recently closed listener thread still holds it.
// Cost: O(1), plus the resolution and up to 10 bind attempts 20ms apart.
pub(crate) fn bind_tcp_listener(host: &str, port: u16) -> Result<TcpListener, TcpBindError> {
    let bind_addr = format!("{host}:{port}")
        .to_socket_addrs()
        .ok()
        .and_then(|mut addrs| addrs.next())
        .ok_or(TcpBindError::Resolve)?;
    let mut attempt = 0;
    loop {
        match TcpListener::bind(bind_addr) {
            Ok(listener) => return Ok(listener),
            Err(e) if e.kind() == std::io::ErrorKind::AddrInUse && attempt < 9 => {
                attempt += 1;
                crate::gc::block_quiescent(|| std::thread::sleep(Duration::from_millis(20)));
            }
            Err(e) => return Err(TcpBindError::Io(e)),
        }
    }
}

/// How an accept loop ended.
pub(crate) enum AcceptEnd {
    /// `closed` was set and the backlog is drained.
    Closed,
    /// A fatal accept error.
    Failed,
    /// The connection handler asked to stop.
    Stopped,
}

/// Accept connections on `listener` until `closed` is set, handing each to
/// `on_conn` (which answers `false` to stop at once). The accept polls every
/// 10ms, GC-quiescently, so the flag is seen.
///
/// Once the flag is seen the backlog is drained one last time before the
/// loop ends. The OS completes the TCP handshake without this thread's help,
/// so a peer whose `connect` has already returned is an ESTABLISHED
/// connection sitting in the backlog -- dropping it because the close won the
/// poll race would lose a connection the client believes it made.
// Cost: O(c), c = connections accepted, plus one poll per 10ms.
pub(crate) fn accept_loop(
    listener: &TcpListener,
    closed: &AtomicBool,
    mut on_conn: impl FnMut(TcpStream, SocketAddr) -> bool,
) -> AcceptEnd {
    let _ = listener.set_nonblocking(true);
    let mut draining = false;
    loop {
        if !draining && closed.load(Ordering::SeqCst) {
            draining = true;
        }
        match listener.accept() {
            Ok((stream, peer)) => {
                if !on_conn(stream, peer) {
                    return AcceptEnd::Stopped;
                }
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                if draining {
                    return AcceptEnd::Closed;
                }
                crate::gc::block_quiescent(|| std::thread::sleep(Duration::from_millis(10)));
            }
            Err(_) => return AcceptEnd::Failed,
        }
    }
}

/// Open an outbound TCP connection. Every resolved address is tried in turn,
/// so a host that resolves to both IPv6 and IPv4 (`localhost` on a dual-stack
/// box) still connects when only one family has a listener.
// Cost: O(a), a = resolved addresses, each attempt bounded by 10s.
pub(crate) fn connect_tcp(host: &str, port: u16) -> std::io::Result<TcpStream> {
    let addr = format!("{host}:{port}");
    let addrs: Vec<_> = addr.to_socket_addrs()?.collect();
    let mut last_err = std::io::Error::new(
        std::io::ErrorKind::AddrNotAvailable,
        format!("no addresses found for '{addr}'"),
    );
    for sock_addr in addrs {
        match TcpStream::connect_timeout(&sock_addr, Duration::from_secs(10)) {
            Ok(s) => return Ok(s),
            Err(e) => last_err = e,
        }
    }
    Err(last_err)
}
