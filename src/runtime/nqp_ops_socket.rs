//! The socket ops of the Asynchronous `nqp::` family (#11502, ADR-11502
//! §2.4.4): `nqp::asynclisten`, `nqp::asyncconnect`, `nqp::asyncreadbytes`
//! and `nqp::asyncwritebytes`, plus `nqp::closefh` of an OS handle.
//!
//! Each takes a `ConcBlockingQueue`, a schedulee and an `AsyncTask` handle
//! type, answers a handle of that type, and pushes `[schedulee, args...]`:
//!
//! - `asynclisten($q, $s, $host, $port, $backlog, $type)`: once listening,
//!   `[$s, BOOTIO, BOOTStr, BOOTStr, BOOTInt, server, host, port]` (the
//!   connection slots null); then per connection `[$s, conn, BOOTStr,
//!   peer-host, peer-port, server, host, port]`;
//! - `asyncconnect($q, $s, $host, $port, $type)`: `[$s, conn, BOOTStr,
//!   peer-host, peer-port, host, port]`, or `[$s, BOOTIO, error, BOOTStr,
//!   BOOTInt, BOOTStr, BOOTInt]`;
//! - `asyncreadbytes($h, $q, $s, $buf_type, $type)`: `[$s, seq, Buf,
//!   BOOTStr]` per chunk, then `[$s, seq, BOOTStr, BOOTStr]` at the end (or
//!   the read error in the last slot);
//! - `asyncwritebytes($h, $q, $s, $buf, $type)`: `[$s, bytes written,
//!   BOOTStr]`, or `[$s, BOOTInt, error]`. A process handle's write goes to
//!   the child's stdin.
//!
//! The bare `BOOTStr` / `BOOTInt` / `BOOTIO` in these are the type objects,
//! MoarVM's null. A connection is a `BOOTIO` over a stream in the same
//! registry `IO::Socket::Async` connections use; the bind, the accept loop
//! and the connect are `IO::Socket::Async`'s (`runtime::tcp_core`), and the
//! read loop is the process pipes' (`runtime::proc_spawn::pump_pipe`).

use crate::runtime::native_methods::{get_tcp_stream, next_async_socket_id, register_tcp_stream};
use crate::runtime::nqp_ops_async::{QueueSink, check_arity, handle_class, operand, queue_operand};
use crate::runtime::nqp_ops_proc::OS_HANDLE_CLASS;
use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};
use std::collections::HashMap;
use std::net::{SocketAddr, TcpStream};
use std::sync::atomic::Ordering;

/// The attribute of a `BOOTIO` holding its socket's registry id.
const SOCKET_ATTR: &str = "socket";

/// A type object, MoarVM's null in a pushed event.
// Cost: O(1).
fn null(type_name: &str) -> Value {
    Value::package(Symbol::intern(type_name))
}

/// A `BOOTIO` over socket `id` (`None`: a handle with no stream of its own,
/// a listener's).
// Cost: O(1).
fn socket_handle(id: Option<u64>) -> Value {
    let mut attrs = HashMap::new();
    if let Some(id) = id {
        attrs.insert(SOCKET_ATTR.to_string(), Value::int(id as i64));
    }
    Value::make_instance(Symbol::intern(OS_HANDLE_CLASS), attrs)
}

/// The registry id of a socket handle.
// Cost: O(1).
fn socket_id(handle: &Value) -> Option<u64> {
    match operand(handle).view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name.as_str() == OS_HANDLE_CLASS => attributes
            .as_map()
            .get(SOCKET_ATTR)
            .and_then(Value::as_int)
            .map(|id| id as u64),
        _ => None,
    }
}

/// Whether `v` is an OS handle (`BOOTIO`).
// Cost: O(1).
fn is_os_handle(v: &Value) -> bool {
    matches!(operand(v).view(), ValueView::Instance { class_name, .. }
        if class_name.as_str() == OS_HANDLE_CLASS)
}

/// MoarVM's refusal of a non-handle: `<what> requires an object with REPR
/// MVMOSHandle (got T with REPR R)`.
// Cost: O(1).
fn not_a_handle(what: &str, v: &Value) -> RuntimeError {
    let v = operand(v);
    let (name, repr) = match v.view() {
        ValueView::Instance { class_name, .. } => (class_name.as_str().to_string(), "P6opaque"),
        ValueView::Int(_) | ValueView::BigInt(_) => ("Int".to_string(), "P6opaque"),
        ValueView::Str(_) => ("Str".to_string(), "P6opaque"),
        _ => (crate::value::type_name::value_type_name(&v).to_string(), "P6opaque"),
    };
    RuntimeError::new(format!(
        "{what} requires an object with REPR MVMOSHandle (got {name} with REPR {repr})"
    ))
}

/// The address `host` resolves to first, as `nqp::asyncconnect` /
/// `nqp::asynclisten` check it before going asynchronous.
// Cost: O(1), plus the resolution.
fn resolve(host: &str, port: u16) -> Result<SocketAddr, RuntimeError> {
    use std::net::ToSocketAddrs;
    let failure = |detail: String| {
        RuntimeError::new(format!(
            "Failed to resolve host name '{host}' with family 0.\nError: {detail}"
        ))
    };
    match (host, port).to_socket_addrs() {
        Ok(mut addrs) => addrs
            .next()
            .ok_or_else(|| failure("no address".to_string())),
        Err(e) => {
            let text = e.to_string();
            Err(failure(text.rsplit(": ").next().unwrap_or(&text).to_string()))
        }
    }
}

/// Register a connected stream; answers its handle, peer and local address.
// Cost: O(1).
fn adopt_stream(stream: TcpStream) -> (Value, Option<SocketAddr>, Option<SocketAddr>) {
    let peer = stream.peer_addr().ok();
    let local = stream.local_addr().ok();
    let id = next_async_socket_id();
    register_tcp_stream(id, stream);
    (socket_handle(Some(id)), peer, local)
}

/// An address as the `BOOTStr` host and `BOOTInt` port of a pushed event.
// Cost: O(1).
fn addr_parts(addr: Option<SocketAddr>) -> [Value; 2] {
    match addr {
        Some(a) => [
            Interpreter::boot_str(&a.ip().to_string()),
            Interpreter::boot_int(i64::from(a.port())),
        ],
        None => [null("BOOTStr"), null("BOOTInt")],
    }
}

impl Interpreter {
    /// The socket ops, and `nqp::closefh` of an OS handle. `None` for any
    /// other op, or a `closefh` of something else.
    pub(crate) fn call_nqp_op_socket(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // Cost: O(1) here, plus the bind; the accept thread pays O(1) per
            // connection.
            "asynclisten" => self.nqp_asynclisten(args),
            // Cost: O(1) here, plus the resolution; the connect runs on its
            // own thread.
            "asyncconnect" => self.nqp_asyncconnect(args),
            // Cost: O(1) here; the reader pays O(n), n = bytes read.
            "asyncreadbytes" => self.nqp_asyncreadbytes(args),
            // Cost: O(n), n = bytes written.
            "asyncwritebytes" => self.nqp_asyncwritebytes(args),
            // nqp::closefh($h) of a socket shuts it down; of a process, it
            // closes the child's stdin. Answers the handle.
            // Cost: O(1) + syscall.
            "closefh" if args.first().is_some_and(is_os_handle) => {
                let handle = args[0].clone();
                if let Some(id) = socket_id(&handle) {
                    if let Some(stream) = get_tcp_stream(id) {
                        let stream = stream.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                        let _ = stream.shutdown(std::net::Shutdown::Both);
                    }
                    crate::runtime::native_methods::remove_tcp_stream(id);
                } else {
                    crate::runtime::nqp_ops_proc::close_proc_stdin(&handle);
                }
                Ok(handle)
            }
            _ => return None,
        })
    }

    /// The queue sink and handle of a socket op whose queue, schedulee and
    /// handle type are at `q`, `s` and `t`.
    // Cost: O(1).
    fn socket_task(
        &mut self,
        op: &str,
        args: &[Value],
        (q, s, t): (usize, usize, usize),
    ) -> Result<(QueueSink, Value), RuntimeError> {
        let queue = queue_operand(op, "target queue", &args[q])?;
        let class = handle_class(self, op, &args[t])?;
        let (handle, cancelled) = self.new_task(class)?;
        Ok((QueueSink::new(queue, args[s].clone(), cancelled), handle))
    }

    fn nqp_asynclisten(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        check_arity("asynclisten", args, 6)?;
        let (sink, handle) = self.socket_task("asynclisten", args, (0, 1, 5))?;
        let host = operand(&args[2]).to_string_value();
        let port = crate::runtime::to_int(&operand(&args[3])).clamp(0, 65535) as u16;
        resolve(&host, port)?;
        let listener = match crate::runtime::tcp_core::bind_tcp_listener(&host, port) {
            Ok(listener) => listener,
            Err(err) => {
                let text = match err {
                    crate::runtime::tcp_core::TcpBindError::Resolve => "no address".to_string(),
                    crate::runtime::tcp_core::TcpBindError::Io(e) => {
                        crate::runtime::native_io::fs_errors::libuv_text(&e)
                    }
                };
                sink.push(vec![
                    null(OS_HANDLE_CLASS),
                    Interpreter::boot_str(&text),
                    null("BOOTStr"),
                    null("BOOTInt"),
                    null(OS_HANDLE_CLASS),
                    null("BOOTStr"),
                    null("BOOTInt"),
                ]);
                return Ok(handle);
            }
        };
        let [host, port] = addr_parts(listener.local_addr().ok());
        let server = socket_handle(None);
        sink.push(vec![
            null(OS_HANDLE_CLASS),
            null("BOOTStr"),
            null("BOOTStr"),
            null("BOOTInt"),
            server.clone(),
            host.clone(),
            port.clone(),
        ]);
        // Builds `Gc` values per connection: a registered GC mutator, its
        // accept poll quiescent. Runs no user VM code.
        crate::runtime::builtins_system::try_spawn_gc_helper_thread("nqp-listen", move || {
            let cancelled = sink.cancelled_flag();
            crate::runtime::tcp_core::accept_loop(&listener, &cancelled, |stream, _| {
                // MoarVM's connections block; some platforms hand back an
                // accepted stream non-blocking like its listener.
                let _ = stream.set_nonblocking(false);
                let (conn, peer, _) = adopt_stream(stream);
                let [peer_host, peer_port] = addr_parts(peer);
                sink.push(vec![
                    conn,
                    null("BOOTStr"),
                    peer_host,
                    peer_port,
                    server.clone(),
                    host.clone(),
                    port.clone(),
                ]);
                true
            });
        })
        .map_err(crate::runtime::builtins_system::refused_thread_error)?;
        Ok(handle)
    }

    fn nqp_asyncconnect(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        check_arity("asyncconnect", args, 5)?;
        let (sink, handle) = self.socket_task("asyncconnect", args, (0, 1, 4))?;
        let host = operand(&args[2]).to_string_value();
        let port = crate::runtime::to_int(&operand(&args[3])).clamp(0, 65535) as u16;
        resolve(&host, port)?;
        // Builds `Gc` values: a registered GC mutator, the connect quiescent.
        crate::runtime::builtins_system::try_spawn_gc_helper_thread("nqp-connect", move || {
            let connected =
                crate::gc::block_quiescent(|| crate::runtime::tcp_core::connect_tcp(&host, port));
            match connected {
                Ok(stream) => {
                    let (conn, peer, local) = adopt_stream(stream);
                    let [peer_host, peer_port] = addr_parts(peer);
                    let [host, port] = addr_parts(local);
                    sink.push(vec![conn, null("BOOTStr"), peer_host, peer_port, host, port]);
                }
                Err(e) => sink.push(vec![
                    null(OS_HANDLE_CLASS),
                    Interpreter::boot_str(&crate::runtime::native_io::fs_errors::libuv_text(&e)),
                    null("BOOTStr"),
                    null("BOOTInt"),
                    null("BOOTStr"),
                    null("BOOTInt"),
                ]),
            }
        })
        .map_err(crate::runtime::builtins_system::refused_thread_error)?;
        Ok(handle)
    }

    fn nqp_asyncreadbytes(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        check_arity("asyncreadbytes", args, 5)?;
        let what = "read bytes asynchronously";
        let id = socket_id(&args[0]).ok_or_else(|| not_a_handle(what, &args[0]))?;
        let (sink, handle) = self.socket_task("asyncreadbytes", args, (1, 2, 4))?;
        let buf_class = match operand(&args[3]).view() {
            ValueView::Instance { class_name, .. } | ValueView::Package(class_name) => class_name,
            _ => Symbol::intern("Buf"),
        };
        let stream = get_tcp_stream(id)
            .and_then(|s| {
                s.lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .try_clone()
                    .ok()
            })
            .ok_or_else(|| RuntimeError::new("Cannot read from a closed socket"))?;
        // The timeout lets a cancelled read stop between chunks.
        let _ = stream.set_read_timeout(Some(std::time::Duration::from_millis(50)));
        // Pushes `Value`s: a registered GC mutator, its reads quiescent.
        crate::runtime::builtins_system::try_spawn_gc_helper_thread("nqp-sock-read", move || {
            let cancelled = sink.cancelled_flag();
            let mut seq = 0i64;
            let error = crate::runtime::proc_spawn::pump_pipe(
                stream,
                || cancelled.load(Ordering::Acquire),
                |chunk| {
                    let buf = crate::value::value_buf::make_buf_from_bytes(buf_class, chunk);
                    sink.push(vec![Interpreter::boot_int(seq), buf, null("BOOTStr")]);
                    seq += 1;
                    true
                },
            );
            if cancelled.load(Ordering::Acquire) {
                return;
            }
            let last = match error {
                Some(e) => Interpreter::boot_str(&crate::runtime::native_io::fs_errors::libuv_text(&e)),
                None => null("BOOTStr"),
            };
            // MoarVM closes a socket whose read ended: a later write reports
            // `Cannot write to a closed socket`.
            crate::runtime::native_methods::remove_tcp_stream(id);
            sink.push(vec![Interpreter::boot_int(seq), null("BOOTStr"), last]);
        })
        .map_err(crate::runtime::builtins_system::refused_thread_error)?;
        Ok(handle)
    }

    fn nqp_asyncwritebytes(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        check_arity("asyncwritebytes", args, 5)?;
        let what = "write buffer asynchronously";
        if !is_os_handle(&args[0]) {
            return Err(not_a_handle(what, &args[0]));
        }
        let (sink, handle) = self.socket_task("asyncwritebytes", args, (1, 2, 4))?;
        let bytes = match operand(&args[3]).view() {
            ValueView::Instance { attributes, .. } => {
                crate::value::value_buf::buf_bytes_or_empty(&attributes)
            }
            _ => Vec::new(),
        };
        // The write completes before the op answers: the event is pushed in
        // the order the writes were made, as MoarVM's write queue keeps it.
        let written = match socket_id(&args[0]) {
            Some(id) => match get_tcp_stream(id) {
                Some(stream) => {
                    use std::io::Write;
                    let mut stream = stream.lock().unwrap_or_else(std::sync::PoisonError::into_inner);
                    crate::gc::block_quiescent(|| stream.write_all(&bytes)).map(|()| bytes.len())
                }
                None => Err(std::io::Error::new(
                    std::io::ErrorKind::NotConnected,
                    "Cannot write to a closed socket",
                )),
            },
            None => crate::runtime::nqp_ops_proc::write_proc_stdin(&args[0], &bytes)
                .unwrap_or_else(|| Err(std::io::Error::other("Cannot write to this handle"))),
        };
        sink.push(match written {
            Ok(n) => vec![Interpreter::boot_int(n as i64), null("BOOTStr")],
            Err(e) => {
                let text = match e.kind() {
                    std::io::ErrorKind::NotConnected | std::io::ErrorKind::Other => e.to_string(),
                    _ => crate::runtime::native_io::fs_errors::libuv_text(&e),
                };
                vec![null("BOOTInt"), Interpreter::boot_str(&text)]
            }
        });
        Ok(handle)
    }
}
