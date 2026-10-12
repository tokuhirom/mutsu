//! Reading an accepted or connected async TCP socket into its `.Supply`.
//!
//! Each `$conn.Supply` tap owns a [`SocketReader`]: the socket (a clone of the
//! stream in `tcp_stream_map`), the channel its chunks go to, and the text
//! decoder's carry-over state. What drives the reads differs by platform:
//!
//! - On Unix (with `libc`) one process-wide reactor thread watches every
//!   reader's socket with `poll(2)` and reads a socket only when it is
//!   readable ([`socket_reactor`]). An idle connection costs nothing but its
//!   slot in the poll set. This is the model Rakudo gets from libuv (#12865).
//! - Elsewhere each reader keeps its own thread, which wakes every 50 ms to
//!   notice that every tap has gone.
//!
//! Either way a reader stops *before* it takes a chunk nobody can receive:
//! rakudo reads a connection only while a tap listens, so a consumer that
//! finished (a `react` that said `done`) leaves the next chunk in the kernel
//! buffer for the next `.Supply` tap.

use super::SupplyEvent;
use super::supply_channel::SupplySender;
use crate::runtime::{Interpreter, RuntimeError};
use crate::value::Value;
use std::net::TcpStream;

/// What a reader does after handling one read.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ReadOutcome {
    /// Keep reading.
    More,
    /// The stream ended, failed, or nobody listens any more: drop the reader.
    Finished,
}

pub(super) struct SocketReader {
    pub(super) stream: TcpStream,
    tx: SupplySender,
    is_bin: bool,
    enc: String,
    // Text mode carries decoding state across reads: TCP splits wherever it
    // likes, so a read can end mid-UTF-8-sequence (`pending_bytes`) or
    // mid-grapheme (`pending_text`, e.g. a "u" whose COMBINING DOT ABOVE is
    // in the next packet). Emitting either half on its own is wrong, so both
    // are held back until the next read resolves them or the stream ends.
    pending_bytes: Vec<u8>,
    pending_text: String,
}

impl SocketReader {
    pub(super) fn new(stream: TcpStream, tx: SupplySender, is_bin: bool, enc: String) -> Self {
        Self {
            stream,
            tx,
            is_bin,
            enc,
            pending_bytes: Vec::new(),
            pending_text: String::new(),
        }
    }

    /// Every tap of this connection's Supply has gone.
    pub(super) fn is_retired(&self) -> bool {
        self.tx.is_retired()
    }

    /// Handle the result of one read of up to `buf.len()` bytes: `Ok(0)` is
    /// end of stream, `Ok(n)` the bytes in `buf[..n]`. A would-block (or
    /// timed-out) read is not an outcome; callers retry those themselves.
    // Cost: O(n), n = bytes read.
    pub(super) fn handle_read(&mut self, read: std::io::Result<usize>, buf: &[u8]) -> ReadOutcome {
        match read {
            Ok(0) | Err(_) => {
                if !self.is_bin && !self.pending_text.is_empty() {
                    let _ = self.tx.send(SupplyEvent::Emit(Value::str(std::mem::take(
                        &mut self.pending_text,
                    ))));
                }
                let _ = self.tx.send(SupplyEvent::Done);
                ReadOutcome::Finished
            }
            Ok(n) => {
                let value = if self.is_bin {
                    Some(Interpreter::make_buf(buf[..n].to_vec()))
                } else {
                    match Interpreter::decode_socket_chunk(
                        &buf[..n],
                        &self.enc,
                        &mut self.pending_bytes,
                        &mut self.pending_text,
                    ) {
                        Ok(text) => text.map(Value::str),
                        // Not UTF-8: Raku quits the Supply with the decode
                        // failure rather than substituting replacement
                        // characters.
                        Err(()) => {
                            let _ = self.tx.send(SupplyEvent::Quit(
                                Interpreter::malformed_utf8_exception(),
                            ));
                            return ReadOutcome::Finished;
                        }
                    }
                };
                if let Some(value) = value
                    && self.tx.send(SupplyEvent::Emit(value)).is_err()
                {
                    return ReadOutcome::Finished;
                }
                ReadOutcome::More
            }
        }
    }
}

pub(super) fn is_would_block(e: &std::io::Error) -> bool {
    matches!(
        e.kind(),
        std::io::ErrorKind::WouldBlock | std::io::ErrorKind::TimedOut
    )
}

/// Start reading `reader`'s socket into its Supply.
pub(super) fn start_socket_reader(reader: SocketReader) -> Result<(), RuntimeError> {
    #[cfg(all(unix, feature = "libc"))]
    {
        super::socket_reactor::register(reader)
    }
    #[cfg(not(all(unix, feature = "libc")))]
    {
        start_thread_reader(reader)
    }
}

/// The portable reader: one thread per connection, waking every 50 ms to
/// notice retirement (the timeout is a socket option shared by the stream's
/// clones; writers are unaffected by a receive timeout).
#[cfg(not(all(unix, feature = "libc")))]
fn start_thread_reader(mut reader: SocketReader) -> Result<(), RuntimeError> {
    // Registered spawn: emits freshly built `Gc` values (Buf instances)
    // whose drops on a failed send must not race a cycle scan; the blocking
    // read is a quiescent safe region.
    crate::runtime::builtins_system::try_spawn_gc_helper_thread("sock-conn", move || {
        use std::io::Read;
        let mut buf = [0u8; 4096];
        let _ = reader
            .stream
            .set_read_timeout(Some(std::time::Duration::from_millis(50)));
        loop {
            if reader.is_retired() {
                break;
            }
            // Peek first: the retirement check has to come after the data
            // shows up and before it is read.
            match crate::gc::block_quiescent(|| reader.stream.peek(&mut buf)) {
                Err(e) if is_would_block(&e) => continue,
                Ok(n) if n > 0 && reader.is_retired() => break,
                _ => {}
            }
            let read = crate::gc::block_quiescent(|| reader.stream.read(&mut buf));
            if matches!(&read, Err(e) if is_would_block(e)) {
                continue;
            }
            if reader.handle_read(read, &buf) == ReadOutcome::Finished {
                break;
            }
        }
    })
    .map(|_| ())
    .map_err(crate::runtime::builtins_system::refused_thread_error)
}
