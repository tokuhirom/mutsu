//! Reading an accepted or connected async TCP socket into its `.Supply`.
//!
//! Each `$conn.Supply` tap owns a [`SocketReader`]: the connection's stream
//! (shared with `tcp_stream_map`, not a `dup`ed descriptor, so a connection
//! costs one fd as in rakudo — #12981), the channel its chunks go to, and the
//! text decoder's carry-over state (the shared `ChunkDecoder`). What drives the reads differs by platform:
//!
//! - On Unix (with `libc`) one process-wide reactor thread watches every
//!   reader's socket with `poll(2)` and reads a socket only when it is
//!   readable (`super::socket_reactor`). An idle connection costs nothing but its
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
use crate::builtins::stream_decoder::{ChunkDecoder, Codec};
use crate::runtime::{Interpreter, RuntimeError};
use crate::value::Value;
use std::net::TcpStream;
use std::sync::{Arc, Mutex};

/// A connection's stream as `tcp_stream_map` holds it; writers lock it.
pub(super) type SharedStream = Arc<Mutex<TcpStream>>;

/// What a reader does after handling one read.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ReadOutcome {
    /// Keep reading.
    More,
    /// The stream ended, failed, or nobody listens any more: drop the reader.
    Finished,
}

pub(super) struct SocketReader {
    /// Held so the descriptor stays open while the reactor polls it, even
    /// after `close` has dropped the registry's entry: `close` shuts the
    /// socket down, which the reactor reads as end of stream. The reactor
    /// reads `fd` and never this field, which is the point of holding it.
    #[cfg_attr(
        all(unix, feature = "libc"),
        expect(dead_code, reason = "held only to keep `fd` open")
    )]
    pub(super) stream: SharedStream,
    /// `stream`'s descriptor, read by the reactor without taking the lock a
    /// writer may hold.
    #[cfg(all(unix, feature = "libc"))]
    pub(super) fd: std::os::fd::RawFd,
    tx: SupplySender,
    /// The text decoder (`None` for a `.Supply(:bin)` of raw `Buf`s). TCP
    /// splits wherever it likes, so a read can end mid-sequence or
    /// mid-grapheme (a "u" whose COMBINING DOT ABOVE is in the next packet);
    /// the shared chunk decoder holds either back until the next read
    /// settles it or the stream ends (#11783).
    decoder: Option<ChunkDecoder>,
}

impl SocketReader {
    pub(super) fn new(stream: SharedStream, tx: SupplySender, is_bin: bool, enc: String) -> Self {
        #[cfg(all(unix, feature = "libc"))]
        let fd = {
            use std::os::fd::AsRawFd;
            stream.lock().unwrap_or_else(|e| e.into_inner()).as_raw_fd()
        };
        Self {
            stream,
            #[cfg(all(unix, feature = "libc"))]
            fd,
            tx,
            // An encoding the decoder does not know reads as UTF-8, as it
            // did before the shared decoder.
            decoder: (!is_bin).then(|| {
                ChunkDecoder::new(Codec::from_label(&enc).unwrap_or(Codec::Utf8), false)
            }),
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
                if let Some(decoder) = &mut self.decoder {
                    match decoder.finish() {
                        Ok(text) => {
                            self.emit_text(text);
                        }
                        Err(e) => return self.quit(&e),
                    }
                }
                let _ = self.tx.send(SupplyEvent::Done);
                ReadOutcome::Finished
            }
            Ok(n) => {
                let Some(decoder) = &mut self.decoder else {
                    let chunk = Interpreter::make_buf(buf[..n].to_vec());
                    if self.tx.send(SupplyEvent::Emit(chunk)).is_err() {
                        return ReadOutcome::Finished;
                    }
                    return ReadOutcome::More;
                };
                match decoder.feed(&buf[..n]) {
                    Ok(text) => {
                        if self.emit_text(text) {
                            ReadOutcome::More
                        } else {
                            ReadOutcome::Finished
                        }
                    }
                    // Bytes the encoding cannot decode quit the Supply with
                    // the decoder's error, as in Rakudo, rather than
                    // substituting replacement characters.
                    Err(e) => self.quit(&e),
                }
            }
        }
    }

    /// Emit decoded text, unless it is empty. `false` when nobody listens.
    // Cost: O(1).
    fn emit_text(&self, text: String) -> bool {
        text.is_empty() || self.tx.send(SupplyEvent::Emit(Value::str(text))).is_ok()
    }

    /// Quit the Supply with a decoding error.
    // Cost: O(1).
    fn quit(&self, e: &RuntimeError) -> ReadOutcome {
        let _ = self.tx.send(SupplyEvent::Quit(e.exception_value()));
        ReadOutcome::Finished
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
    // This reader blocks in `read` for up to 50 ms at a time, so it reads a
    // clone of its own instead of holding the lock writers take.
    let stream = reader
        .stream
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .try_clone()
        .map_err(|e| RuntimeError::new(format!("Could not read socket: {e}")))?;
    // Registered spawn: emits freshly built `Gc` values (Buf instances)
    // whose drops on a failed send must not race a cycle scan; the blocking
    // read is a quiescent safe region.
    crate::runtime::builtins_system::try_spawn_gc_helper_thread("sock-conn", move || {
        use std::io::Read;
        let mut buf = [0u8; 4096];
        let mut stream = stream;
        let _ = stream.set_read_timeout(Some(std::time::Duration::from_millis(50)));
        loop {
            if reader.is_retired() {
                break;
            }
            // Peek first: the retirement check has to come after the data
            // shows up and before it is read.
            match crate::gc::block_quiescent(|| stream.peek(&mut buf)) {
                Err(e) if is_would_block(&e) => continue,
                Ok(n) if n > 0 && reader.is_retired() => break,
                _ => {}
            }
            let read = crate::gc::block_quiescent(|| stream.read(&mut buf));
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
