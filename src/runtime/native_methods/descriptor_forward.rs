//! `Proc::Async::Pipe.native-descriptor`: hand the caller a descriptor it can
//! read the stream from (#15647). Unix only; elsewhere no descriptor exists and
//! the promise keeps its placeholder.

#[cfg(unix)]
mod unix {
    use std::collections::HashMap;
    use std::sync::OnceLock;

    /// The forwarding pipe behind one `Proc::Async::Pipe.native-descriptor`: the
    /// reader thread of the stream writes every byte it reads to `write_fd` too
    /// (from `forwarded` on), and the caller reads the other end.
    struct DescriptorForward {
        write_fd: Option<std::os::fd::OwnedFd>,
        forwarded: usize,
    }

    /// The bytes of recently ended output streams, kept so a descriptor requested
    /// after the process finished (Shell::DSL's pipe block asks from its own
    /// thread, often after a short command exited) still reads the whole output.
    /// Bounded: [`ENDED_STREAM_KEEP`] streams of at most [`ENDED_STREAM_MAX`] bytes.
    const ENDED_STREAM_KEEP: usize = 32;
    const ENDED_STREAM_MAX: usize = 1 << 20;

    type EndedStreams = std::sync::Mutex<std::collections::VecDeque<(u64, Vec<u8>)>>;

    fn ended_streams() -> &'static EndedStreams {
        static ENDED: OnceLock<EndedStreams> = OnceLock::new();
        ENDED.get_or_init(|| std::sync::Mutex::new(std::collections::VecDeque::new()))
    }

    type NativeDescriptorMap = std::sync::Mutex<HashMap<u64, DescriptorForward>>;

    fn native_descriptor_forwards() -> &'static NativeDescriptorMap {
        static MAP: OnceLock<NativeDescriptorMap> = OnceLock::new();
        MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
    }

    /// `Proc::Async::Pipe.native-descriptor` for the stream behind `supply_id`:
    /// open a close-on-exec pipe, forward the stream's bytes into it (those read so
    /// far and every later one, see [`forward_stream_bytes`]) and return the end
    /// the caller reads (and closes). A second request answers the first one's
    /// descriptor, `None` when the pipe could not be made.
    // Cost: O(1) (one pipe2 system call).
    pub(crate) fn request_native_descriptor(supply_id: u64) -> Option<i64> {
        use std::os::fd::{FromRawFd, IntoRawFd, OwnedFd};
        let mut map = native_descriptor_forwards().lock().ok()?;
        if map.contains_key(&supply_id) {
            return None;
        }
        let mut fds = [0 as std::ffi::c_int; 2];
        // SAFETY: `fds` is a valid two-element array for pipe2 to fill.
        if unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC) } != 0 {
            return None;
        }
        // SAFETY: pipe2 succeeded, so both descriptors are open and owned by us;
        // the read end leaves our ownership (the caller closes it), the write end
        // is owned by the forward until the stream ends.
        let write_fd = unsafe { OwnedFd::from_raw_fd(fds[1]) };
        let read_fd = unsafe { OwnedFd::from_raw_fd(fds[0]) }.into_raw_fd();
        let ended = ended_streams().lock().ok().and_then(|ended| {
            ended
                .iter()
                .find(|(sid, _)| *sid == supply_id)
                .map(|(_, bytes)| bytes.clone())
        });
        if let Some(bytes) = ended {
            // The stream already ended: its bytes go in from a helper thread (the
            // pipe may be smaller than the output) and the write end closes after.
            map.insert(
                supply_id,
                DescriptorForward {
                    write_fd: None,
                    forwarded: bytes.len(),
                },
            );
            let _ = std::thread::Builder::new()
                .name("proc-fd-forward".to_string())
                .spawn(move || {
                    use std::io::Write;
                    let mut out = std::fs::File::from(write_fd);
                    let _ = out.write_all(&bytes);
                });
            return Some(i64::from(read_fd));
        }
        map.insert(
            supply_id,
            DescriptorForward {
                write_fd: Some(write_fd),
                forwarded: 0,
            },
        );
        Some(i64::from(read_fd))
    }

    /// The stream reader's hook: `raw` is everything read from the stream so far.
    /// When its descriptor was requested, write the bytes not yet forwarded into
    /// the forwarding pipe; `end` closes the write end (the caller then reads EOF).
    // Cost: O(b) for the b bytes forwarded; O(1) when no descriptor was requested.
    pub(crate) fn forward_stream_bytes(supply_id: u64, raw: &[u8], end: bool) {
        use std::os::fd::AsRawFd;
        let Ok(mut map) = native_descriptor_forwards().lock() else {
            return;
        };
        let Some(forward) = map.get_mut(&supply_id) else {
            if end
                && raw.len() <= ENDED_STREAM_MAX
                && let Ok(mut ended) = ended_streams().lock()
            {
                if ended.len() >= ENDED_STREAM_KEEP {
                    ended.pop_front();
                }
                ended.push_back((supply_id, raw.to_vec()));
            }
            return;
        };
        if let Some(fd) = forward.write_fd.as_ref() {
            let mut rest = &raw[forward.forwarded.min(raw.len())..];
            while !rest.is_empty() {
                // SAFETY: `fd` is an open descriptor we own and `rest` is a valid
                // readable slice of the stated length.
                let n = unsafe { libc::write(fd.as_raw_fd(), rest.as_ptr().cast(), rest.len()) };
                if n <= 0 {
                    // The caller closed its end (or the write failed): stop
                    // forwarding; the runtime's own copy of the bytes is unaffected.
                    forward.write_fd = None;
                    break;
                }
                rest = &rest[n as usize..];
            }
            forward.forwarded = raw.len();
        }
        if end {
            forward.write_fd = None;
        }
    }

}

#[cfg(unix)]
pub(in crate::runtime) use unix::{forward_stream_bytes, request_native_descriptor};

/// No descriptors off Unix: the placeholder promise stays as it is.
#[cfg(not(unix))]
// Cost: O(1).
pub(in crate::runtime) fn request_native_descriptor(_supply_id: u64) -> Option<i64> {
    None
}

/// No forwarding off Unix.
#[cfg(not(unix))]
// Cost: O(1).
pub(in crate::runtime) fn forward_stream_bytes(_supply_id: u64, _raw: &[u8], _end: bool) {}
