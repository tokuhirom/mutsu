mod collation_temporal;
mod encoding_rotor_toggle;
mod first_polymod_tree;
mod grep;
mod minmax_extrema;
mod socket_inet_proc;
mod socket_thread;
pub(crate) mod sort;
mod tail_rotate;
mod thread_ops;
mod unique_squish;

use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

use std::sync::Mutex;

/// Compute 0-based indices of filtered items within the original list.
pub(crate) fn compute_grep_indices(original_items: &[Value], filtered: &Value) -> Vec<usize> {
    let filtered_items = if let ValueView::Array(items, ..) = filtered.view() {
        items.to_vec()
    } else {
        return vec![];
    };
    let mut indices = Vec::new();
    let mut scan_from = 0usize;
    for needle in &filtered_items {
        if let Some(rel) = original_items[scan_from..]
            .iter()
            .position(|candidate| crate::runtime::utils::values_identical(candidate, needle))
        {
            let absolute = scan_from + rel;
            indices.push(absolute);
            scan_from = absolute.saturating_add(1);
        }
    }
    indices
}

/// Adverb mode for grep: controls what is returned.
enum GrepAdverb {
    /// :v (default) — return matching values
    V,
    /// :k — return indices of matching elements
    K,
    /// :kv — return alternating index, value pairs
    Kv,
    /// :p — return index => value Pairs
    P,
}

impl GrepAdverb {
    /// Transform a grep result (array of matched values) into the adverb-specific form.
    /// `indices` contains the 0-based positions of matched items in the original list.
    fn transform_result(&self, filtered: Value, indices: &[usize]) -> Result<Value, RuntimeError> {
        match self {
            GrepAdverb::V => Ok(filtered),
            GrepAdverb::K => {
                let idx_vals: Vec<Value> = indices.iter().map(|&i| Value::int(i as i64)).collect();
                Ok(Value::array(idx_vals))
            }
            GrepAdverb::Kv => {
                let items = if let ValueView::Array(items, ..) = filtered.view() {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    result.push(Value::int(*i as i64));
                    result.push(item.clone());
                }
                Ok(Value::array(result))
            }
            GrepAdverb::P => {
                let items = if let ValueView::Array(items, ..) = filtered.view() {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    // The key is the Int index (`3 => v`), not a Str ("3" => v).
                    result.push(Value::value_pair(Value::int(*i as i64), item.clone()));
                }
                Ok(Value::array(result))
            }
        }
    }
}

static THREAD_HANDLES: std::sync::LazyLock<
    Mutex<HashMap<u64, crate::runtime::thread_compat::JoinHandle<()>>>,
> = std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

static NEXT_THREAD_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

/// Ids of `Thread`s whose code has been handed to an OS thread, so
/// `Thread.run` can refuse to start the same `Thread` twice (rakudo: "it is an
/// error to run a thread that has already been started"). Separate from
/// `THREAD_HANDLES`, which a `.finish`/`.join` empties and which never holds an
/// `app_lifetime` thread at all.
static STARTED_THREADS: std::sync::LazyLock<Mutex<std::collections::HashSet<u64>>> =
    std::sync::LazyLock::new(|| Mutex::new(std::collections::HashSet::new()));

/// Allocate the next process-unique `Thread` id. `Thread.new` allocates one up
/// front (rakudo reports a real `.id` on a not-yet-run thread), and
/// `Thread.start` allocates one as it spawns.
pub(in crate::runtime) fn next_thread_id() -> u64 {
    NEXT_THREAD_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

/// Mark `thread_id` as started. Returns false if it already was.
pub(in crate::runtime) fn claim_thread_start(thread_id: u64) -> bool {
    STARTED_THREADS
        .lock()
        .map(|mut set| set.insert(thread_id))
        .unwrap_or(true)
}

/// Join every still-running non-`app_lifetime` `Thread`.
///
/// Rakudo's `Thread.new`/`.start` default `:!app_lifetime` means "the process
/// will only terminate when the thread has finished" (`Type/Thread.rakudoc`),
/// so the mainline running out of statements is *not* enough to end the
/// program. Verified against raku v2026.06: a fire-and-forget
/// `Thread.start({ sleep 1; say "..." })` reliably prints before exit, while
/// the same thread with `:app_lifetime` never does, and neither `exit` nor an
/// uncaught exception waits.
pub(crate) fn join_outstanding_threads() {
    loop {
        let handle = {
            let mut handles = match THREAD_HANDLES.lock() {
                Ok(handles) => handles,
                Err(_) => return,
            };
            let next = handles.keys().min().copied();
            match next {
                Some(id) => handles.remove(&id),
                None => return,
            }
        };
        match handle {
            // STW-aware, exactly like `Thread.finish`: a thread blocked joining
            // counts as quiescent for the GC's cooperative stop-the-world.
            Some(handle) => {
                let _ = crate::gc::block_quiescent(|| handle.join());
            }
            None => return,
        }
    }
}

/// The OS thread ID of the initial (main) thread, captured at first access.
static INITIAL_THREAD_ID: std::sync::LazyLock<std::thread::ThreadId> =
    std::sync::LazyLock::new(|| std::thread::current().id());

/// Returns true if the current OS thread is the initial (main) thread.
pub(crate) fn is_initial_thread() -> bool {
    std::thread::current().id() == *INITIAL_THREAD_ID
}

// Thread-local mutsu thread ID. Set by Thread.start for spawned threads.
thread_local! {
    static MUTSU_THREAD_ID: std::cell::Cell<i64> = const { std::cell::Cell::new(0) };
}

/// Set the mutsu thread ID for the current thread.
pub(super) fn set_current_mutsu_thread_id(id: i64) {
    MUTSU_THREAD_ID.with(|cell| cell.set(id));
}

/// Get the mutsu thread ID for the current thread.
/// Returns 1 for the main thread, the assigned ID for spawned threads.
pub(crate) fn current_mutsu_thread_id() -> i64 {
    let id = MUTSU_THREAD_ID.with(|cell| cell.get());
    if id == 0 {
        if is_initial_thread() {
            MUTSU_THREAD_ID.with(|cell| cell.set(1));
            1
        } else {
            // Fallback for threads not started via Thread.start
            let thread_id = std::thread::current().id();
            let id_str = format!("{:?}", thread_id);
            id_str
                .chars()
                .filter(|c| c.is_ascii_digit())
                .collect::<String>()
                .parse()
                .unwrap_or(0)
        }
    } else {
        id
    }
}
