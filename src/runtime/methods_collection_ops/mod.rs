mod collation_temporal;
mod encoding_rotor_toggle;
mod first_polymod_tree;
mod grep;
mod minmax_extrema;
mod socket_inet_proc;
mod socket_thread;
mod sort;
mod tail_rotate;
mod unique_squish;

use super::*;
use crate::symbol::Symbol;

use std::sync::Mutex;

/// Compute 0-based indices of filtered items within the original list.
pub(crate) fn compute_grep_indices(original_items: &[Value], filtered: &Value) -> Vec<usize> {
    let filtered_items = if let Value::Array(items, ..) = filtered {
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
                let idx_vals: Vec<Value> = indices.iter().map(|&i| Value::Int(i as i64)).collect();
                Ok(Value::array(idx_vals))
            }
            GrepAdverb::Kv => {
                let items = if let Value::Array(items, ..) = &filtered {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    result.push(Value::Int(*i as i64));
                    result.push(item.clone());
                }
                Ok(Value::array(result))
            }
            GrepAdverb::P => {
                let items = if let Value::Array(items, ..) = &filtered {
                    items.to_vec()
                } else {
                    vec![filtered]
                };
                let mut result = Vec::new();
                for (i, item) in indices.iter().zip(items.iter()) {
                    result.push(Value::Pair(i.to_string(), Box::new(item.clone())));
                }
                Ok(Value::array(result))
            }
        }
    }
}

static THREAD_HANDLES: std::sync::LazyLock<Mutex<HashMap<u64, std::thread::JoinHandle<()>>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

static NEXT_THREAD_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);

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
