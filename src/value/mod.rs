use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock, RwLock, Weak};

use crate::ast::{ParamDef, Stmt};
use crate::env::Env;
use crate::gc::Gc;
use crate::opcode::{CompiledCode, CompiledFunction};
use crate::symbol::Symbol;
use num_bigint::BigInt as NumBigInt;
use num_integer::Integer;
use num_traits::{Signed, ToPrimitive, Zero};
/// Global list tracking consumed Seq instances via Weak references.
/// Uses Weak<Vec<Value>> so that when the Seq is dropped, the Weak expires
/// and won't cause false positives from address reuse.
static CONSUMED_SEQS: OnceLock<Mutex<Vec<Weak<Vec<Value>>>>> = OnceLock::new();

fn consumed_seqs() -> &'static Mutex<Vec<Weak<Vec<Value>>>> {
    CONSUMED_SEQS.get_or_init(|| Mutex::new(Vec::new()))
}

/// Global set tracking "cached" Seq instances (have called .cache).
static CACHED_SEQS: OnceLock<Mutex<Vec<Weak<Vec<Value>>>>> = OnceLock::new();

fn cached_seqs() -> &'static Mutex<Vec<Weak<Vec<Value>>>> {
    CACHED_SEQS.get_or_init(|| Mutex::new(Vec::new()))
}

/// Global map from Seq Arc ptr (as usize) to a deferred iterator Value.
/// Used for `Seq.new(iterator)` to defer pulling until the Seq is consumed.
static DEFERRED_SEQ_ITERS: OnceLock<Mutex<HashMap<usize, Value>>> = OnceLock::new();

fn deferred_seq_iters() -> &'static Mutex<HashMap<usize, Value>> {
    DEFERRED_SEQ_ITERS.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Global set tracking lazy Seq instances (e.g. from Seq.from-loop without condition).
static LAZY_SEQS: OnceLock<Mutex<Vec<Weak<Vec<Value>>>>> = OnceLock::new();

fn lazy_seqs() -> &'static Mutex<Vec<Weak<Vec<Value>>>> {
    LAZY_SEQS.get_or_init(|| Mutex::new(Vec::new()))
}

/// Mark a Seq as lazy (infinite, from Seq.from-loop without condition).
pub(crate) fn seq_mark_lazy(arc_ptr: &Arc<Vec<Value>>) {
    let mut list = lazy_seqs().lock().unwrap();
    let target_ptr = Arc::as_ptr(arc_ptr);
    list.retain(|w| w.strong_count() > 0);
    for w in list.iter() {
        if let Some(existing) = w.upgrade()
            && Arc::as_ptr(&existing) == target_ptr
        {
            return; // already tracked
        }
    }
    list.push(std::sync::Arc::downgrade(arc_ptr));
}

/// Check if a Seq has been marked as lazy.
pub(crate) fn seq_is_lazy(arc_ptr: &Arc<Vec<Value>>) -> bool {
    let list = lazy_seqs().lock().unwrap();
    let target_ptr = Arc::as_ptr(arc_ptr);
    for w in list.iter() {
        if let Some(existing) = w.upgrade()
            && Arc::as_ptr(&existing) == target_ptr
        {
            return true;
        }
    }
    false
}

/// Global set tracking consumed LazyList instances (gather-based Seqs).
static CONSUMED_LAZYLISTS: OnceLock<Mutex<Vec<crate::gc::WeakGc<LazyList>>>> = OnceLock::new();

fn consumed_lazylists() -> &'static Mutex<Vec<crate::gc::WeakGc<LazyList>>> {
    CONSUMED_LAZYLISTS.get_or_init(|| Mutex::new(Vec::new()))
}

/// Register a deferred iterator for a Seq. Called by Seq.new(iterator).
pub(crate) fn seq_register_deferred_iter(arc_ptr: &Arc<Vec<Value>>, iterator: Value) {
    let key = Arc::as_ptr(arc_ptr) as usize;
    let mut map = deferred_seq_iters().lock().unwrap();
    map.insert(key, iterator);
}

/// Take the deferred iterator for a Seq (if any). Removes and returns it.
pub(crate) fn seq_take_deferred_iter(arc_ptr: &Arc<Vec<Value>>) -> Option<Value> {
    let key = Arc::as_ptr(arc_ptr) as usize;
    let mut map = deferred_seq_iters().lock().unwrap();
    map.remove(&key)
}

/// Check if a Seq has a deferred iterator.
pub(crate) fn seq_has_deferred_iter(arc_ptr: &Arc<Vec<Value>>) -> bool {
    let key = Arc::as_ptr(arc_ptr) as usize;
    let map = deferred_seq_iters().lock().unwrap();
    map.contains_key(&key)
}

/// Mark a LazyList (from gather) as consumed.
pub(crate) fn lazylist_consume(gc_ptr: &crate::gc::Gc<LazyList>) -> bool {
    let mut list = consumed_lazylists().lock().unwrap();
    let target_ptr = crate::gc::Gc::as_ptr(gc_ptr);
    list.retain(|w| w.strong_count() > 0);
    for w in list.iter() {
        if w.as_ptr() == target_ptr {
            return false; // already consumed
        }
    }
    list.push(crate::gc::Gc::downgrade(gc_ptr));
    true
}

/// Check if a LazyList has been consumed.
pub(crate) fn lazylist_is_consumed(gc_ptr: &crate::gc::Gc<LazyList>) -> bool {
    let list = consumed_lazylists().lock().unwrap();
    let target_ptr = crate::gc::Gc::as_ptr(gc_ptr);
    list.iter().any(|w| w.as_ptr() == target_ptr)
}

/// Mark a Seq as cached (called .cache on it). Cached Seqs do not get consumed.
pub(crate) fn seq_mark_cached(arc_ptr: &Arc<Vec<Value>>) {
    let mut list = cached_seqs().lock().unwrap();
    let target_ptr = Arc::as_ptr(arc_ptr);
    list.retain(|w| w.strong_count() > 0);
    for w in list.iter() {
        if let Some(existing) = w.upgrade()
            && Arc::as_ptr(&existing) == target_ptr
        {
            return; // already tracked
        }
    }
    list.push(std::sync::Arc::downgrade(arc_ptr));
}

/// Check if a Seq has been marked as cached.
pub(crate) fn seq_is_cached(arc_ptr: &Arc<Vec<Value>>) -> bool {
    let list = cached_seqs().lock().unwrap();
    let target_ptr = Arc::as_ptr(arc_ptr);
    for w in list.iter() {
        if let Some(existing) = w.upgrade()
            && Arc::as_ptr(&existing) == target_ptr
        {
            return true;
        }
    }
    false
}

/// Build a structured X::Seq::Consumed error.
pub(crate) fn seq_consumed_error() -> RuntimeError {
    seq_consumed_error_for("Seq")
}

/// Build a structured X::Seq::Consumed error naming the consumed type
/// (e.g. "Seq", "HyperSeq", "RaceSeq").
pub(crate) fn seq_consumed_error_for(type_name: &str) -> RuntimeError {
    let msg = format!(
        "The iterator of this {type_name} is already in use/consumed by another {type_name} \
         (you might solve this by adding .cache on usages of the {type_name}, or by \
         assigning the {type_name} into an array)"
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert(
        "kind".to_string(),
        Value::Package(crate::symbol::Symbol::intern(type_name)),
    );
    let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Seq::Consumed"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Mark a Seq (identified by its Arc) as consumed.
/// Returns Err if the Seq was already consumed.
pub(crate) fn seq_consume(arc_ptr: &Arc<Vec<Value>>) -> Result<(), RuntimeError> {
    let mut list = consumed_seqs().lock().unwrap();
    // Clean up expired Weak references and check for duplicates
    let target_ptr = Arc::as_ptr(arc_ptr);
    list.retain(|w| w.strong_count() > 0);
    for w in list.iter() {
        if let Some(existing) = w.upgrade()
            && Arc::as_ptr(&existing) == target_ptr
        {
            return Err(seq_consumed_error());
        }
    }
    list.push(std::sync::Arc::downgrade(arc_ptr));
    Ok(())
}

/// Check if a Seq (identified by its Arc) has been consumed.
pub(crate) fn seq_is_consumed(arc_ptr: &Arc<Vec<Value>>) -> bool {
    let list = consumed_seqs().lock().unwrap();
    let target_ptr = Arc::as_ptr(arc_ptr);
    for w in list.iter() {
        if let Some(existing) = w.upgrade()
            && Arc::as_ptr(&existing) == target_ptr
        {
            return true;
        }
    }
    false
}

/// For sink: if cached, do nothing; if consumed, do nothing (re-sink is ok);
/// if not cached and not consumed, mark as consumed.
pub(crate) fn seq_sink(arc_ptr: &Arc<Vec<Value>>) {
    if seq_is_cached(arc_ptr) {
        seq_take_deferred_iter(arc_ptr); // discard deferred iter if any
        return;
    }
    if seq_is_consumed(arc_ptr) {
        return;
    }
    let _ = seq_consume(arc_ptr);
    // Caller is responsible for pulling from deferred iter if needed.
}

/// Shared mutable attribute storage for Proxy subclasses.
pub(crate) type ProxySubclassAttrs = Arc<Mutex<HashMap<String, Value>>>;

/// Bag data: wraps HashMap<String, NumBigInt> with optional original-typed keys.
/// Implements Deref to HashMap<String, NumBigInt> so existing code works unchanged.
/// Weights are arbitrary-precision so a BagHash weight can exceed i64::MAX.
#[derive(Debug, Clone)]
pub(crate) struct BagData {
    pub counts: HashMap<String, NumBigInt>,
    /// Maps string keys back to original Values (e.g. Int(2), Bool(false)).
    /// Only populated when the Bag is created from mixed-type data.
    pub original_keys: Option<HashMap<String, Value>>,
    /// Element value-type constraint (e.g. `Int` for `BagHash[Int]`), if any.
    pub value_type: Option<String>,
    /// Key-type constraint for parameterized QuantHashes, if any.
    pub key_type: Option<String>,
    /// Declared container type name (e.g. `BagHash`, `Bag[Int]`), if any.
    /// Embedded here (not in a pointer-keyed side table) so it travels with
    /// the container through copy-on-write and can never be inherited by an
    /// unrelated container via Arc-pointer reuse.
    pub declared_type: Option<String>,
}

/// Set data: wraps HashSet<String> with optional original-typed keys.
/// Implements Deref to HashSet<String> so existing code works unchanged.
#[derive(Debug, Clone)]
pub(crate) struct SetData {
    pub elements: HashSet<String>,
    /// Maps string keys back to original Values (e.g. Int(2), Bool(false)).
    /// Only populated when the Set is created from mixed-type data.
    pub original_keys: Option<HashMap<String, Value>>,
    /// Element value-type constraint (e.g. `Str` for `SetHash[Str]`), if any.
    pub value_type: Option<String>,
    /// Key-type constraint for parameterized QuantHashes, if any.
    pub key_type: Option<String>,
    /// Declared container type name (e.g. `SetHash`, `Set[Int]`), if any.
    /// Embedded here (not in a pointer-keyed side table) so it travels with
    /// the container through copy-on-write and can never be inherited by an
    /// unrelated container via Arc-pointer reuse.
    pub declared_type: Option<String>,
}

/// Mix data: wraps HashMap<String, f64> with optional original-typed keys.
/// Implements Deref to HashMap<String, f64> so existing code works unchanged.
#[derive(Debug, Clone)]
pub(crate) struct MixData {
    pub weights: HashMap<String, f64>,
    /// Maps string keys back to original Values (e.g. Int(2), Bool(false)).
    /// Only populated when the Mix is created from mixed-type data.
    pub original_keys: Option<HashMap<String, Value>>,
    /// Element value-type constraint (e.g. `Real` for `MixHash`), if any.
    pub value_type: Option<String>,
    /// Key-type constraint for parameterized QuantHashes, if any.
    pub key_type: Option<String>,
    /// Declared container type name (e.g. `MixHash`, `Mix[Int]`), if any.
    /// Embedded here (not in a pointer-keyed side table) so it travels with
    /// the container through copy-on-write and can never be inherited by an
    /// unrelated container via Arc-pointer reuse.
    pub declared_type: Option<String>,
}

mod aliased_mut;
/// The instance-attribute map (`Symbol -> Value`); see [`AttrMap`].
mod attr_map;
mod display;
mod error;
mod error_construct;
mod error_typed;
mod guards;
/// NaN-boxed 8-byte representation core (3b-1 step B): the packed word that
/// IS the `Value` storage. The only module that knows the bit layout.
mod nanbox;
#[cfg(feature = "jit")]
pub(crate) use nanbox::jit_words;
mod serde_support;
pub(crate) mod signature;
pub(crate) mod types;
pub(crate) mod types_eqv;
pub(crate) mod types_isa;
pub(crate) mod types_truthy;
mod value_async;
mod value_collections;
mod value_enum;
mod value_eq;
mod value_gc;
mod value_instance;
mod value_lazy;
mod value_methods_a;
mod value_methods_b;
mod value_methods_c;
mod value_setbagmix;
mod view;
pub(crate) use crate::gc::gc_contents_mut;
pub(crate) use aliased_mut::arc_contents_mut;
pub(crate) use aliased_mut::gc_data_mut;
pub(crate) use attr_map::{AttrKey, AttrMap, attr_twigil_base};
pub use guards::{ArcRef, GcRef, RefGuard, WeakGcRef};
pub(in crate::value) use nanbox::NanBox;

/// A `'static` Nil for call sites that keep a `&Value` beyond one expression:
/// `&Value::NIL` stopped const-promoting once `Value` gained `Drop` (the
/// NaN-box word releases its payload). Nil owns no payload, so a static is
/// free.
pub(crate) static NIL_VALUE: Value = Value(NanBox::NIL);
pub(crate) use types::what_type_name;
pub use view::ValueView;

/// Get current time as seconds since UNIX epoch (returns 0.0 on WASM).
pub(crate) fn current_time_secs_f64() -> f64 {
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs_f64())
            .unwrap_or(0.0)
    }
    #[cfg(target_arch = "wasm32")]
    {
        0.0
    }
}

pub(crate) use display::is_internal_anon_type_name;
pub(crate) use display::user_facing_type_name;
pub use display::{format_complex, tclc_str, wordcase_segments, wordcase_str};
pub(crate) use error::expected_type_object;
pub use error::{Control, RuntimeError, RuntimeErrorCode};
// SubData is re-exported so callers can destructure Value::Sub(data)

static INSTANCE_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone)]
pub(crate) struct PendingInstanceDestroy {
    pub(crate) class_name: Symbol,
    pub(crate) attributes: AttrMap,
}

thread_local! {
    static PENDING_INSTANCE_DESTROYS: RefCell<Vec<PendingInstanceDestroy>> = const { RefCell::new(Vec::new()) };
    /// When true, suppress queuing new DESTROY items (we are already inside a DESTROY handler).
    static IN_DESTROY_HANDLER: RefCell<bool> = const { RefCell::new(false) };
}

/// Set the in-destroy-handler flag to suppress recursive DESTROY queuing.
pub(crate) fn set_in_destroy_handler(value: bool) {
    IN_DESTROY_HANDLER.with(|flag| *flag.borrow_mut() = value);
}

fn is_in_destroy_handler() -> bool {
    IN_DESTROY_HANDLER.with(|flag| *flag.borrow())
}

/// DESTROY-dedup refcounts (instance id -> live `queue_destroy` holders),
/// SHARDED by id so the per-instance bookkeeping never funnels every thread
/// through one global mutex: `InstanceAttrs::new` locks it on every user
/// instance construction and `finalize_destroy` on every instance death, and
/// under GC=on the dead sweep drops candidates in 16k-node batches while
/// worker threads keep constructing — one global lock measured 3.5-4.2s of
/// ping-pong per collect on S17-lowlevel/thread.t (vs ~40ms uncontended).
const LIVE_REFCOUNT_SHARDS: usize = 64;

fn live_instance_refcounts(id: u64) -> &'static Mutex<HashMap<u64, usize>> {
    static SHARDS: OnceLock<Vec<Mutex<HashMap<u64, usize>>>> = OnceLock::new();
    let shards = SHARDS.get_or_init(|| {
        (0..LIVE_REFCOUNT_SHARDS)
            .map(|_| Mutex::new(HashMap::new()))
            .collect()
    });
    &shards[(id as usize) & (LIVE_REFCOUNT_SHARDS - 1)]
}

/// The shared mutable attribute cell of an instance (Phase 3, Stage 1).
pub(crate) type AttrCell = Arc<RwLock<AttrMap>>;

thread_local! {
    /// Addresses of attribute cells this thread is currently holding a read
    /// guard on (with nesting; the same cell can be read-locked recursively).
    /// The writeback consults this to tell apart a *same-thread* read-then-write
    /// (which would self-deadlock on the non-reentrant `RwLock`) from legitimate
    /// *cross-thread* contention (which must block and write — e.g. concurrent
    /// `cas` on a shared instance attribute). See [`write_cell_respecting_reads`].
    static HELD_READ_CELLS: RefCell<Vec<usize>> = const { RefCell::new(Vec::new()) };

    /// Writes to instance cells that were deferred because this thread held a
    /// read guard on the cell at write time (a self-deadlock hazard). Applied —
    /// in order — when the last read guard on that cell is dropped. This keeps a
    /// legacy write-back that runs inside an outstanding `as_map()` borrow (e.g.
    /// the `$obj.attr = v` accessor, whose let-chain condition holds the guard
    /// across the write-back) from either deadlocking *or* silently dropping a
    /// real mutation.
    static PENDING_CELL_WRITES: RefCell<Vec<PendingCellWrite>> = const { RefCell::new(Vec::new()) };
}

/// A deferred cell write: `(cell address, cell, new map)`. See
/// [`PENDING_CELL_WRITES`].
type PendingCellWrite = (usize, AttrCell, AttrMap);

fn cell_addr(cell: &RwLock<AttrMap>) -> usize {
    cell as *const RwLock<AttrMap> as usize
}

/// A read guard over an instance's attribute map. Derefs to `&AttrMap`, so the
/// vast majority of read sites (`.get`, `.iter`, `.len`, `for (k, v) in ...`)
/// and `&guard` argument coercions to `&AttrMap` keep working unchanged. On
/// construction it records the cell address in [`HELD_READ_CELLS`]; on drop it
/// removes it and, when this was the last read guard on the cell, applies any
/// write that was deferred while the cell was read-locked.
pub(crate) struct AttrReadGuard<'a> {
    // `Option` so `Drop` can release the read lock *before* flushing deferred
    // writes — otherwise the flush's blocking write lock would deadlock against
    // this guard's own still-held read lock (struct fields drop after `drop`).
    guard: Option<std::sync::RwLockReadGuard<'a, AttrMap>>,
    addr: usize,
}

#[derive(Debug)]
pub(crate) struct InstanceAttrs {
    class_name: Symbol,
    /// Shared mutable attribute cell.
    ///
    /// Cross-frame *sharing* (the Raku object-identity semantics that makes an
    /// in-place mutation visible to every alias) comes from cloning the
    /// `crate::gc::Gc<InstanceAttrs>` held by `Value::Instance` — that aliases this same
    /// struct and cell. An explicit `InstanceAttrs::clone` (see the manual
    /// `Clone` impl below) instead makes an *independent* deep copy with a fresh
    /// cell, preserving the long-standing semantics of the legacy writeback
    /// sites that do `(*attributes).clone()` to build a detached "updated" map.
    attributes: AttrCell,
    id: u64,
    queue_destroy: bool,
    /// Once-guard for DESTROY queueing: `Trace::finalize` (GC-on refcount
    /// death / cycle reclaim) and Rust `Drop` (GC-off, and the eventual memory
    /// drop of a GC node) funnel into the same `finalize_destroy`; whichever
    /// runs first wins and the other becomes a no-op.
    finalized: std::sync::atomic::AtomicBool,
}

/// Type constraints for typed scalar `ContainerRef` cells, keyed by the cell's
/// `Arc` pointer. A free typed container (e.g. `my Int $` used as a value, then
/// stored as a `Pair` value) carries no name, so its `of`-type cannot live in
/// the name-keyed `var_type_constraint` map; this side table lets the
/// `ContainerRef` write chokepoint enforce the constraint and raise
/// `X::TypeCheck::Assignment` on a bad assignment. Only `my T $` typed-anonymous
/// scalars register here, so the table stays tiny.
static TYPED_CONTAINER_CONSTRAINTS: OnceLock<Mutex<HashMap<usize, String>>> = OnceLock::new();

fn typed_container_constraints() -> &'static Mutex<HashMap<usize, String>> {
    TYPED_CONTAINER_CONSTRAINTS.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Record that the `ContainerRef` cell `cell` has the `of`-type `type_name`.
pub fn register_container_constraint(cell: &crate::gc::Gc<Mutex<Value>>, type_name: &str) {
    let ptr = crate::gc::Gc::as_ptr(cell) as usize;
    typed_container_constraints()
        .lock()
        .unwrap()
        .insert(ptr, type_name.to_string());
}

/// Look up the `of`-type constraint of a `ContainerRef` cell, if any.
pub fn lookup_container_constraint(cell: &crate::gc::Gc<Mutex<Value>>) -> Option<String> {
    let ptr = crate::gc::Gc::as_ptr(cell) as usize;
    typed_container_constraints()
        .lock()
        .unwrap()
        .get(&ptr)
        .cloned()
}

/// Write `map` into `cell`, deferring if the current thread already holds a read
/// guard on it.
///
/// If this thread is mid-read of the cell, a blocking write would self-deadlock
/// (the `RwLock` is not reentrant): this happens when an attribute writeback runs
/// while an outer call frame on the same thread still holds an `as_map()` borrow
/// on this instance (e.g. the `$obj.attr = v` accessor, whose let-chain
/// condition keeps the guard alive across the write-back). We can't skip the
/// write (it may be a real mutation, not a no-op), so we *defer* it: queue it in
/// `PENDING_CELL_WRITES` and apply it when the last read guard on the cell drops.
///
/// When this thread is *not* reading the cell we take a blocking write lock,
/// which is required for correctness under genuine cross-thread contention
/// (e.g. concurrent `cas` on a shared instance attribute must not drop updates).
fn write_cell_respecting_reads(cell: &AttrCell, map: AttrMap) {
    let addr = cell_addr(cell);
    if HELD_READ_CELLS.with(|c| c.borrow().contains(&addr)) {
        PENDING_CELL_WRITES.with(|p| p.borrow_mut().push((addr, cell.clone(), map)));
        return;
    }
    *write_attrs(cell) = map;
}

/// Recover the map from a poisoned lock instead of propagating the panic. A
/// poisoned attribute lock only means some thread panicked mid-mutation; the
/// map itself is still a valid `HashMap`, and matching the codebase's other
/// `lock().unwrap()` sites would just turn a poison into a second panic.
fn read_attrs(cell: &RwLock<AttrMap>) -> AttrReadGuard<'_> {
    let guard = cell.read().unwrap_or_else(|e| e.into_inner());
    AttrReadGuard::new(guard, cell_addr(cell))
}

fn write_attrs(cell: &RwLock<AttrMap>) -> std::sync::RwLockWriteGuard<'_, AttrMap> {
    cell.write().unwrap_or_else(|e| e.into_inner())
}

pub(crate) fn next_instance_id() -> u64 {
    INSTANCE_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(crate) fn take_pending_instance_destroys() -> Vec<PendingInstanceDestroy> {
    PENDING_INSTANCE_DESTROYS.with(|pending| std::mem::take(&mut *pending.borrow_mut()))
}

/// Drop every `Gc`-bearing value held in this thread's thread-local state.
///
/// Worker threads accumulate `Gc` `Value`s in thread-local registries
/// (`PENDING_INSTANCE_DESTROYS`, `FAILURE_PENDING_REGISTRY`). Rust runs a
/// thread's TLS destructors *after* the thread closure returns — i.e. after the
/// worker's `WorkerGuard` has already called `exit_mutator_worker` and
/// unregistered the thread from the GC's stop-the-world accounting. A collector
/// on another thread could then achieve STW (believing this thread gone) and run
/// trial deletion while libc's TLS teardown concurrently drops these `Gc`s —
/// corrupting the scan's refcount bookkeeping (freeing a live node). Draining
/// them HERE, while the worker is still registered, closes that window: the drop
/// happens under the normal counted-mutator discipline (a collector defers for
/// this thread), and the later TLS destructor frees only empty collections. The
/// leftover DESTROY items are dropped without running their Raku handlers, which
/// matches the prior behavior (the TLS destructor never ran handlers either).
pub(crate) fn drop_thread_local_gc_state() {
    // Take each collection OUT of its RefCell before dropping it: an element's
    // `Drop` re-enters these same thread-locals (an instance's `finalize_destroy`
    // pushes to PENDING_INSTANCE_DESTROYS), so dropping in place while holding the
    // `borrow_mut` would double-borrow the RefCell and panic — and a panic in a
    // Drop during thread teardown aborts the process. Suppress the re-entrant
    // DESTROY queuing too, so the drain terminates instead of re-filling the queue.
    set_in_destroy_handler(true);
    let pending = PENDING_INSTANCE_DESTROYS
        .try_with(|pending| std::mem::take(&mut *pending.borrow_mut()))
        .unwrap_or_default();
    drop(pending);
    let failures = FAILURE_PENDING_REGISTRY
        .try_with(|reg| std::mem::take(&mut *reg.borrow_mut()))
        .unwrap_or_default();
    drop(failures);
    set_in_destroy_handler(false);
}

// Global registry for Failure handled state. Maps Failure instance IDs to
// their handled flag. This allows the handled state to be shared across
// all clones of a Failure value (which share the same `id`).
thread_local! {
    static FAILURE_HANDLED_REGISTRY: RefCell<HashMap<u64, bool>> = RefCell::new(HashMap::new());
}

/// Mark a Failure as handled by its instance id.
pub(crate) fn mark_failure_handled(id: u64) {
    FAILURE_HANDLED_REGISTRY.with(|reg| {
        reg.borrow_mut().insert(id, true);
    });
}

/// Set the handled state of a Failure by its instance id.
pub(crate) fn set_failure_handled(id: u64, handled: bool) {
    FAILURE_HANDLED_REGISTRY.with(|reg| {
        reg.borrow_mut().insert(id, handled);
    });
}

/// Check if a Failure is handled by its instance id.
/// Returns None if the id is not in the registry (meaning check the attribute).
pub(crate) fn is_failure_handled(id: u64) -> Option<bool> {
    FAILURE_HANDLED_REGISTRY.with(|reg| reg.borrow().get(&id).copied())
}

/// Remove a Failure from the handled registry (cleanup).
#[allow(dead_code)]
pub(crate) fn remove_failure_handled(id: u64) {
    FAILURE_HANDLED_REGISTRY.with(|reg| {
        reg.borrow_mut().remove(&id);
    });
}

// Global registry of all live Failure values, used to implement $!.pending.
// Maps Failure instance IDs to the Failure Value itself.
thread_local! {
    static FAILURE_PENDING_REGISTRY: RefCell<HashMap<u64, Value>> = RefCell::new(HashMap::new());
}

/// Register a Failure value in the pending registry.
pub(crate) fn register_pending_failure(id: u64, value: Value) {
    FAILURE_PENDING_REGISTRY.with(|reg| {
        reg.borrow_mut().insert(id, value);
    });
}

/// Return all pending (tracked) Failure values.
pub(crate) fn get_pending_failures() -> Vec<Value> {
    FAILURE_PENDING_REGISTRY.with(|reg| reg.borrow().values().cloned().collect())
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum JunctionKind {
    Any,
    All,
    One,
    None,
}

#[derive(Debug, Clone)]
pub struct SubData {
    pub package: Symbol,
    pub name: Symbol,
    pub params: Vec<String>,
    pub(crate) param_defs: Vec<ParamDef>,
    pub(crate) body: Vec<Stmt>,
    pub(crate) is_rw: bool,
    pub(crate) is_raw: bool,
    pub env: Env,
    pub(crate) assumed_positional: Vec<Value>,
    pub(crate) assumed_named: HashMap<String, Value>,
    pub id: u64,
    /// When true, this sub has an explicit empty signature `()` and should reject any arguments.
    pub(crate) empty_sig: bool,
    /// When true, this sub is a bare block `{ ... }` (not a pointy block or named sub).
    /// Bare blocks have an implicit `$_` parameter with default from outer scope.
    pub(crate) is_bare_block: bool,
    /// Pre-compiled bytecode for this closure (if compiled).
    pub(crate) compiled_code: Option<Arc<CompiledCode>>,
    /// `is DEPRECATED` message: None = not deprecated, Some(msg) = deprecated.
    pub(crate) deprecated_message: Option<String>,
    /// Source line number (1-based) where this block/sub was defined.
    pub(crate) source_line: Option<u32>,
    /// Source file path where this block/sub was defined.
    pub(crate) source_file: Option<String>,
    /// Free variables that were declared in an enclosing *loop body* when this
    /// closure was created (see `VM::loop_local_vars`). Each loop iteration is a
    /// fresh binding, so at call time these names are read from this closure's own
    /// frozen captured `env` (overwriting the caller's current value) rather than
    /// the shared lexical name — Raku's per-iteration closure capture. Empty for
    /// non-loop closures and named subs.
    pub(crate) owned_captures: Vec<Symbol>,
    /// Captured upvalue array, aligned with `compiled_code.upvalue_syms`. Built at
    /// closure-creation time (after `box_captured_lexicals`). An entry is
    /// `Some(cell)` only when the captured lexical is a shared `ContainerRef` cell
    /// (a boxed, mutated-and-shared lexical): reading the cell tracks the
    /// creator's container, so the snapshot stays correct. A non-cell capture is
    /// `None` — it cannot be safely frozen by value (an enclosing scope may
    /// reassign it after capture, e.g. a closure created in a loop over a shared
    /// outer `$n`), so `GetUpvalue` falls back to a live by-name env read instead.
    /// Installed as `Interpreter::upvalues` on closure entry. Empty for closures
    /// with no upvalue-eligible free variables.
    pub(crate) upvalues: Vec<Option<Value>>,
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    a = a.wrapping_abs();
    b = b.wrapping_abs();
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

pub fn make_rat(num: i64, den: i64) -> Value {
    if den == 0 {
        if num == 0 {
            return Value::Rat(0, 0); // NaN
        } else if num > 0 {
            return Value::Rat(1, 0); // Inf
        } else {
            return Value::Rat(-1, 0); // -Inf
        }
    }
    let g = gcd(num, den);
    let (mut n, mut d) = (num / g, den / g);
    if d < 0 {
        n = -n;
        d = -d;
    }
    Value::Rat(n, d)
}

/// Create a Rat from BigInt numerator/denominator.
/// Per Raku spec, Rat denominators are limited to uint64 range.
/// When the reduced denominator exceeds this, degrade to Num.
/// Use `make_big_fat_rat` for FatRat contexts that should never degrade.
pub fn make_big_rat(num: NumBigInt, den: NumBigInt) -> Value {
    if den.is_zero() {
        if num.is_zero() {
            return Value::Rat(0, 0);
        }
        return if num.is_positive() {
            Value::Rat(1, 0)
        } else {
            Value::Rat(-1, 0)
        };
    }
    let g = num.gcd(&den);
    let mut n = num / &g;
    let mut d = den / &g;
    if d.is_negative() {
        n = -n;
        d = -d;
    }
    if let (Some(n_i64), Some(d_i64)) = (n.to_i64(), d.to_i64()) {
        Value::Rat(n_i64, d_i64)
    } else {
        Value::bigrat(n, d)
    }
}

/// Create a Rat from BigInt numerator/denominator, used for arithmetic results.
/// Per Raku spec, Rat denominators are limited to uint64 range after reduction.
/// When the reduced denominator exceeds this, degrade to Num.
/// This is different from `make_big_rat` which preserves BigRat for literal values.
pub fn make_big_rat_arith(num: NumBigInt, den: NumBigInt) -> Value {
    if den.is_zero() {
        if num.is_zero() {
            return Value::Rat(0, 0);
        }
        return if num.is_positive() {
            Value::Rat(1, 0)
        } else {
            Value::Rat(-1, 0)
        };
    }
    let g = num.gcd(&den);
    let mut n = num / &g;
    let mut d = den / &g;
    if d.is_negative() {
        n = -n;
        d = -d;
    }
    if let (Some(n_i64), Some(d_i64)) = (n.to_i64(), d.to_i64()) {
        Value::Rat(n_i64, d_i64)
    } else if d.to_u64().is_none() || d.bits() > 64 {
        // Per Raku spec, Rat denominators are limited to uint64 range.
        // When arithmetic produces a denominator exceeding this, degrade to Num.
        // The bits() check is a redundant safety net for d.to_u64().
        Value::Num(bigrat_to_f64(&n, &d))
    } else {
        Value::bigrat(n, d)
    }
}

/// Create a FatRat from BigInt numerator/denominator.
/// Unlike `make_big_rat`, this never degrades to Num — FatRat has unlimited precision.
pub fn make_big_fat_rat(num: NumBigInt, den: NumBigInt) -> Value {
    if den.is_zero() {
        if num.is_zero() {
            return Value::Rat(0, 0);
        }
        return if num.is_positive() {
            Value::Rat(1, 0)
        } else {
            Value::Rat(-1, 0)
        };
    }
    let g = num.gcd(&den);
    let mut n = num / &g;
    let mut d = den / &g;
    if d.is_negative() {
        n = -n;
        d = -d;
    }
    if let (Some(n_i64), Some(d_i64)) = (n.to_i64(), d.to_i64()) {
        Value::Rat(n_i64, d_i64)
    } else {
        Value::bigrat(n, d)
    }
}

/// Convert a Mix/MixHash weight (f64) back to a Raku Value.
/// Returns Int for whole numbers, Rat for representable fractions, Num otherwise.
pub fn mix_weight_to_value(w: f64) -> Value {
    if w.is_nan() || w.is_infinite() {
        return Value::Num(w);
    }
    // Check for exact integer
    if w == (w as i64 as f64) && w.abs() < i64::MAX as f64 {
        return Value::Int(w as i64);
    }
    // Try to reconstruct as Rat: use the decimal representation to find
    // a rational number. Multiply by powers of 10 to clear the decimal.
    // This works for values like 42.1, 1.5, 3.14 etc.
    let s = format!("{}", w);
    if let Some(dot_pos) = s.find('.') {
        let decimals = s.len() - dot_pos - 1;
        if decimals <= 15 {
            let denom = 10i64.checked_pow(decimals as u32);
            if let Some(d) = denom {
                // Parse the string without the dot as numerator
                let without_dot: String = s.chars().filter(|c| *c != '.').collect();
                if let Ok(n) = without_dot.parse::<i64>() {
                    // Verify round-trip: n/d as f64 == w
                    if (n as f64 / d as f64 - w).abs() < f64::EPSILON * w.abs().max(1.0) {
                        return make_rat(n, d);
                    }
                }
            }
        }
    }
    Value::Num(w)
}

/// Convert a BigInt ratio n/d to f64 with correct rounding.
/// Uses bit-level scaling to avoid precision loss from independent to_f64() conversions.
pub fn bigrat_to_f64(n: &NumBigInt, d: &NumBigInt) -> f64 {
    if d.is_zero() {
        return if n.is_zero() {
            f64::NAN
        } else if n.is_negative() {
            f64::NEG_INFINITY
        } else {
            f64::INFINITY
        };
    }
    let sign = n.is_negative() != d.is_negative();
    let na = n.abs();
    let da = d.abs();
    if na.is_zero() {
        return if sign { -0.0 } else { 0.0 };
    }
    // Compute bit counts to estimate the magnitude of the result.
    // Then shift so the quotient has ~55 significant bits (enough for f64 + rounding).
    let n_bits = na.bits();
    let d_bits = da.bits();
    let target_bits: u64 = 55;
    // We want quotient = (na << shift) / da to have ~target_bits significant bits.
    // quotient_bits ~ n_bits + shift - d_bits, so shift = target_bits - n_bits + d_bits.
    let shift = if d_bits + target_bits > n_bits {
        (d_bits + target_bits - n_bits) as u32
    } else {
        0u32
    };
    let scaled = &na << shift;
    let quotient = &scaled / &da;
    let qf = quotient.to_f64().unwrap_or(f64::INFINITY);
    // Scale back: divide by 2^shift
    let result = qf * 2.0f64.powi(-(shift as i32));
    if sign { -result } else { result }
}

/// Distinguishes the five array/list container kinds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ArrayKind {
    /// `(1,2,3)` — flattens in slurpy context, `.raku` → `(1, 2, 3)`
    List,
    /// `[1,2,3]` — doesn't flatten, `.raku` → `[1, 2, 3]`
    Array,
    /// `$(1,2,3)` — Scalar-wrapped list, doesn't flatten, `.raku` → `$(1, 2, 3)`
    ItemList,
    /// `$[1,2,3]` — Scalar-wrapped array, doesn't flatten, `.raku` → `$[1, 2, 3]`
    ItemArray,
    /// `my @a[2;3]` — shaped (multidimensional) array declared with fixed dimensions
    Shaped,
    /// Lazy array — backed by an infinite range or similar lazy source.
    /// Operations like `pop`, `push`, `elems` should throw `X::Cannot::Lazy`.
    Lazy,
}

/// Backing storage for `Value::Hash`. Bundles the key/value map with its
/// container type metadata (element/key types) and object-hash original keys,
/// so the metadata travels WITH the hash through copy-on-write and rebuilds —
/// replacing the fragile `Arc::as_ptr`-keyed side tables (the root of the
/// intermittent typed-hash / object-hash flaky). `Deref`s to the inner map so
/// the overwhelming majority of read sites (`.get`/`.iter`/`.len`/`.values`/…)
/// are unchanged; only structural mutation/rebuild sites touch the wrapper.
#[derive(Debug, Clone, Default)]
pub struct HashData {
    pub map: HashMap<String, Value>,
    /// Element value-type constraint (e.g. `Int` for `my Int %h`), if any.
    pub value_type: Option<String>,
    /// Object-hash key-type constraint (e.g. `Mu` for `my %h{Mu}`), if any.
    /// `Some(..)` marks this as an object hash (`.WHICH`-keyed).
    pub key_type: Option<String>,
    /// Declared container type name (e.g. `Map`, `Hash[Int]`, `array[int]`) —
    /// mirrors `ContainerTypeInfo::declared_type`. Distinguishes an immutable
    /// `Map` from a plain `Hash`, etc.
    pub declared_type: Option<String>,
    /// For object hashes / typed-key hashes: maps each stored `.WHICH` key
    /// string back to the original key object (so `.keys`/subscript see the
    /// real key, not the WHICH string).
    pub original_keys: Option<HashMap<String, Value>>,
    /// `is default(...)` element default — the value a missing-key read yields.
    /// Embedded (replacing the former `Arc::as_ptr`-keyed `hash_defaults` side
    /// table) so it travels with the hash through copy-on-write — the pointer
    /// table broke whenever the backing `Arc` was rebuilt. Mirrors the array
    /// `ArrayData::default` field.
    pub default: Option<Box<Value>>,
}

/// Backing data for `Value::Array`: the element vector plus embedded
/// container type metadata (mirrors `HashData`). `Deref`/`DerefMut` to
/// `Vec<Value>` keep the overwhelming majority of read sites
/// (`.iter`/`.len`/`.get`/indexing/…) unchanged; only structural
/// mutation/rebuild sites touch the wrapper. Embedding the metadata in the
/// container (instead of an `Arc`-pointer-keyed side table) means it travels
/// through copy-on-write and can never be inherited by an unrelated array
/// via pointer reuse.
#[derive(Debug, Clone, Default)]
pub struct ArrayData {
    pub items: Vec<Value>,
    /// Element value-type constraint (e.g. `Int` for `my Int @a`), if any.
    pub value_type: Option<String>,
    /// Key-type constraint — unused for arrays, present so the shared
    /// embed/tag machinery treats all containers uniformly.
    pub key_type: Option<String>,
    /// Declared container type name (e.g. `Array[Int]`, `array[int]`), if any.
    pub declared_type: Option<String>,
    /// `is default(...)` element default. Embedded so it travels with the
    /// container through copy-on-write (the pointer-keyed side table broke
    /// whenever the backing Arc was rebuilt), and so pure value-level code
    /// (e.g. the Array→Slip coercion materializing holes) can read it.
    pub default: Option<Box<Value>>,
    /// Dimensions of a shaped (multidimensional) array (`my @a[2;3]`). `Some`
    /// only on `ArrayKind::Shaped` arrays. Embedded (replacing the former
    /// `Arc::as_ptr`-keyed `ShapedArrayIds` side table) so the shape travels
    /// with the container through copy-on-write.
    pub shape: Option<Vec<usize>>,
    /// Indices that were explicitly element-assigned (`@a[i] = …`), as opposed
    /// to autovivification gaps. `None` means the array was bulk/literal-
    /// constructed, so every in-range index exists (the historical
    /// "no side table → all exist" fallback). `Some(set)` means element-wise
    /// assignment has occurred: an in-range slot exists iff it is in the set OR
    /// its value is not a `Package("Any")` hole. Embedded (not an env-name-keyed
    /// side table) so it travels with the value across scopes, closures, and
    /// method calls — the former `__mutsu_initialized_index::name` table was
    /// scoped to the assigning frame and lost on scope exit, so an outer array
    /// filled from inside a method/closure (e.g. HTTP::Status's `method sink`
    /// populating `@codes`) reported every gap as existing.
    pub initialized: Option<std::collections::HashSet<usize>>,
}

/// Value stored in an enum variant: an integer, a string, or an arbitrary Value.
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum EnumValue {
    Int(i64),
    Str(String),
    /// Arbitrary value (e.g., Array for typed enums like `my Array enum ...`)
    Generic(Box<Value>),
}

/// The public `Value` type: one NaN-boxed 8-byte word behind the newtype
/// **seal** (ADR-0005, 3b-1 step B — the representation flip). The field and
/// the [`NanBox`] encoding are private to `crate::value`; [`ValueRepr`]
/// survives as the transient *working* enum crossed through
/// [`Value::into_repr`] / [`Value::from_repr`], and borrowed reads decode
/// through [`Value::view`]. Call sites outside `src/value/` see only the
/// 3b-0 wall API, which is unchanged by the flip.
#[derive(Clone)]
pub struct Value(nanbox::NanBox);

impl std::fmt::Debug for Value {
    /// Forward to the repr decode so debug output is byte-identical to the
    /// pre-flip derived output (trace logs and test expectations never see
    /// the packed word).
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// Raw NaN-box word access for the JIT Tier B inline emitter (see
/// [`jit_words`]). Read-only: exposing the bits does not breach the newtype
/// seal's ownership rules (the word still owns its payload reference).
#[cfg(feature = "jit")]
impl Value {
    pub(crate) fn nanbox_bits(&self) -> u64 {
        self.0.bits()
    }
}

impl Value {
    /// O(1) *binding* identity: the same immediate value, or the same heap
    /// allocation. Unlike `PartialEq` it never walks container contents, so it is
    /// usable on hot paths that only need to know whether a name was **rebound**
    /// (as opposed to its container being mutated in place, which leaves the
    /// binding — and these bits — unchanged).
    ///
    /// Available in every build, unlike the `jit`-gated `nanbox_bits`.
    #[inline]
    pub(crate) fn same_binding(&self, other: &Value) -> bool {
        self.0.bits() == other.0.bits()
    }
}

/// The `Value` representation (3b-1 step A). Private to `crate::value` — this
/// is the compile-time seal: only the wall API (`view()` / `as_*` /
/// constructors) crosses the module boundary. Tuple/unit variant construction
/// inside `src/value/` goes through the variant-named constructor shims on
/// `Value` below (which 3b-1 step B will turn into the NaN-box tag-packing
/// constructors); struct-like variants and all patterns use
/// `Value::from_repr(ValueRepr::...)` / `.0` directly.
#[allow(private_interfaces)]
#[derive(Debug, Clone)]
pub(in crate::value) enum ValueRepr {
    Int(i64),
    BigInt(Arc<NumBigInt>),
    Num(f64),
    Str(Arc<String>),
    Bool(bool),
    Range(i64, i64),
    RangeExcl(i64, i64),
    RangeExclStart(i64, i64),
    RangeExclBoth(i64, i64),
    GenericRange {
        start: Arc<Value>,
        end: Arc<Value>,
        excl_start: bool,
        excl_end: bool,
    },
    /// Distinguishes Array, List, and their itemized (Scalar-wrapped) variants.
    /// GC-migrated (§11 step 5c first wave): backed by a cycle-collectable
    /// `Gc<ArrayData>` rather than a plain `crate::gc::Gc<ArrayData>`.
    Array(crate::gc::Gc<ArrayData>, ArrayKind),
    /// The `bool` is the per-holder itemization flag (`true` when this hash sits
    /// in a `$` scalar container: `$(%h)` / `.item` / `my $h = %x`). It mirrors
    /// `ArrayKind::ItemArray` for arrays — a Value-level marker that shares the
    /// SAME `HashData` `Gc` as the non-itemized holder (so `=`-shared mutation
    /// still tracks), while `.raku` and list-context flattening read it to
    /// distinguish `${...}` / opaque-single-element from the bare `{...}` /
    /// spilled-pairs forms. Value equality/eqv IGNORE it (see `value_eq`).
    Hash(Gc<HashData>, bool),
    Rat(i64, i64),
    FatRat(i64, i64),
    BigRat(Box<NumBigInt>, Box<NumBigInt>),
    Complex(f64, f64),
    /// Set (immutable) or SetHash (mutable). The bool is `true` for mutable (SetHash).
    Set(crate::gc::Gc<SetData>, bool),
    /// Bag (immutable) or BagHash (mutable). The bool is `true` for mutable (BagHash).
    Bag(crate::gc::Gc<BagData>, bool),
    /// Mix (immutable) or MixHash (mutable). The bool is `true` for mutable (MixHash).
    Mix(crate::gc::Gc<MixData>, bool),
    CompUnitDepSpec {
        short_name: Symbol,
    },
    Package(Symbol),
    Routine {
        package: Symbol,
        name: Symbol,
        is_regex: bool,
    },
    Pair(String, Box<Value>),
    /// Pair with a non-string key (preserves the original key type for `.key`)
    ValuePair(Box<Value>, Box<Value>),
    Enum {
        enum_type: Symbol,
        key: Symbol,
        value: EnumValue,
        index: usize,
    },
    Regex(Arc<String>),
    /// A regex literal carrying adverbs. Boxed payload to keep `Value` small
    /// (this variant has 13 fields).
    RegexWithAdverbs(Box<RegexAdverbs>),
    Sub(crate::gc::Gc<SubData>),
    /// A weak reference to a Sub (used for &?BLOCK self-references to break cycles).
    /// Upgrade to the strong value when accessed; returns Nil if expired.
    WeakSub(crate::gc::WeakGc<SubData>),
    Instance {
        class_name: Symbol,
        attributes: crate::gc::Gc<InstanceAttrs>,
        id: u64,
    },
    Junction {
        kind: JunctionKind,
        values: Arc<Vec<Value>>,
    },
    Seq(Arc<Vec<Value>>),
    /// HyperSeq: result of `.hyper` — order-preserving parallel map/grep
    /// (worker threads, see `vm_hyper_race_parallel.rs`).
    HyperSeq(Arc<Vec<Value>>),
    /// RaceSeq: result of `.race` — unordered parallel map/grep (worker threads).
    RaceSeq(Arc<Vec<Value>>),
    Slip(Arc<Vec<Value>>),
    LazyList(crate::gc::Gc<LazyList>),
    Version {
        parts: Vec<VersionPart>,
        plus: bool,
        minus: bool,
    },
    Promise(SharedPromise),
    Channel(SharedChannel),
    Nil,
    Whatever,
    HyperWhatever,
    /// A value with mixin overrides from the `but` operator.
    /// Inner value is the original; the HashMap maps type names (e.g. "Bool") to override values.
    Mixin(Arc<Value>, Arc<HashMap<String, Value>>),
    /// A Capture: positional args + named args.
    /// Both fields are boxed to keep `Value` small (the inline `Vec` + `HashMap`
    /// otherwise made this the largest variant, inflating every `Value`).
    /// The double indirection is deliberate (`box_collection` fires now that
    /// the repr is private — the lint skips public API).
    Capture {
        #[allow(clippy::box_collection)]
        positional: Box<Vec<Value>>,
        #[allow(clippy::box_collection)]
        named: Box<HashMap<String, Value>>,
    },
    /// A *named variable reference*: an argument (or pair value, or `:=` RHS)
    /// tagged with the name of the variable it was read from, so that `is rw` /
    /// `is raw` binding, `:=` and `\($a)` can alias the caller's container
    /// rather than snapshot its value. Produced by `OpCode::WrapVarRef`, and
    /// stripped again by the binder (`unwrap_varref_value`) — so it is a
    /// *transient* wrapper that never reaches user-visible value space.
    VarRef {
        name: Symbol,
        value: Box<Value>,
        /// Source-element index, for a reference into a slurpy's element.
        index: Option<u32>,
    },
    /// Unicode normalization form types (NFC, NFD, NFKC, NFKD).
    /// `form` is one of "NFC", "NFD", "NFKC", "NFKD".
    /// `text` is the normalized string. Boxed to keep `Value` small.
    Uni(Box<UniData>),
    /// A Proxy container with FETCH and STORE callbacks.
    Proxy {
        fetcher: Box<Value>,
        storer: Box<Value>,
        /// Subclass name and shared mutable attributes (for Proxy subclasses)
        subclass: Option<(Symbol, ProxySubclassAttrs)>,
        /// When true, this Proxy has been decontainerized via .VAR and should not auto-FETCH
        decontainerized: bool,
    },
    /// A parametric role type, e.g. `R1[C1]` or `R1[C1, C2]`.
    /// `base_name` is the role name, `type_args` are the type arguments.
    ParametricRole {
        base_name: Symbol,
        type_args: Vec<Value>,
    },
    /// A type object created by Metamodel::Primitives.create_type.
    /// `how` is the meta-object (HOW), `repr` is the REPR name, `name` is the type
    /// name. Boxed payload to keep `Value` small.
    CustomType(Box<CustomTypeData>),
    /// An instance of a custom type (created by .CREATE on a CustomType).
    /// Boxed payload to keep `Value` small (this variant has six fields).
    CustomTypeInstance(Box<CustomTypeInstanceData>),
    /// A Scalar container wrapping a value (from `.item` or `$()`).
    /// Prevents one level of flattening in list/array context.
    Scalar(Box<Value>),
    /// A shared mutable Scalar container for `:=` binding.
    /// Two variables bound together share the same `ContainerRef`.
    ContainerRef(Gc<Mutex<Value>>),
    /// A lazy thunk: wraps a Sub that is evaluated on first access and cached.
    /// Used by `lazy { ... }` statement prefix.
    LazyThunk(Arc<LazyThunkData>),
    /// Lazy IO lines iterator. Reads lines from a file handle on demand.
    /// When `kv` is true, produces index-value pairs (for `.kv`).
    LazyIoLines {
        handle: Box<Value>,
        kv: bool,
        /// When true, the lazy iterator yields whitespace-delimited *words*
        /// (via `read_word_from_handle_value`) rather than lines. Used by
        /// `words($fh)` so a partial consumer leaves the handle open.
        words: bool,
    },
    /// A deferred write-through reference to a (possibly not-yet-existent) hash
    /// entry reached by `path` from `hash`. This is the sole survivor of the old
    /// slot-reference era — NOT a stale Arc+index back-reference, but a vivification
    /// token that cannot be a `ContainerRef` cell because its target entry does
    /// not exist yet (binding to a missing key must not pollute `:exists` /
    /// iteration until the first write — see `t/phantom-entry-bind.t`).
    ///
    /// - Reading returns the current value at the path (or `Any` if any level is
    ///   missing/non-hash), without creating anything.
    /// - Writing walk-creates intermediate hashes and inserts at the terminal key.
    /// - `path.len() == 1` is the single-key case (`$b := %h<x>`); a longer path
    ///   is a deferred nested bind (`$b := %h<a><b>`), accumulated one subscript
    ///   at a time by `exec_index_autovivify_lazy_op`.
    ///
    /// It also serves `is raw` reduce lvalue descent (`hash_autovivify`), where
    /// the entry is eagerly created first so the path is always length 1 —
    /// those tokens set `eager: true` and reads see through to the (already
    /// created) plain entry value. A deferred bind token (`eager: false`)
    /// connects on read ONLY via the `ContainerRef` cell installed by a write
    /// through the bound var: with in-place hash writes (container identity
    /// §3) an independent `%g<x><y> = 42` reaches the token's captured root,
    /// and rakudo does not retro-bind that (t/phantom-entry-bind.t).
    HashEntryRef {
        hash: Gc<HashData>,
        path: Vec<String>,
        eager: bool,
    },
}

impl Value {
    /// Whether two values hold the same representation variant. Wall-safe
    /// replacement for `std::mem::discriminant` on the pre-seal `enum Value`:
    /// compares the NaN-box words' variant tags (companion-field kinds — the
    /// six Array kinds, itemized Hash, mutable Set/Bag/Mix, the four Junction
    /// kinds, boxed Int — collapse onto their `ValueRepr` discriminant).
    #[inline]
    pub fn same_variant(&self, other: &Value) -> bool {
        self.0.variant_tag() == other.0.variant_tag()
    }
}

/// The 3b-1 step-B **internal wall** seam (see
/// `docs/nanbox-3b1-step-b-design.md` §4). `src/value/` code that today names
/// `ValueRepr::` in a *place* expression — a pattern on `.0`, a `&mut` into the
/// enum — migrates onto these three entry points (or `view()` for borrowed
/// reads), because after the representation flip there is no enum place to
/// borrow: the storage is a packed 8-byte word. While the enum IS the storage
/// these are identities, so each migration slice is byte-identical.
///
/// `ValueRepr` itself survives the flip as the transient *working* enum:
/// naming it in owned expressions (constructing one to pass to `from_repr`,
/// matching one returned by `into_repr`) stays valid forever.
impl Value {
    /// Decompose into the working representation enum (owned decode seam):
    /// the NaN-box tag decode. Pointer payloads move (no refcount traffic);
    /// shared multi-field boxes clone field-wise.
    #[inline]
    pub(in crate::value) fn into_repr(self) -> ValueRepr {
        self.0.into_repr()
    }

    /// Rebuild from the working representation enum (construction seam for
    /// struct-like variants; tuple/unit variants keep their named shims
    /// below): the NaN-box tag-packing encode.
    #[inline]
    pub(in crate::value) fn from_repr(repr: ValueRepr) -> Value {
        Value(nanbox::NanBox::from_repr(repr))
    }

    /// Edit the representation in place (mutation seam): decode to the
    /// working enum, run `f`, re-pack — so `f` must not assume its edits
    /// alias other holders (no site does; aliased container mutation goes
    /// through the pointee, not the repr). If `f` unwinds, `self` is left
    /// `Nil` — acceptable, the value is torn mid-edit either way.
    #[inline]
    pub(in crate::value) fn with_repr_mut<R>(&mut self, f: impl FnOnce(&mut ValueRepr) -> R) -> R {
        let mut repr = std::mem::replace(&mut self.0, nanbox::NanBox::NIL).into_repr();
        let out = f(&mut repr);
        self.0 = nanbox::NanBox::from_repr(repr);
        out
    }
}

/// Variant-named constructor shims for the tuple/unit variants of
/// [`ValueRepr`], so the `Value::Int(..)` / `Value::Nil` *expression* sites
/// inside `src/value/` compile unchanged across the newtype seal (patterns
/// cannot resolve to functions/consts, so every pattern site was rewritten to
/// `Value::from_repr(ValueRepr::..)`). Private to `crate::value` like the repr itself —
/// outside the module these do not exist, which is the seal. In 3b-1 step B
/// these bodies become the NaN-box tag-packing constructors, so construction
/// is already funneled through the single place the flip will edit.
/// Struct-like variants (`Instance { .. }`, `Capture { .. }`, ...) cannot be
/// shimmed as functions; their sites construct `Value::from_repr(ValueRepr::.. { .. })`.
#[allow(non_snake_case, non_upper_case_globals, dead_code)]
impl Value {
    pub(in crate::value) const Nil: Value = Value(nanbox::NanBox::NIL);
    pub(in crate::value) const Whatever: Value = Value(nanbox::NanBox::WHATEVER);
    pub(in crate::value) const HyperWhatever: Value = Value(nanbox::NanBox::HYPER_WHATEVER);

    #[inline]
    pub(in crate::value) fn Int(v: i64) -> Value {
        Value::from_repr(ValueRepr::Int(v))
    }
    #[inline]
    pub(in crate::value) fn BigInt(v: Arc<NumBigInt>) -> Value {
        Value::from_repr(ValueRepr::BigInt(v))
    }
    #[inline]
    pub(in crate::value) fn Num(v: f64) -> Value {
        Value::from_repr(ValueRepr::Num(v))
    }
    #[inline]
    pub(in crate::value) fn Str(v: Arc<String>) -> Value {
        Value::from_repr(ValueRepr::Str(v))
    }
    #[inline]
    pub(in crate::value) fn Bool(v: bool) -> Value {
        Value::from_repr(ValueRepr::Bool(v))
    }
    #[inline]
    pub(in crate::value) fn Range(a: i64, b: i64) -> Value {
        Value::from_repr(ValueRepr::Range(a, b))
    }
    #[inline]
    pub(in crate::value) fn RangeExcl(a: i64, b: i64) -> Value {
        Value::from_repr(ValueRepr::RangeExcl(a, b))
    }
    #[inline]
    pub(in crate::value) fn RangeExclStart(a: i64, b: i64) -> Value {
        Value::from_repr(ValueRepr::RangeExclStart(a, b))
    }
    #[inline]
    pub(in crate::value) fn RangeExclBoth(a: i64, b: i64) -> Value {
        Value::from_repr(ValueRepr::RangeExclBoth(a, b))
    }
    #[inline]
    pub(in crate::value) fn Array(data: crate::gc::Gc<ArrayData>, kind: ArrayKind) -> Value {
        Value::from_repr(ValueRepr::Array(data, kind))
    }
    #[inline]
    pub(in crate::value) fn Hash(data: Gc<HashData>, itemized: bool) -> Value {
        Value::from_repr(ValueRepr::Hash(data, itemized))
    }
    #[inline]
    pub(in crate::value) fn Rat(n: i64, d: i64) -> Value {
        Value::from_repr(ValueRepr::Rat(n, d))
    }
    #[inline]
    pub(in crate::value) fn FatRat(n: i64, d: i64) -> Value {
        Value::from_repr(ValueRepr::FatRat(n, d))
    }
    #[inline]
    pub(in crate::value) fn BigRat(n: Box<NumBigInt>, d: Box<NumBigInt>) -> Value {
        Value::from_repr(ValueRepr::BigRat(n, d))
    }
    #[inline]
    pub(in crate::value) fn Complex(re: f64, im: f64) -> Value {
        Value::from_repr(ValueRepr::Complex(re, im))
    }
    #[inline]
    pub(in crate::value) fn Set(data: crate::gc::Gc<SetData>, mutable: bool) -> Value {
        Value::from_repr(ValueRepr::Set(data, mutable))
    }
    #[inline]
    pub(in crate::value) fn Bag(data: crate::gc::Gc<BagData>, mutable: bool) -> Value {
        Value::from_repr(ValueRepr::Bag(data, mutable))
    }
    #[inline]
    pub(in crate::value) fn Mix(data: crate::gc::Gc<MixData>, mutable: bool) -> Value {
        Value::from_repr(ValueRepr::Mix(data, mutable))
    }
    #[inline]
    pub(in crate::value) fn Package(sym: Symbol) -> Value {
        Value::from_repr(ValueRepr::Package(sym))
    }
    #[inline]
    pub(in crate::value) fn Pair(key: String, val: Box<Value>) -> Value {
        Value::from_repr(ValueRepr::Pair(key, val))
    }
    #[inline]
    pub(in crate::value) fn ValuePair(key: Box<Value>, val: Box<Value>) -> Value {
        Value::from_repr(ValueRepr::ValuePair(key, val))
    }
    #[inline]
    pub(in crate::value) fn Regex(pat: Arc<String>) -> Value {
        Value::from_repr(ValueRepr::Regex(pat))
    }
    #[inline]
    pub(in crate::value) fn RegexWithAdverbs(adv: Box<RegexAdverbs>) -> Value {
        Value::from_repr(ValueRepr::RegexWithAdverbs(adv))
    }
    #[inline]
    pub(in crate::value) fn Sub(data: crate::gc::Gc<SubData>) -> Value {
        Value::from_repr(ValueRepr::Sub(data))
    }
    #[inline]
    pub(in crate::value) fn WeakSub(data: crate::gc::WeakGc<SubData>) -> Value {
        Value::from_repr(ValueRepr::WeakSub(data))
    }
    #[inline]
    pub(in crate::value) fn Seq(items: Arc<Vec<Value>>) -> Value {
        Value::from_repr(ValueRepr::Seq(items))
    }
    #[inline]
    pub(in crate::value) fn HyperSeq(items: Arc<Vec<Value>>) -> Value {
        Value::from_repr(ValueRepr::HyperSeq(items))
    }
    #[inline]
    pub(in crate::value) fn RaceSeq(items: Arc<Vec<Value>>) -> Value {
        Value::from_repr(ValueRepr::RaceSeq(items))
    }
    #[inline]
    pub(in crate::value) fn Slip(items: Arc<Vec<Value>>) -> Value {
        Value::from_repr(ValueRepr::Slip(items))
    }
    #[inline]
    pub(in crate::value) fn LazyList(data: crate::gc::Gc<LazyList>) -> Value {
        Value::from_repr(ValueRepr::LazyList(data))
    }
    #[inline]
    pub(in crate::value) fn Promise(p: SharedPromise) -> Value {
        Value::from_repr(ValueRepr::Promise(p))
    }
    #[inline]
    pub(in crate::value) fn Channel(c: SharedChannel) -> Value {
        Value::from_repr(ValueRepr::Channel(c))
    }
    #[inline]
    pub(in crate::value) fn Mixin(
        inner: Arc<Value>,
        overrides: Arc<HashMap<String, Value>>,
    ) -> Value {
        Value::from_repr(ValueRepr::Mixin(inner, overrides))
    }
    #[inline]
    pub(in crate::value) fn Uni(data: Box<UniData>) -> Value {
        Value::from_repr(ValueRepr::Uni(data))
    }
    #[inline]
    pub(in crate::value) fn CustomType(data: Box<CustomTypeData>) -> Value {
        Value::from_repr(ValueRepr::CustomType(data))
    }
    #[inline]
    pub(in crate::value) fn CustomTypeInstance(data: Box<CustomTypeInstanceData>) -> Value {
        Value::from_repr(ValueRepr::CustomTypeInstance(data))
    }
    #[inline]
    pub(in crate::value) fn Scalar(inner: Box<Value>) -> Value {
        Value::from_repr(ValueRepr::Scalar(inner))
    }
    #[inline]
    pub(in crate::value) fn ContainerRef(cell: Gc<Mutex<Value>>) -> Value {
        Value::from_repr(ValueRepr::ContainerRef(cell))
    }
    #[inline]
    pub(in crate::value) fn LazyThunk(data: Arc<LazyThunkData>) -> Value {
        Value::from_repr(ValueRepr::LazyThunk(data))
    }
}

/// Boxed payload of [`Value::CustomTypeInstance`] (an instance of a type created
/// via Metamodel::Primitives.create_type + `.CREATE`). Boxed to keep `Value` small.
#[derive(Debug, Clone)]
pub struct CustomTypeInstanceData {
    pub type_id: u64,
    pub how: Box<Value>,
    pub repr: String,
    pub type_name: Symbol,
    pub attributes: Arc<HashMap<String, Value>>,
    pub id: u64,
}

/// Boxed payload of [`Value::CustomType`] (a type object from create_type).
#[derive(Debug, Clone)]
pub struct CustomTypeData {
    pub how: Box<Value>,
    pub repr: String,
    pub name: Symbol,
    pub id: u64,
}

/// Boxed payload of [`Value::Uni`] (a Uni in a normalization form).
#[derive(Debug, Clone)]
pub struct UniData {
    pub form: String,
    pub text: String,
}

/// Boxed payload of [`Value::RegexWithAdverbs`] (a regex literal carrying adverbs).
#[derive(Debug, Clone)]
pub struct RegexAdverbs {
    pub pattern: Arc<String>,
    pub global: bool,
    pub exhaustive: bool,
    pub overlap: bool,
    pub repeat: Option<usize>,
    pub nth: Option<Arc<String>>,
    pub perl5: bool,
    pub pos: bool,
    pub continue_: bool,
    pub ignore_case: bool,
    pub sigspace: bool,
    pub samecase: bool,
    pub samespace: bool,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub(crate) enum VersionPart {
    Num(i64),
    Str(String),
    Whatever,
}

/// Specification for a lazy infinite sequence (e.g. `1...*`, `2, 4 ... *`).
/// Stored in `LazyList` to allow on-demand element generation beyond the
/// initially cached seed elements.
#[derive(Debug, Clone)]
pub(crate) enum SequenceSpec {
    /// Arithmetic progression: next = last + step
    Arithmetic {
        step: i64,
        /// Whether the seed values are all Int (enables Int-typed output)
        all_int: bool,
    },
    /// Geometric progression: next = last * num / den
    GeometricRat { num: i64, den: i64 },
    /// Geometric progression with floating-point ratio
    Geometric { ratio: f64 },
    /// Infinite `.roll(*)`: each additional element is an independent
    /// uniform-random pick from `pool` (unlike Arithmetic/Geometric, this
    /// never terminates and has no fixed "next" formula beyond the pool).
    RollPool(Vec<Value>),
}

/// Specification for a lazy scan (triangle) reduction.
/// Stored in `LazyList` to allow on-demand element computation.
#[derive(Debug, Clone)]
pub(crate) struct ScanSpec {
    /// The base operator name (e.g. "+", "*", "~").
    pub(crate) op: String,
    /// Whether to negate the result of each step.
    pub(crate) negate: bool,
    /// The source value to iterate over (typically a Range variant).
    pub(crate) source: Value,
    /// The accumulator state after the last computed element.
    pub(crate) accumulator: Option<Value>,
    /// The number of elements already computed (matches cache length).
    pub(crate) computed_count: usize,
}

/// A single lazy `map`/`grep` stage applied to a source, pulled element by
/// element. Chains nest: the `source` of one [`MapGrepSpec`] can itself be a
/// `LazyList` carrying another [`MapGrepSpec`], so `(1..Inf).map(...).grep(...)`
/// becomes a grep stage whose source is the map stage. See
/// [`crate::value::LazyList::lazy_pipe`].
#[derive(Debug, Clone)]
pub(crate) struct MapGrepSpec {
    /// The source to pull elements from (infinite `Range`, or another lazy
    /// `LazyList`). Pulled one element at a time via the VM.
    pub(crate) source: Value,
    /// The `map`/`grep` callback (a `Sub`, `WhateverCode`, regex, …).
    pub(crate) func: Value,
    /// `true` for `grep` (filter), `false` for `map` (transform).
    pub(crate) is_grep: bool,
    /// Index of the next element to pull from `source`.
    pub(crate) source_idx: usize,
    /// `true` once `source` reported exhaustion (finite source ran out).
    pub(crate) done: bool,
    /// When set, this stage ignores `func`/`is_grep` and emits an index-based
    /// transform of each source element (`.pairs`/`.antipairs`/`.kv`), using
    /// `source_idx` as the positional key. Lets these methods stay lazy over a
    /// lazy source instead of materializing it.
    pub(crate) index_transform: Option<IndexTransform>,
}

/// Index-based transform applied lazily to a positional source, used by
/// `.pairs`/`.antipairs`/`.kv` over a lazy list so they don't force it.
#[derive(Debug, Clone, Copy)]
pub(crate) enum IndexTransform {
    /// `.pairs`: element `i` → `Pair(i, elem)`.
    Pairs,
    /// `.antipairs`: element `i` → `Pair(elem, i)`.
    AntiPairs,
    /// `.kv`: element `i` → two flat outputs `i, elem`.
    Kv,
}

/// What a [`CatPullSpec`] lazy list pulls from its backing `IO::CatHandle`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CatPullMode {
    /// `.lines`: each pull reads the next line across all handles (`.get`).
    Lines,
    /// `.handles`: the first pull yields the currently-active handle, then each
    /// subsequent pull advances to the next handle (`.next-handle`).
    Handles,
}

/// Lazy `IO::CatHandle.lines` / `.handles` generator. Holds the live cat
/// instance (sharing its attribute cell with the user's `$cat`), so each pull
/// reads/advances the *same* cat — letting mid-iteration changes to
/// `.chomp`/`.nl-in`/`.encoding` take effect and `.path`/`on-switch` track the
/// current handle, exactly as Rakudo's lazy iterators do.
#[derive(Debug, Clone)]
pub(crate) struct CatPullSpec {
    /// The backing `IO::CatHandle` instance (shares its `AttrCell`).
    pub(crate) cat: Value,
    /// Whether to pull lines or handles.
    pub(crate) mode: CatPullMode,
    /// `Handles` only: whether the current-active handle was already yielded.
    pub(crate) started: bool,
    /// Set once the cat is exhausted so further pulls stop.
    pub(crate) done: bool,
}

/// Saved for-loop state for resuming a gather coroutine mid-iteration.
#[derive(Debug, Clone)]
pub(crate) enum ForLoopResumeState {
    /// Resume an integer-range for loop at the given current value.
    IntRange {
        current: i64,
        end_val: i64,
        inclusive: bool,
    },
    /// Resume a list-based for loop at the given index.
    List {
        items: Vec<Value>,
        next_index: usize,
    },
    /// Resume a lazy-gather for loop at the given element index.
    LazyGather {
        lazy_list: crate::gc::Gc<LazyList>,
        next_index: usize,
    },
    /// Resume a C-style / `loop` / `while` loop. These loops carry no iteration
    /// index — their state lives entirely in locals/env — so the marker only
    /// signals "we suspended inside this loop; re-enter it" and keeps the VM ip
    /// parked on the loop opcode across a gather coroutine suspend.
    CStyleLoop,
}

/// Pre-compiled fast-path body for a closure sequence generator: the compiled
/// code plus its associated compiled functions.
pub(crate) type CompiledClosureBody = (Arc<CompiledCode>, Arc<HashMap<String, CompiledFunction>>);

/// State for an infinite closure-based sequence (`1, 1, * + * ... *`).
///
/// Unlike `SequenceSpec` (arithmetic/geometric, computed without VM context),
/// a closure generator must re-invoke user code to produce each next element,
/// so its state is kept here and extended on demand via the VM.
#[derive(Clone)]
pub(crate) struct ClosureSeqState {
    /// The generator closure (`Value::Sub` or `Value::Routine`).
    pub(crate) generator: Value,
    /// Mutable captured env for side-effecting generators (e.g. `my $i = 0; { $i++ } ... *`).
    /// Persisted across pulls so side effects accumulate.
    pub(crate) closure_env: Option<Env>,
    /// Pre-compiled fast-path body for the generator (when the fast path applies).
    pub(crate) precompiled: Option<CompiledClosureBody>,
    /// Set once the generator signals termination (`last`/error) so we stop pulling.
    pub(crate) finished: bool,
}

/// Saved VM state for a suspended gather coroutine.
/// When `take` is encountered during gather body execution, the VM state
/// is captured here so execution can resume later for more elements.
#[derive(Debug, Clone)]
pub(crate) struct GatherCoroutineState {
    pub(crate) ip: usize,
    pub(crate) locals: Vec<Value>,
    pub(crate) stack: Vec<Value>,
    pub(crate) env: Env,
    pub(crate) finished: bool,
    /// True once the body has run and suspended at least once. The resume
    /// check must use this, NOT `ip > 0`: a body that is a single compound
    /// loop op (`gather { loop { take ... } }`) suspends with `ip == 0` and
    /// its position in `for_loop_resume`, and re-running it from scratch
    /// instead of resuming double-applies the body's side effects on
    /// cell-promoted captured lexicals.
    pub(crate) started: bool,
    /// Unique state-variable scope for this gather instance (from
    /// `next_instance_id()` at `MakeGather` time). Each evaluation of a
    /// `gather` expression is a fresh block clone, so its `state` variables
    /// must not collide with a sibling gather's — the bodies are compiled
    /// separately (ip restarts at 0), so the compile-time `@ip` key suffix
    /// alone cannot distinguish them. Installed as `state_scope_id` while
    /// the body runs. 0 = no scope (pre-existing coroutine states).
    pub(crate) state_scope_id: u64,
    /// Saved for-loop iteration state when suspended inside a for loop.
    pub(crate) for_loop_resume: Option<ForLoopResumeState>,
}

/// Lazy `WALK(method)` invocation state. `$obj.WALK("foo")()` walks the MRO
/// calling each level's own `foo` ONE AT A TIME as the result list is pulled
/// (Rakudo: the calls are lazy — `for WALK("foo")() { ... }` invokes the Nth
/// candidate on the Nth iteration). `targets` holds the resolved per-level
/// candidates (`"C|Owner"` / `"R|Owner"` tags); `idx` is the next to invoke.
#[derive(Debug, Clone)]
pub(crate) struct WalkPendingState {
    pub(crate) receiver_class: String,
    pub(crate) method_name: String,
    pub(crate) targets: Vec<String>,
    pub(crate) args: Vec<Value>,
    pub(crate) invocant: Value,
    pub(crate) attributes: AttrMap,
    pub(crate) quiet: bool,
    pub(crate) idx: usize,
}

pub(crate) struct LazyList {
    pub(crate) body: Vec<Stmt>,
    pub(crate) env: Env,
    pub(crate) cache: Mutex<Option<Vec<Value>>>,
    /// Pre-compiled bytecode for the gather body (used by VM-native forcing).
    pub(crate) compiled_code: Option<Arc<CompiledCode>>,
    /// Compiled functions associated with the compiled code.
    pub(crate) compiled_fns: Option<Arc<HashMap<String, CompiledFunction>>>,
    /// Known element count for combinatorial lazy sequences (e.g. n! for permutations).
    /// When set, numeric coercion uses this value instead of materializing all elements.
    pub(crate) elems_count: Option<Value>,
    /// Scan (triangle reduce) specification for lazy on-demand computation.
    pub(crate) scan_spec: Option<Mutex<ScanSpec>>,
    /// Sequence specification for infinite arithmetic/geometric sequences.
    /// When present, more elements can be generated on demand beyond the cache.
    pub(crate) sequence_spec: Option<SequenceSpec>,
    /// Suspended coroutine state for lazy gather/take.
    /// When present, the gather body can be resumed from where `take` paused it.
    pub(crate) coroutine: Option<Mutex<GatherCoroutineState>>,
    /// Lazy `map`/`grep` pipeline stage. When present, elements are produced by
    /// pulling from `lazy_pipe.source` and applying the stage's callback on
    /// demand, so `(1..Inf).map(...)`/`.grep(...)` stay truly lazy instead of
    /// materializing the (possibly infinite) source.
    pub(crate) lazy_pipe: Option<Mutex<MapGrepSpec>>,
    /// Infinite closure-based sequence generator (`1, 1, * + * ... *`).
    /// When present, more elements are produced on demand by re-invoking the
    /// generator closure over the growing element history.
    pub(crate) closure_seq: Option<Mutex<ClosureSeqState>>,
    /// Lazy `WALK(method)()` candidate-invocation state (see `WalkPendingState`):
    /// each pull invokes the next MRO-level candidate.
    pub(crate) walk_pending: Option<Mutex<WalkPendingState>>,
    /// Lazy `IO::CatHandle.lines` / `.handles` generator (see `CatPullSpec`):
    /// each pull reads the next line / handle from the live cat instance.
    pub(crate) cat_pull: Option<Mutex<CatPullSpec>>,
}

/// Placeholder string rendered for a genuinely-lazy list under
/// gist/Str/raku/perl, matching Rakudo: `...` for `.Str`, `[...]` for a list
/// held in `@` array context, `(...)` for a bare Seq.
pub(crate) fn lazy_list_placeholder(method: &str, array_context: bool) -> String {
    if method == "Str" {
        "...".to_string()
    } else if array_context {
        "[...]".to_string()
    } else {
        "(...)".to_string()
    }
}

// --- LazyThunkData: deferred evaluation with caching ---

#[derive(Debug)]
pub(crate) struct LazyThunkData {
    /// The block/sub to evaluate when the thunk is forced.
    pub(crate) thunk: Value,
    /// Cached result: None means not yet evaluated.
    pub(crate) cache: Mutex<Option<Value>>,
}

// --- SharedPromise: thread-safe promise state ---

/// A callback registered via `SharedPromise::on_resolve` while the promise
/// was still `Planned`. Invoked with (status, result, output, stderr) once
/// the promise resolves. Boxed so `.then`/`.andthen`/`.orelse` chains can
/// queue heterogeneous closures.
pub(crate) type PromiseWaiter = Box<dyn FnOnce(String, Value, String, String) + Send>;

struct PromiseState {
    status: String, // "Planned", "Kept", "Broken"
    result: Value,
    output: String, // captured stdout from thread
    stderr_output: String,
    class_name: Symbol, // "Promise" or subclass name
    /// Opaque payload transferred from the worker thread to the awaiting
    /// thread. Used e.g. to hand off newly-opened IO handle state that
    /// lives inside the per-thread Interpreter.
    thread_payload: Option<Box<dyn std::any::Any + Send>>,
    /// Callbacks queued by `.then`/`.andthen`/`.orelse` while `Planned`,
    /// drained and run **in registration order** by whichever `keep`/
    /// `break_with` call resolves this promise. See `on_resolve` for why:
    /// without a shared, ordered queue, each combinator call on an
    /// unresolved promise used to spawn its own OS thread that raced the
    /// others to wake from the same `Condvar::notify_all`, so sibling
    /// callbacks (e.g. an independent `.then` alongside an `.andthen`
    /// chain, both registered on the same promise) had no ordering
    /// guarantee and could run in either order depending on OS scheduling
    /// — observable as an intermittent CI-only failure under load
    /// (S17-promise/then.t "simple keep"/"simple break").
    waiters: Vec<PromiseWaiter>,
}

#[derive(Debug, Clone)]
pub(crate) struct SharedPromise {
    inner: crate::gc::Gc<(Mutex<PromiseState>, Condvar)>,
}

// --- SharedChannel: thread-safe channel ---

#[derive(Debug)]
struct ChannelState {
    queue: std::collections::VecDeque<Value>,
    send_closed: bool,
    drained_closed: bool,
    failure: Option<Value>,
    closed_promise: SharedPromise,
    supplier_ids: Vec<u64>,
}

#[derive(Debug, Clone)]
pub(crate) struct SharedChannel {
    inner: crate::gc::Gc<(Mutex<ChannelState>, Condvar)>,
}

// Compile-time assertions that Value is Send + Sync
const _: fn() = || {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<Value>();
};

#[cfg(test)]
mod value_size_guard {
    use super::*;
    #[test]
    fn value_stays_small() {
        // Since the 3b-1 step-B representation flip, `Value` is one NaN-boxed
        // 8-byte word (`NanBox`); `Option<Value>` keeps the same size via the
        // NonZeroU64 niche. Guard against layout regressions.
        let sz = std::mem::size_of::<Value>();
        assert!(sz <= 8, "size_of::<Value>() = {sz}, expected <= 8");
        assert_eq!(std::mem::size_of::<Option<Value>>(), sz);
    }
}

#[cfg(test)]
mod hash_chokepoint_tests {
    use super::*;

    #[test]
    fn insert_through_replaces_bare_entry() {
        let mut map = HashMap::new();
        map.insert("a".to_string(), Value::Int(1));
        Value::hash_insert_through(&mut map, "a".to_string(), Value::Int(2));
        assert_eq!(map.get("a").and_then(Value::as_int), Some(2));
    }

    #[test]
    fn insert_through_creates_missing_entry() {
        let mut map = HashMap::new();
        Value::hash_insert_through(&mut map, "b".to_string(), Value::Int(7));
        assert_eq!(map.get("b").and_then(Value::as_int), Some(7));
    }

    #[test]
    fn insert_through_writes_through_container_ref_cell() {
        // A `:=`-bound element holds a shared `ContainerRef` cell; a later
        // assignment to the key must write *through* the cell (preserving the
        // binding), not replace the entry with a bare value.
        let cell = crate::gc::Gc::new(Mutex::new(Value::Int(1)));
        let mut map = HashMap::new();
        map.insert("k".to_string(), Value::ContainerRef(cell.clone()));
        Value::hash_insert_through(&mut map, "k".to_string(), Value::Int(99));
        // The entry is still the same cell (binding preserved)...
        assert!(matches!(
            map.get("k").map(Value::view),
            Some(ValueView::ContainerRef(_))
        ));
        // ...and the alias observes the new value through the cell.
        assert_eq!(cell.lock().unwrap().as_int(), Some(99));
    }
}
