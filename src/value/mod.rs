use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock, RwLock, Weak};

use crate::ast::{ParamDef, Stmt};
use crate::env::Env;
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
    list.push(Arc::downgrade(arc_ptr));
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
static CONSUMED_LAZYLISTS: OnceLock<Mutex<Vec<Weak<LazyList>>>> = OnceLock::new();

fn consumed_lazylists() -> &'static Mutex<Vec<Weak<LazyList>>> {
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
pub(crate) fn lazylist_consume(arc_ptr: &Arc<LazyList>) -> bool {
    let mut list = consumed_lazylists().lock().unwrap();
    let target_ptr = Arc::as_ptr(arc_ptr);
    list.retain(|w| w.strong_count() > 0);
    for w in list.iter() {
        if let Some(existing) = w.upgrade()
            && Arc::as_ptr(&existing) == target_ptr
        {
            return false; // already consumed
        }
    }
    list.push(Arc::downgrade(arc_ptr));
    true
}

/// Check if a LazyList has been consumed.
pub(crate) fn lazylist_is_consumed(arc_ptr: &Arc<LazyList>) -> bool {
    let list = consumed_lazylists().lock().unwrap();
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
    list.push(Arc::downgrade(arc_ptr));
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
#[allow(clippy::result_large_err)]
pub(crate) fn seq_consumed_error() -> RuntimeError {
    let msg = "The iterator of this Seq is already in use/consumed by another Seq \
               (you might solve this by adding .cache on usages of the Seq, or by \
               assigning the Seq into an array)";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    let ex = Value::make_instance(crate::symbol::Symbol::intern("X::Seq::Consumed"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Mark a Seq (identified by its Arc) as consumed.
/// Returns Err if the Seq was already consumed.
#[allow(clippy::result_large_err)]
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
    list.push(Arc::downgrade(arc_ptr));
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
mod display;
mod error;
mod error_construct;
mod error_typed;
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
mod value_instance;
mod value_lazy;
mod value_methods_a;
mod value_methods_b;
mod value_methods_c;
mod value_setbagmix;
pub(crate) use aliased_mut::arc_contents_mut;
pub(crate) use types::what_type_name;

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
pub use display::{format_complex, tclc_str, wordcase_str};
pub(crate) use error::expected_type_object;
pub use error::{Control, RuntimeError, RuntimeErrorCode};
// SubData is re-exported so callers can destructure Value::Sub(data)

static INSTANCE_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone)]
pub(crate) struct PendingInstanceDestroy {
    pub(crate) class_name: Symbol,
    pub(crate) attributes: HashMap<String, Value>,
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

fn live_instance_refcounts() -> &'static Mutex<HashMap<u64, usize>> {
    static LIVE_INSTANCE_REFCOUNTS: OnceLock<Mutex<HashMap<u64, usize>>> = OnceLock::new();
    LIVE_INSTANCE_REFCOUNTS.get_or_init(|| Mutex::new(HashMap::new()))
}

/// The shared mutable attribute cell of an instance (Phase 3, Stage 1).
pub(crate) type AttrCell = Arc<RwLock<HashMap<String, Value>>>;

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
type PendingCellWrite = (usize, AttrCell, HashMap<String, Value>);

fn cell_addr(cell: &RwLock<HashMap<String, Value>>) -> usize {
    cell as *const RwLock<HashMap<String, Value>> as usize
}

/// A read guard over an instance's attribute map. Derefs to `&HashMap`, so the
/// vast majority of read sites (`.get`, `.iter`, `.len`, `for (k, v) in ...`)
/// and `&guard` argument coercions to `&HashMap` keep working unchanged. On
/// construction it records the cell address in [`HELD_READ_CELLS`]; on drop it
/// removes it and, when this was the last read guard on the cell, applies any
/// write that was deferred while the cell was read-locked.
pub(crate) struct AttrReadGuard<'a> {
    // `Option` so `Drop` can release the read lock *before* flushing deferred
    // writes — otherwise the flush's blocking write lock would deadlock against
    // this guard's own still-held read lock (struct fields drop after `drop`).
    guard: Option<std::sync::RwLockReadGuard<'a, HashMap<String, Value>>>,
    addr: usize,
}

#[derive(Debug)]
pub(crate) struct InstanceAttrs {
    class_name: Symbol,
    /// Shared mutable attribute cell.
    ///
    /// Cross-frame *sharing* (the Raku object-identity semantics that makes an
    /// in-place mutation visible to every alias) comes from cloning the
    /// `Arc<InstanceAttrs>` held by `Value::Instance` — that aliases this same
    /// struct and cell. An explicit `InstanceAttrs::clone` (see the manual
    /// `Clone` impl below) instead makes an *independent* deep copy with a fresh
    /// cell, preserving the long-standing semantics of the legacy writeback
    /// sites that do `(*attributes).clone()` to build a detached "updated" map.
    attributes: AttrCell,
    id: u64,
    queue_destroy: bool,
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
pub fn register_container_constraint(cell: &Arc<Mutex<Value>>, type_name: &str) {
    let ptr = Arc::as_ptr(cell) as usize;
    typed_container_constraints()
        .lock()
        .unwrap()
        .insert(ptr, type_name.to_string());
}

/// Look up the `of`-type constraint of a `ContainerRef` cell, if any.
pub fn lookup_container_constraint(cell: &Arc<Mutex<Value>>) -> Option<String> {
    let ptr = Arc::as_ptr(cell) as usize;
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
fn write_cell_respecting_reads(cell: &AttrCell, map: HashMap<String, Value>) {
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
fn read_attrs(cell: &RwLock<HashMap<String, Value>>) -> AttrReadGuard<'_> {
    let guard = cell.read().unwrap_or_else(|e| e.into_inner());
    AttrReadGuard::new(guard, cell_addr(cell))
}

fn write_attrs(
    cell: &RwLock<HashMap<String, Value>>,
) -> std::sync::RwLockWriteGuard<'_, HashMap<String, Value>> {
    cell.write().unwrap_or_else(|e| e.into_inner())
}

pub(crate) fn next_instance_id() -> u64 {
    INSTANCE_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(crate) fn take_pending_instance_destroys() -> Vec<PendingInstanceDestroy> {
    PENDING_INSTANCE_DESTROYS.with(|pending| std::mem::take(&mut *pending.borrow_mut()))
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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
    /// Itemization flag: `true` when this hash came from a `$` scalar container
    /// (`$(%h)` / `$hashitem` placed in a list / `.item`). Mirrors
    /// `ArrayKind::ItemList`/`ItemArray` for arrays — value operations IGNORE it
    /// (the value is still a plain `Value::Hash`, so it never leaks), but
    /// list-context hash flattening (`build_hash_from_items`) treats an itemized
    /// hash as a single opaque element instead of spilling its pairs. This is
    /// what distinguishes `%m = (%h,)` (flattens) from `%m = ($hashitem,)`
    /// (stays opaque → "Odd number"). Excluded from `PartialEq` (see below).
    pub itemized: bool,
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

#[allow(private_interfaces)]
#[derive(Debug, Clone)]
pub enum Value {
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
    Array(Arc<ArrayData>, ArrayKind),
    Hash(Arc<HashData>),
    Rat(i64, i64),
    FatRat(i64, i64),
    BigRat(Box<NumBigInt>, Box<NumBigInt>),
    Complex(f64, f64),
    /// Set (immutable) or SetHash (mutable). The bool is `true` for mutable (SetHash).
    Set(Arc<SetData>, bool),
    /// Bag (immutable) or BagHash (mutable). The bool is `true` for mutable (BagHash).
    Bag(Arc<BagData>, bool),
    /// Mix (immutable) or MixHash (mutable). The bool is `true` for mutable (MixHash).
    Mix(Arc<MixData>, bool),
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
    Sub(Arc<SubData>),
    /// A weak reference to a Sub (used for &?BLOCK self-references to break cycles).
    /// Upgrade to the strong value when accessed; returns Nil if expired.
    WeakSub(Weak<SubData>),
    Instance {
        class_name: Symbol,
        attributes: Arc<InstanceAttrs>,
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
    LazyList(Arc<LazyList>),
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
    Capture {
        positional: Box<Vec<Value>>,
        named: Box<HashMap<String, Value>>,
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
    ContainerRef(Arc<Mutex<Value>>),
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
    /// the entry is eagerly created first so the path is always length 1.
    HashEntryRef {
        hash: Arc<HashData>,
        path: Vec<String>,
    },
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
        lazy_list: Arc<LazyList>,
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
    pub(crate) attributes: std::collections::HashMap<String, Value>,
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
}

#[derive(Debug, Clone)]
pub(crate) struct SharedPromise {
    inner: Arc<(Mutex<PromiseState>, Condvar)>,
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
    inner: Arc<(Mutex<ChannelState>, Condvar)>,
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
        // Oversized variants used to hold their payloads inline, making `Value`
        // 72 bytes (every clone/move pays for the largest variant). Boxing the
        // big payloads shrank it: Capture+BigRat (72->64), CustomTypeInstance
        // (64->56), Uni+RegexWithAdverbs+CustomType (56->48). Remaining 40-byte
        // variants (Proxy, Enum, ...) are boxed in follow-up steps. Guard against
        // layout regressions.
        let sz = std::mem::size_of::<Value>();
        assert!(sz <= 48, "size_of::<Value>() = {sz}, expected <= 48");
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
        assert!(matches!(map.get("a"), Some(Value::Int(2))));
    }

    #[test]
    fn insert_through_creates_missing_entry() {
        let mut map = HashMap::new();
        Value::hash_insert_through(&mut map, "b".to_string(), Value::Int(7));
        assert!(matches!(map.get("b"), Some(Value::Int(7))));
    }

    #[test]
    fn insert_through_writes_through_container_ref_cell() {
        // A `:=`-bound element holds a shared `ContainerRef` cell; a later
        // assignment to the key must write *through* the cell (preserving the
        // binding), not replace the entry with a bare value.
        let cell = Arc::new(Mutex::new(Value::Int(1)));
        let mut map = HashMap::new();
        map.insert("k".to_string(), Value::ContainerRef(cell.clone()));
        Value::hash_insert_through(&mut map, "k".to_string(), Value::Int(99));
        // The entry is still the same cell (binding preserved)...
        assert!(matches!(map.get("k"), Some(Value::ContainerRef(_))));
        // ...and the alias observes the new value through the cell.
        assert!(matches!(*cell.lock().unwrap(), Value::Int(99)));
    }
}
