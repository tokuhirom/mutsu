# Memory Management & GC Design

## Goal

mutsu aims to support long-running programs (daemons, servers). The current
"clone everything, free at exit" strategy causes unbounded memory growth in
such scenarios. This document describes an incremental plan to fix that.

## Current state

- `Value` derives `Clone`. Every operation (variable access, function call,
  closure capture, array/hash copy) performs a deep clone.
- `Value` is required to be `Send + Sync` (compile-time assertion in
  `value/mod.rs`) for thread support (Promise, Channel, `clone_for_thread`).
  This rules out `Rc<RefCell<>>`.
- `Interpreter.env` is `HashMap<String, Value>`. Closure creation clones the
  entire env (~21 `.env.clone()` call sites, plus ~123 `env.insert()` sites).
- Circular references exist via `&?BLOCK` (a Sub whose `env` contains itself).
- `Arc` is already used for `LazyList`, `SharedPromise`, `SharedChannel`.

### Problems for long-running programs

| Problem | Mechanism |
|---------|-----------|
| Circular references never freed | `&?BLOCK` creates Sub.env -> Sub cycle |
| Closure env accumulates | Every function call clones the full env |
| END phasers capture full env | `push_end_phaser` clones `self.env` |
| Large collection cloning | Array/Hash deeply cloned on every pass |
| state vars persist forever | `state_vars: HashMap<String, Value>` |

## Design

### Phase 1: Arc-wrap heap-allocated Value internals

Replace deep clones with shared references for heap-allocated data. Cloning an
`Arc` is just an atomic increment instead of a deep copy.

#### Changes to Value

```rust
#[derive(Debug, Clone)]
pub enum Value {
    // Scalar types: unchanged (Copy-like cost)
    Int(i64),
    Num(f64),
    Bool(bool),
    Nil,
    HyperWhatever,
    Rat(i64, i64),
    FatRat(i64, i64),
    Complex(f64, f64),
    Range(i64, i64),
    RangeExcl(i64, i64),
    RangeExclStart(i64, i64),
    RangeExclBoth(i64, i64),

    // Heap types: wrap in Arc
    Str(Arc<String>),              // was: String
    BigInt(Arc<NumBigInt>),        // was: NumBigInt
    Array(Arc<Vec<Value>>),        // was: Vec<Value>
    Hash(Arc<HashMap<String, Value>>),  // was: HashMap
    Set(Arc<HashSet<String>>),     // was: HashSet
    Bag(Arc<HashMap<String, i64>>),
    Mix(Arc<HashMap<String, f64>>),

    GenericRange {
        start: Arc<Value>,         // was: Box<Value>
        end: Arc<Value>,
    },
    Pair(String, Arc<Value>),      // was: Box<Value>
    Mixin(Arc<Value>, Arc<HashMap<String, Value>>),

    Sub {
        package: Arc<String>,
        name: Arc<String>,
        params: Arc<Vec<String>>,
        body: Arc<Vec<Stmt>>,
        env: Arc<HashMap<String, Value>>,  // KEY CHANGE
        id: u64,
    },
    Instance {
        class_name: String,
        attributes: Arc<HashMap<String, Value>>,  // was: HashMap
        id: u64,
    },
    Junction {
        kind: JunctionKind,
        values: Arc<Vec<Value>>,
    },
    Slip(Arc<Vec<Value>>),
    // ... remaining variants already use Arc
}
```

#### Key principles

- **Read path unchanged**: Pattern matching on `Arc<T>` works via auto-deref.
  `match &*arc_vec { ... }` or `arc_str.as_str()`.
- **Write path uses `Arc::make_mut`**: When mutation is needed (push to array,
  insert to hash), `Arc::make_mut()` provides clone-on-write semantics. If
  there is only one reference, it mutates in place; otherwise it clones.
- **Value::clone() becomes cheap**: Cloning a Value with Arc internals only
  increments reference counts, no deep copy.
- **Send + Sync preserved**: `Arc<T>` is Send + Sync when T is Send + Sync.

#### Migration strategy

This is a large mechanical refactor. Approach:

1. Change the `Value` enum definition.
2. Fix all construction sites (wrap in `Arc::new()`).
3. Fix all pattern match sites (add derefs where needed).
4. Fix all mutation sites (use `Arc::make_mut()`).
5. Run `cargo build` and fix remaining errors iteratively.

The `env: Arc<HashMap<String, Value>>` change has the biggest impact:
- `self.env.clone()` in `call_sub_value`, `push_end_phaser`, etc. becomes
  an Arc clone (O(1) instead of O(n)).
- `self.env.insert(k, v)` requires `Arc::make_mut(&mut self.env).insert(k, v)`.
  When env has refcount 1 (common case), this is a no-op wrapper.

### Phase 2: Weak references for &?BLOCK

The `&?BLOCK` variable creates a cycle: a Sub's env contains a reference to
itself. With Phase 1's `Arc`, this cycle prevents deallocation.

#### Solution: dedicated Weak variant

```rust
pub enum Value {
    // ... existing variants ...

    /// A weak reference to a Value (used for &?BLOCK self-references).
    /// Upgrade to the strong value when accessed; returns Nil if expired.
    WeakSub(Weak<SubData>),
}
```

Where `SubData` is extracted from the Sub variant:

```rust
pub(crate) struct SubData {
    pub package: Arc<String>,
    pub name: Arc<String>,
    pub params: Arc<Vec<String>>,
    pub body: Arc<Vec<Stmt>>,
    pub env: Arc<HashMap<String, Value>>,
    pub id: u64,
}

pub enum Value {
    Sub(Arc<SubData>),
    WeakSub(Weak<SubData>),
    // ...
}
```

#### Changes to &?BLOCK insertion

In `resolution.rs` and `builtins.rs`, where `&?BLOCK` is inserted:

```rust
// Before (Phase 1):
let block_self = Value::Sub(Arc::new(SubData { ... env: new_env.clone(), ... }));
Arc::make_mut(&mut new_env).insert("&?BLOCK".to_string(), block_self);

// After (Phase 2):
let sub_data = Arc::new(SubData { ... env: new_env.clone(), ... });
let weak_ref = Value::WeakSub(Arc::downgrade(&sub_data));
Arc::make_mut(&mut new_env).insert("&?BLOCK".to_string(), weak_ref);
let block_self = Value::Sub(sub_data);
```

#### Changes to &?BLOCK access

When `&?BLOCK` is accessed (variable lookup), upgrade the weak reference:

```rust
// In env lookup:
match value {
    Value::WeakSub(weak) => {
        match weak.upgrade() {
            Some(strong) => Value::Sub(strong),
            None => Value::Nil,  // already freed
        }
    }
    other => other.clone(),
}
```

### Phase 3 (future): Cycle collector

For user-created cycles (`$a[0] = @a`, mutual object references), a proper
cycle collector will be needed. This is deferred until the need arises.

Options:
- **Backup mark-and-sweep**: Periodically scan all live Values from roots
  (Interpreter.env, stack, locals) and free unreachable Arc values.
- **Python-style cycle detector**: Track container objects (Array, Hash,
  Instance, Sub) in a generation list; periodically scan for reference cycles.
- **Epoch-based collection**: Use a GC epoch counter; values created before
  a certain epoch that are unreachable get collected.

## Impact on existing tests

All existing tests should pass unchanged. The refactor is purely internal:
- Value semantics are preserved (clone-on-write matches deep-clone behavior).
- Thread safety is preserved (Arc is Send + Sync).
- Performance should improve (fewer deep clones).

## Implementation order

1. Phase 1a: Arc-wrap `Sub.env` (biggest win, most `.env.clone()` call sites).
2. Phase 1b: Arc-wrap `Array`, `Hash`, `Str`, other heap types.
3. Phase 2: Extract `SubData`, add `WeakSub`, fix `&?BLOCK` cycle.
4. Run full test suite (`make test && make roast`) at each step.
