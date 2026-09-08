# ADR-0077: A call's locals are a window into one contiguous stack, not a pooled `Vec`

- Status: **Proposed**
- Date: 2026-09-08
- Related: [#7562](https://github.com/tokuhirom/mutsu/issues/7562) (the perf
  finding this ADR unblocks), [#7579](https://github.com/tokuhirom/mutsu/issues/7579)
  item 1 (the args/locals `Vec` churn cluster — the *args* half already shipped),
  [ADR-0004](0004-jit-strategy.md) (the JIT reads `Interpreter::locals` directly),
  [ADR-0006](0006-baseline-interpreter-optimizations.md),
  [ADR-0066](0066-call-dispatch-inline-cache.md) (the preceding item in the same
  call-path campaign), [docs/vm-dual-store.md](../vm-dual-store.md) (slot-indexed
  locals as the eventual source of truth)

## Context

Every call gives its callee a `locals` array. Today that array is a
`Vec<Value>` borrowed from a free list:

```rust
// src/vm/vm_env_helpers.rs
pub(super) fn take_locals_from_pool(&mut self, num_locals: usize) -> Vec<Value> {
    let mut v = self.locals_pool.pop().unwrap_or_default();
    v.clear();
    v.resize(num_locals, Value::NIL);
    v
}

pub(super) fn recycle_locals(&mut self, mut used: Vec<Value>) {
    const LOCALS_POOL_MAX: usize = 64;
    if self.locals_pool.len() < LOCALS_POOL_MAX {
        used.clear();
        self.locals_pool.push(used);
    }
}
```

So per call the VM performs: a pool pop, a `clear`, an out-of-line
`resize(n, Value::NIL)`, a `std::mem::take(&mut self.locals)` to stash the
caller's vector (plus the symmetric restore on return), a `clear` that drops the
callee's slot values, and a pool push. Six operations on a 24-byte `Vec` header
and its heap buffer, for storage whose lifetime is exactly LIFO.

On `bench-fib` — whose only local is the parameter `$n` — #7562 measured that
cluster at **~5.7% of the profile**:

| symbol | self time |
| --- | ---: |
| `recycle_locals` | 2.36% |
| `<Vec<T,A> as Drop>::drop` | 0.96% |
| `Vec::extend_with` | 0.90% |
| `drop_in_place<[Value]>` | 0.85% |
| `Vec::resize` | 0.68% |

That is ~5.7% spent managing a **one-element vector**.

### This is the second half of an already-shipped change

The same call path used to materialize an *argument* buffer the same way. That
half is closed: `news/2026-09/positional-light-call-binds-from-the-stack.md`
removed the argument buffer entirely by letting the callee take an `args_base`
index and bind by move straight out of `self.stack`, truncating on every exit
path (`bench-tak` −10.6%, `fib` −7.2%, `bench-fib` −6.7% locally, both
orderings). The mechanism this ADR proposes for locals is **the same mechanism,
already proven in the same function** — a contiguous `Vec<Value>` addressed by a
per-frame base index, with the frame's region released by `truncate`.

## Decision

Represent the locals frame as a window into a single contiguous stack:

```rust
locals_stack: Vec<Value>,   // all live frames' slots, contiguous
locals_base: usize,         // this frame's slot 0
```

A call extends `locals_stack` by `num_locals` (amortized O(1), no pool, no
allocation); a return truncates it back to the caller's base. `locals_pool`,
`take_locals_from_pool`, `recycle_locals`, and the `mem::take`/restore pair all
disappear: the caller's slots are simply the region below the callee's base.

`VmCallFrame::saved_locals: Vec<Value>` becomes `saved_locals_base: usize`.

**The window is an index, never a pointer.** Anything that can grow the stack
invalidates a raw base pointer, and growth is exactly what a deep recursion
does, so no Rust-side or JIT-side code may cache the data pointer across a
push. (§"The JIT is not a blocker" explains why this costs the JIT nothing —
it already reloads the pointer per access.)

### The strongest form: for a leading-parameter callee, `locals_base = args_base`

The bind loop in `call_compiled_function_positional_light_at`
(`src/vm/vm_call_light.rs:378`) currently moves each argument out of
`self.stack[args_base + param_idx]` into `self.locals[*slot]`, then truncates
the stack back to `args_base`.

Parameter slots come from the compiler's `local_map`
(`CompiledFunction::precompute_param_local_slots`, `src/opcode.rs:8860`), and an
ordinary sub declares its parameters first — so `param_local_slots` is normally
the identity prefix `[0, 1, …, n-1]`. When it is, **the argument already sits in
the stack cell the local slot wants**. Setting `locals_base = args_base` and
extending by `num_locals - num_params` `Value::NIL`s makes a call whose locals
are exactly its parameters — `fib`, `tak`, every leaf accessor — pay *nothing*
for its locals frame: no resize, no pool, no move loop, and no
truncate-then-refill.

That identity check is a per-callee property, so it is settled once at
registration time as a `bool` on `CompiledFunction`, not re-derived per call.

Whether the locals stack should be `self.stack` itself or a sibling `Vec` is
left to Slice 2's measurement (see "Open questions").

## Why this is worth an ADR rather than a slice

`self.locals` is read or written at **507 sites across 65 files** (up from the
484/60 #7562 recorded). The change is mechanical at 331 of them and structural
at the rest, and it reaches the JIT's view of `Interpreter`'s layout. That is a
migration to sequence deliberately, which is what the rest of this document
does.

It is, however, **materially less dangerous than #7562 assumed**. Three of the
four reasons that issue gives for its blast radius do not survive reading the
code:

### 1. Nothing hands the locals vector to another owner

#7562 states that `std::mem::take(&mut self.locals)` is "load-bearing in
`call_compiled_function_positional_light_at`, `push_call_frame`, and the
closure/thread capture paths — they hand the vector to somebody else, which a
window into a shared stack cannot do without copying."

All 47 ownership sites were audited. Every one of them is the same pattern:
**stash the current locals, install a fresh (usually empty) array, run something
inline, restore.** `src/vm/vm_helpers_lazy_pull.rs:90` (gather body),
`src/vm/vm_arith_int_ops.rs:208` (repeat thunk),
`src/runtime/catch_inline.rs:61` (handler frame),
`src/vm/vm_misc_reduction_scan.rs:294`, `src/vm/vm_closure_dispatch.rs:867` — all
of them. None transfers ownership to a value that outlives the restore.

Nor does thread spawn: a spawned interpreter is constructed with
`locals: Vec::new()` (`src/runtime/runtime_thread.rs:869`) and inherits nothing.

Under a shared stack these sites become **simpler**, not harder: "install a
fresh empty locals array" is "push a new base", the same primitive a call uses.
The only site that genuinely needs a copy is
`src/vm/vm_hyper_race_parallel.rs:64`, which snapshots the slots to diff them
afterwards — and it is cold, so a `to_vec()` there is free.

### 2. The JIT is not a blocker

#7562 notes that `vm_jit_layout.rs` knows the offset of `Interpreter::locals`
and emits native code against it. It does — but look at what Tier B emits
(`src/vm/vm_jit_tier_b.rs:557`): for **every** slot access it reloads both the
`Vec`'s data pointer and its length out of the interpreter, then bounds-checks
`idx < len` and falls back to the shim when it fails.

Two consequences:

- Reallocation is **already** safe for the JIT, because it never caches the
  data pointer across an access. This is what makes the index-not-pointer rule
  above cheap rather than restrictive.
- The migration is a small, mechanical edit to one code generator: load
  `locals_base` as well, address `lptr + (base + idx) * 8`, and bounds-check
  against `len - base`. One extra load and one add on a path that already
  performs two loads, a compare, a branch, and a NaN-box kind probe.

### 3. `saved_locals` per frame gets cheaper, including for GC

`gc_roots.rs:78` currently walks `self.call_frames` and visits each frame's
`saved_locals` slice separately. With one contiguous stack, the whole set of
live slots is a single `visit_slice(visitor, &self.locals_stack)` — fewer
visits, no per-frame indirection, and one less place for a frame's slots to be
missed by a root scan.

### 4. What remains genuinely hard

The reason that *does* survive is borrow structure: any site that needs a
caller slot and a callee slot at once currently holds two separately-owned
`Vec`s and will hold two disjoint regions of one `Vec`. Those sites must be
re-expressed (index arithmetic, `split_at_mut`, or a copy through a local),
and they are the reason Slice 0 exists — an accessor layer makes them
enumerable before the representation changes underneath them.

`src/vm/vm_var_get_ops.rs:606`'s `outer_scope_locals` reads and
`src/vm/vm_misc_scope.rs:537`'s block-scope pushes are the other structural
consumers; see Slice 3.

## Migration order

**Slice 0 — accessors (no representation change).** Introduce
`#[inline] fn local(&self, i) -> &Value`, `local_mut`, `set_local`,
`locals_len`, `locals_slice`, `locals_slice_mut` on `Interpreter`, and convert
the 331 `self.locals[i]` sites to them. Purely mechanical, zero behavior
change, no perf change (everything inlines to the same code). Its value is that
it shrinks the representation swap from 507 sites to ~50 and makes the
two-frame-borrow sites (§4 above) show up as compile errors rather than as
subtle aliasing later.

**Slice 1 — frame bookkeeping.** `VmCallFrame::saved_locals: Vec<Value>` →
`saved_locals_base: usize`; collapse `gc_roots`' per-frame visit into one slice
visit. Still on a pooled `Vec` for the live frame, so this slice is testable on
its own.

**Slice 2 — the representation.** `locals_stack` + `locals_base`; delete
`locals_pool`, `take_locals_from_pool`, `recycle_locals`; teach
`vm_jit_layout`/`vm_jit_tier_b` the base; add the leading-parameter
`locals_base = args_base` fast path. This is the slice the measurement below
belongs to.

**Slice 3 (optional, separable) — fold `outer_scope_locals` in.**
`outer_scope_locals: Vec<Vec<Value>>` (`src/runtime/mod.rs:3582`) is a second
stack-of-locals for block scopes. Once locals live on a contiguous stack, a
block scope is just another base index and that field can go away. Not required
by this ADR; recorded so the next reader sees the whole shape.

## Measurement protocol

#7579's "Method notes" section is the canonical protocol for this campaign and
governs here. In particular:

- **Run the correctness gate before the A/B.** A change that accidentally skips
  work measures faster; #7269 read −12.2% before `make test` corrected it to
  −8.2%.
- **Interleave** the A/B binaries; never measure them in sequence.
- **Report retired instructions alongside cycles.** Slice 0 in particular
  cannot move retired instructions at all (it is inlining-identical by
  construction), so a cycles delta there is the ~5% layout lottery and must not
  be reported as a win.
- Any number that reaches a document comes from the **bench CI**
  (`git show origin/bench-data:bench-history.tsv`), not from a session's local
  runs.
- `bench-tak` and `bench-fib` are the targets (call-shaped); pick a control that
  the change cannot affect.

### Cross-check of the ~5.7% claim (2026-09-08)

The 5.7% figure predates the September sweep, so it was re-derived on
`4d684e4` before this ADR was written. `perf` is unavailable in the container
used (not installed; `perf_event_paranoid=2`), so the cross-check used
**callgrind**, whose instruction counts are exact and load-independent — the
right tool for confirming that a cluster is still worth attacking, and the
wrong one for a wall-clock claim. See §"Cross-check result" below, which this
ADR records rather than the bench CI numbers Slice 2 must produce.

## Consequences

- The per-call cost of a locals frame drops from six `Vec` operations to one
  `resize`, and to **zero** for a callee whose locals are exactly its leading
  parameters.
- A deep recursion now grows one buffer instead of cycling a bounded free list,
  so the pool's `LOCALS_POOL_MAX = 64` ceiling (past which a deep recursion
  allocated and freed per call) stops mattering.
- The interpreter gains an invariant to hold: **no cached pointer into
  `locals_stack` across anything that can push a frame.** This is the one new
  footgun the change introduces, and the reason the ADR insists the window is
  an index.
- `locals` stops being a field another component can be handed. Any future
  consumer that wants an owned array must copy explicitly, which is a clearer
  contract than today's implicit `mem::take`.
- It moves `docs/vm-dual-store.md`'s target architecture forward: slot-indexed
  locals become the cheap store, which is the premise of making `env` a lazily
  materialized name view.

## Open questions (for Slice 2, not blocking this ADR)

1. **One stack or two?** Reusing `self.stack` for locals makes
   `locals_base = args_base` free but interleaves operand-stack and locals
   traffic in one buffer, where a `push` during body execution would sit above
   the callee's slots. A sibling `locals_stack` keeps the two disciplines
   separate at the cost of copying arguments into it. Measure both; the
   leading-parameter fast path is the reason to prefer reuse, and the operand
   stack's own `truncate` discipline is the reason it might not work.
2. **Overflow policy.** A growable `Vec` inherits Rust's allocation failure
   behavior on runaway recursion. mutsu has no explicit call-depth limit today;
   whether to add one (a real VM's stack limit, raising an `X::` rather than
   aborting) is a separate decision and should not be smuggled in here.
3. **The named light path.** `call_compiled_function_light` still copies into
   the same buffer shape (#7579 item 1) and also uses
   `take_locals_from_pool(0)` as a general scratch buffer
   (`src/vm/vm_call_func_ops.rs:344`). Deleting the pool means giving those
   sites a scratch `Vec` of their own; that is bookkeeping, not a design fork.
