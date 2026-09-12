# ADR-0077: A call's locals are a window into one contiguous stack, not a pooled `Vec`

- Status: **Accepted** (Slices 0 and 2 implemented; Slice 1 withdrawn into Slice 2; the leading-parameter form's *fill* half implemented via `params_fill_frame`, its argument-move half and Slice 3 still open — see the "What Slice N actually built" sections)
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

**Most of this landed without the fusion, and the fusion turned out not to be
the interesting part.** `CompiledFunction::params_fill_frame` is true when the
parameter slots are `0..n` *and* nothing else is a local — `fib`, `tak`, every
leaf accessor. The light call path then opens the frame **empty** and the bind
loop pushes each bound parameter in order, so each slot is written once instead
of twice (`Nil`-fill then overwrite) and the out-of-line `Vec::resize` that did
the fill disappears from the profile entirely. Measured, same tree, control
included:

| program | JIT on | | JIT off | |
| --- | ---: | ---: | ---: | ---: |
| `fib(22)` | 152 097 034 → 147 907 328 | **−2.75%** | 259 518 024 → 255 332 694 | **−1.61%** |
| `tak(14,7,0)` | 1 627 600 958 → 1 573 773 245 | **−3.31%** | 2 635 348 286 → 2 581 526 152 | **−2.04%** |
| a `while` loop (control) | 860 435 718 → 860 432 596 | −0.0004% | 1 364 731 677 → 1 364 731 057 | −0.00005% |

The trap worth recording: the bind loop `continue`s when a parameter is omitted
and its precomputed fill is absent. Under the sized frame that slot keeps the
`Nil` it was filled with; under the push it would be *skipped*, landing every
later parameter one slot low. The push path pushes that `Nil` explicitly, and
`Locals::put_param_slot` carries a `debug_assert_eq!` on the frame length so the
`gc-stress` / `jit-stress` jobs check the alignment suite-wide.

What `locals_base = args_base` would add on top is only the argument *move* —
the bind loop's `mem::replace` out of the operand stack — since the fill is
already gone. That is the part needing operand-stack fusion, and it is now a
smaller prize than it looked.

Only `vm_call_light` takes this path so far. `vm_call_light_typed` and
`vm_call_fast` bind through differently shaped loops (alias seeding, a separate
plan), so they keep the sized frame until someone measures whether a shared
abstraction is worth its cost.

## Why this is worth an ADR rather than a slice

`Interpreter::locals` is read or written at **464 sites across 62 files**, of
which **330 are plain `self.locals[i]` indexing** and the rest are structural
(frame save/restore, whole-array installs, snapshots). The change also reaches
the JIT's view of `Interpreter`'s layout. That is a migration to sequence
deliberately, which is what the rest of this document does.

(#7562 counted 484/60 and this ADR first said 507/65; both over-count. A plain
`self.locals` grep also matches `CompiledCode::locals: Vec<String>` — the local
*names* — in `src/opcode.rs`, which is an unrelated field. The numbers above
exclude it.)

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

**Slice 0 — a newtype chokepoint (no representation change). SHIPPED; see
"What Slice 0 actually built" below, which supersedes the accessor plan this
paragraph originally described.** The plan was to introduce
`local(i)` / `local_mut(i)` / `set_local(i, v)` / `locals_slice()` accessors on
`Interpreter` and convert every `self.locals[i]` site to them, so that the
representation swap would touch ~50 sites instead of all of them.

That was the wrong shape. Converting the index sites is churn in service of a
boundary that a newtype provides for free, so what shipped is a
`#[repr(transparent)] struct Locals(Vec<Value>)` with `Index`/`IndexMut` and
`Deref<Target = [Value]>`: `self.locals[i]`, `.len()`, `.get()`, `.iter()` and
the `&self.locals` → `&[Value]` coercions all keep working verbatim, and the
representation still has exactly one home. `Index` is implemented explicitly
rather than left to `Deref` so that Slice 2 can address `stack[base + i]` with a
single bounds check instead of slicing the window and then indexing it.

**Slice 1 — frame bookkeeping. WITHDRAWN; folded into Slice 2.** It read
"`VmCallFrame::saved_locals: Vec<Value>` → `saved_locals_base: usize`, still on
a pooled `Vec` for the live frame, so this slice is testable on its own". That
state does not exist: a base index means nothing until the frames share a stack,
so there is nothing for the intermediate slice to be tested against. Its two
pieces — the frame field and the `gc_roots` collapse — landed with Slice 2.

**Slice 2 — the representation. SHIPPED, except the leading-parameter fast
path; see "What Slice 2 actually built".** The shared stack, the deletion of
`locals_pool`/`take_locals_from_pool`/`recycle_locals`, the frame field, the
`gc_roots` collapse, and the JIT's base. The leading-parameter
`locals_base = args_base` form is **not** in it — see open question 1, which the
implementation settled differently from the way this ADR first framed it.

**Slice 3 (optional, separable) — fold `outer_scope_locals` in.**
`outer_scope_locals: Vec<Vec<Value>>` (`src/runtime/mod.rs:3582`) is a second
stack-of-locals for block scopes. Once locals live on a contiguous stack, a
block scope is just another base index and that field can go away. Not required
by this ADR; recorded so the next reader sees the whole shape.

### What Slice 2 actually built

The representation is one vector plus the executing frame's base, both inside
`Locals` rather than as two `Interpreter` fields:

```rust
pub(crate) struct Locals { slots: Vec<Value>, base: usize }
```

Keeping the stack *inside* the type is what makes the migration a local one: the
`Interpreter` still has a single `locals` field, `Index` becomes
`slots[base + slot]`, `Deref` becomes `slots[base..]`, and no call site needs to
borrow two `Interpreter` fields at once. Slice 0's newtype paid off exactly as
intended — the ~330 index sites and the ~100 `Deref` sites did not move again.

The ~40 structural sites reduced to three shapes:

| shape | API | example |
| --- | --- | --- |
| open / close a frame | `push_frame(n) -> CallerFrame`, `pop_frame(CallerFrame)` | every `mem::take(&mut self.locals)` … restore pair; `push_call_frame`/`pop_call_frame`; the retired pool |
| replace the executing frame's slots | `refill_slots(n)`, `refill_from(&[Value])`, `resize_slots(n)` | `self.locals = vec![Nil; n]`; the top-level `run` entry, which sizes then seeds and so must keep surviving slots |
| copy a frame out or in | `to_vec()`, `push_frame_from`, `install_root_frame` | a suspended `gather` coroutine, an inline `CATCH` handler, a hyper/race worker's seed |

**A frame handle is not `Copy`, and `pop_frame` consumes it.** This is the one
piece of the design that was *not* obvious up front, and it was found the hard
way: an earlier version returned a bare `usize`, documented `pop_frame` as
idempotent, and its own unit test failed — closing a frame twice truncates the
*caller's* slots, because the second call truncates to a base that is already
the caller's. The old moved `Vec<Value>` had made a double close a compile
error, and several paths (e.g. `vm_arith_int_ops`) close on more than one
exclusive exit branch, where that move was the only proof the branches really
were exclusive. A non-`Copy` handle restores exactly that protection.
`#[must_use]` then immediately found the hyper/race worker pushing a root frame
it never closes — legitimate, since a worker VM is discarded whole, and now
stated as `install_root_frame`.

**The JIT.** Slice 0's `size_of::<Locals>()` assertion failed the build until
Tier B's GetLocal emitter learned the base, which is what it was for. The
emitter loads `locals_base` alongside the `Vec` header words on every slot
access — a push can reallocate and every call moves the base, so neither may be
cached — reads the absolute element `base + idx`, and bounds-checks against
`len - base` behind a `base <= len` guard. `JitLayout::locals` now means the
*stack* inside the field, with the two byte offsets exported from `Locals` as
consts because its fields are private.

**GC roots — the one way this slice could have broken silently.** `gc_roots`
visited `&self.locals` plus each frame's `saved_locals`. `Deref` now yields only
the executing frame, so a root scan reading through it would miss every
suspended caller's slots and free live values while the program kept running. It
visits `all_slots()` instead and the per-frame walk is gone, which also makes
the scan one slice visit rather than one per frame.
`t/locals-frame-stack-gc-roots.t` pins the end-to-end property (values, cyclic
objects and containers held only by deeply suspended frames survive, including
past the retired pool's 64-entry bound), and a unit test pins that `all_slots()`
and the `Deref` window disagree in the direction they must.

**Two deliberate copies remain.** `vm_call_method_compiled` reads the enclosing
frame's slots through `outer_local_slots` *and* writes the block's
free-variable writes back into them, then restores the whole array on exit — so
the detached copy is the semantics, not an artifact of the old representation.
Writing through to the live region below `base` instead would change behavior on
the panic path (where the restore never runs), so it is left for its own slice.
And the shared-container propagation in `vm_var_assign_coerce`, which walked
`call_frames` writing `frame.saved_locals[i]`, now derives each saved frame's
`[base, end)` region — walking downwards, a frame's region ends where its own
base begins, and the topmost ends at the executing base — collects the absolute
indices during the walk (which borrows `call_frames` mutably for the env
inserts) and applies them afterwards.

### What Slice 0 actually built

`src/runtime/locals.rs` — `#[repr(transparent)] struct Locals(Vec<Value>)` with
`Index`/`IndexMut`, `Deref`/`DerefMut` to `[Value]`, a hand-written `Clone` (so
`clone_from` keeps `Vec`'s buffer reuse, which the hyper/race worker seeding
relies on), and a five-method inherent API that names every *structural* thing
the VM does to a frame: `new`, `nils(n)` (a fresh un-pooled frame),
`resize_slots(n)` (`Vec::resize` semantics, for the top-level `run` entry that
sizes then seeds), `refill(n)` + `release()` (the pool's whole API), and
`from_vec`/`to_vec` (owned snapshots that outlive a frame — a suspended `gather`
coroutine, an inline `CATCH` handler frame, a hyper/race worker's slots crossing
a thread boundary).

The result is 18 files changed, +94/−39, instead of the ~330 mechanical edits
the accessor plan implied. The 330 index sites and the ~100 `Deref` sites
(`.len()`, `.get()`, `.iter()`, `&self.locals`) did not have to be touched at
all, which is the point: the representation now has one home without a churn
commit in front of it.

Two things fell out of it that the plan had not anticipated:

- **The args-scratch pool had to be separated first.** Three sites in
  `vm_call_func_ops.rs` called `take_locals_from_pool(0)` and `extend`ed it —
  borrowing the *locals* pool as an argument buffer for the named/spec light
  call paths. A window into a shared stack cannot be handed out as an owned
  buffer, so this is a hard blocker for Slice 2 (it is ADR-0077 open question 3).
  Slice 0 gives those sites their own `args_scratch_pool: Vec<Vec<Value>>` with
  the same bound and the same clear-before-return discipline, so the two uses
  stop being conflated. Behavior is unchanged; the only cost is up to 64 more
  retained buffers.
- **The JIT coupling now has a compile-time tripwire.** `vm_jit_layout` applies
  the probed `Vec<Value>` word offsets at `offset_of!(Interpreter, locals)`,
  which is sound only while `Locals` is transparent over the vector. A
  `const { assert!(size_of::<Locals>() == size_of::<Vec<Value>>()) }` next to
  the existing probe assertion makes Slice 2 fail to compile until
  `vm_jit_tier_b`'s GetLocal emitter learns the base, rather than silently
  emitting native code against the wrong words.

`Locals`' module doc records the two contracts Slice 2 must preserve: `Index`
addresses the *current* frame (so it must add the base), and `Deref` yields
exactly the current frame's slots (so `.len()` and `.iter()` speak about this
frame and nothing below it). The second is what makes open question 1 — one
stack or two — a real question rather than a preference: a locals region sharing
the operand stack would need a per-frame length here, not "everything above
`base`".

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
  runs — but see "The bench CI could not resolve Slice 2" below: on this
  benchmark set, at this size of change, it cannot, and retired instructions are
  the only honest oracle.
- `bench-tak` and `bench-fib` are the targets (call-shaped); pick a control that
  the change cannot affect.

### The bench CI could not resolve Slice 2 (2026-09-08)

Slice 2 merged as `e5c260c`. Its `bench-history.tsv` rows look like a win —
`bench-fib+jit` 0.1345 → 0.1306 s, `bench-tak+jit` 0.1690 → 0.1583,
`method-call+jit` 0.1453 → 0.1329, `bench-ctor+jit` 0.3472 → 0.2341 — and
**every one of them is noise**. The next, unrelated commit bounces back:
`bench-ctor+jit` reads 0.3472 / 0.2341 / 0.3495 / 0.2326 / 0.3475 / 0.3604 over
six consecutive main commits, a ~48% bimodal swing with nothing in the diffs to
explain it, and plain `bench-fib` jumps to 0.2974 (+13%) on an unrelated commit
too. The `runner` column reads `4c-x86_64-ubuntu24` throughout, so this is not
the host-class effect PERFORMANCE.md's measurement notes correct for — the same class
evidently spans hosts that differ by more than the change being measured.

So a single-commit change of a few percent is **below this series' resolution**,
and "the bench CI is the source of truth" cannot be satisfied here by waiting for
more rows. What it can still do is catch a large regression, and it did not show
one.

### What Slice 2 actually cost, in retired instructions (2026-09-08)

Measured with callgrind on `--profile profiling` builds of the merge commit and
its parent — exact, load-independent, and the oracle #7579's method notes
prescribe for a change whose effect is in code shape rather than in work done.
`fib(22)`, 57 312 calls. Output verified identical (17711) before comparing, per
the same notes.

| build | JIT on | | JIT off | |
| --- | ---: | ---: | ---: | ---: |
| `9cf38ab` (pre-Slice-2) | 152 721 942 | — | 260 873 311 | — |
| `e5c260c` (Slice 2 as merged) | 152 831 725 | **+0.07%** | 259 743 537 | **−0.43%** |
| one-step frame sizing | 149 633 356 | **−2.02%** | 253 330 506 | **−2.89%** |

**Slice 2 as merged was instruction-count neutral**, and the reason is a defect
in it rather than a limit of the design. Deleting the pool removed
`recycle_locals` (2.98M Ir, 1.95%) as intended, but the migration opened each
callee frame in two steps — `push_frame(0)` at the save point, then
`refill_slots(num_locals)` once the size was known — so every call paid **two**
out-of-line `Vec::resize`/`extend_with` calls where the pool had paid one:
`Vec::resize` inclusive went 3.11M → 5.28M, giving back 2.17M of the 2.98M.

Opening the frame at its real size in one step (the three light call paths;
nothing between the save point and the parameter bind reads `self.locals`) puts
`Vec::resize` back at 3.10M and yields the numbers in the third row. `push_frame`
and `refill_slots` also skip the resize entirely for an empty frame, which every
`push_call_frame` opens.

That leaves the *remaining* half of the original cluster exactly where the
cross-check below put it: `resize` itself, ~2% — which is what the
leading-parameter form removes, since an argument already sitting in the cell its
slot wants needs no fill at all. Slice 2 is the precondition for that, not a
substitute for it.

### Cross-check of the ~5.7% claim (2026-09-08)

The 5.7% figure predates the September sweep, so it was re-derived on `4d684e4`
before this ADR was written. `perf` was unavailable in the container used (not
installed; `perf_event_paranoid=2`), so the cross-check used **callgrind**,
whose instruction counts are exact and load-independent — the right tool for
confirming a cluster is still worth attacking, and the wrong one for any
wall-clock claim. **These are not bench-CI numbers**; and per the section above,
the bench CI turned out unable to resolve a change of this size on this runner,
so the retired-instruction table there is the measurement Slice 2 owed.

`--profile profiling` build, JIT on (default), `fib(22)` — 57 312 calls,
152 774 784 Ir total. Inclusive cost, so each row already contains the
allocation it triggers:

| what | Ir | share | per call |
| --- | ---: | ---: | --- |
| `take_locals_from_pool`'s `Vec::resize` | 3 101 409 | **2.03%** | 57 312× — once per call |
| `recycle_locals` | 2 984 559 | **1.95%** | 57 312× — once per call |
| **locals cluster total** | | **~4.0%** | |

So the cluster is still there and still costs about what #7562 said, on a
benchmark whose callee has exactly one local. The `perf` figure being higher
(5.7%) is consistent: it was `fib(30)`, where the call path dominates more, and
cycles weight the malloc traffic that instruction counts do not.

**One attribution in #7562's table is worth correcting.** Its
`<Vec<T,A> as Drop>::drop` row (0.96%) is not the locals vector — the locals
vector is recycled, not dropped. In this run `Vec::drop` is called **171 936
times, three per call**, from `call_compiled_function_positional_light_at`
(2.14% inclusive), and those are the *other* `mem::take`n frame fields
(`loop_local_vars`, `block_declared_vars`, `active_loop_param_names`, …) being
dropped on restore. Together with the 3.90% attributed to `core::mem` inside
the same function, that is a **neighbouring cluster this ADR does not close**:
the per-call frame saves eight independent `Vec` fields, and locals is only the
one that also allocates. Whether the same contiguous-frame treatment should
swallow the other seven is a follow-on question, deliberately out of scope here
so that Slice 2 measures one thing. **It has since been answered for five of the
seven** — the per-call scope stacks — in
[ADR-0078](0078-per-call-scope-stacks-are-windows-not-moved-vectors.md), which
gives them the same base-index representation and measures `fib(22)` at −5.94%
(JIT on) / −3.46% (off). The other two, `frame_authoritative` and `frame_owned`,
are whole-set registers that consumers *assign* rather than push, so a base index
means nothing for them and they keep their `mem::take`.

Also visible: 20 871 `__rdl_alloc` calls for 57 312 calls — the pool misses
about a third of the time even in `fib`'s tree recursion, where returns refill
it constantly. A linear recursion deeper than `LOCALS_POOL_MAX = 64` misses it
structurally (the descent finds the pool empty), so such a program pays a
malloc *and* a free per call today. That cost does not appear in #7562's
profile at all — `fib`'s depth is 30 — and it is the one place where the win
could be much larger than a few percent.

## Consequences

- The per-call cost of a locals frame drops from six `Vec` operations to one
  `resize`, and to **zero** for a callee whose locals are exactly its leading
  parameters.
- A deep recursion now grows one buffer instead of cycling a bounded free list,
  so the pool's `LOCALS_POOL_MAX = 64` ceiling stops mattering. Past that depth
  a linear recursion currently pays a malloc and a free **per call** (the
  descent always finds the pool empty), which no existing benchmark measures —
  see the measurement section.
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

## Open questions

1. **One stack or two? — settled as two, and the reasoning changed.** This was
   framed as a measurement between reusing `self.stack` (making
   `locals_base = args_base` free) and a sibling stack (costing an argument
   copy). Slice 2 shipped the sibling — the slots live in `Locals` — and the
   reason is not a measurement but the `Deref` contract Slice 0 established:
   the executing frame is `[base ..]`, "everything above the base". An operand
   push during body execution would land inside that window, so sharing
   `self.stack` requires a per-frame *length* as well as a base, and every
   `.len()` / `.iter()` / `&self.locals` site starts meaning something subtly
   different. That is a second, larger change wearing the first one's clothes.

   The leading-parameter `locals_base = args_base` form is therefore **not
   shipped and not free**: it needs the fused stack, hence the per-frame length,
   hence its own slice with its own measurement. Slice 2's win is the pool, the
   `Vec` header traffic and the `mem::take` pairs; the remaining argument move
   loop is the next thing to attack, not something this slice quietly dropped.
2. **Overflow policy.** A growable `Vec` inherits Rust's allocation failure
   behavior on runaway recursion. mutsu has no explicit call-depth limit today;
   whether to add one (a real VM's stack limit, raising an `X::` rather than
   aborting) is a separate decision and was deliberately not smuggled into
   Slice 2, which changes where slots live but not how deep they may go.
3. **The named light path — closed by Slice 0.** `call_compiled_function_light`
   used `take_locals_from_pool(0)` as a general argument buffer, which a window
   into a shared stack cannot serve. Slice 0 gave those three sites their own
   `args_scratch_pool`, so Slice 2 could delete the locals pool outright. What
   remains of #7579 item 1 is the *copy* on the named path, not the pool.
4. **The two remaining detached copies** (`vm_call_method_compiled`'s enclosing
   frame, `vm_var_assign_coerce`'s cross-frame propagation) — see "What Slice 2
   actually built". Both are now expressed against the shared stack; the first
   still copies because writing through changes panic-path behavior.
