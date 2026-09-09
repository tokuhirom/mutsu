# ADR-0078: A call's scope stacks are windows into shared vectors, not moved-out `Vec`s

- Status: **Accepted** (implemented; measurement in "What this cost, in retired instructions")
- Date: 2026-09-09
- Related: [#7562](https://github.com/tokuhirom/mutsu/issues/7562) (the perf
  finding whose profile named this cluster), [ADR-0077](0077-locals-are-a-window-into-a-contiguous-stack.md)
  (the same mechanism, applied to the slot array — this ADR is the follow-on it
  deliberately left out of scope), [ADR-0023](0023-binding-provenance-spawn-capture.md)
  (`active_loop_param_names`, one of the five stacks),
  [ADR-0027](0027-loop-frozen-value-capture-cascade.md)

## Context

Besides its slot array, a call carries five *scope stacks* — bookkeeping the VM
pushes and pops at block and loop boundaries:

| field | what a frame of it holds |
| --- | --- |
| `block_declared_vars` | the `my` names a `BlockScope` declared, to revert at block exit |
| `loop_local_vars` | the `my` names a loop body declared, for per-iteration capture semantics |
| `loop_local_saved_env` | the env values a loop-local declaration shadowed, to restore at loop exit |
| `active_loop_param_names` | the parameter names of the loops currently iterating (ADR-0023) |
| `active_loop_rw_param_names` | the `is rw` subset of the same |

Each must be **invisible to a callee**. A routine's own `my $x` runs before the
routine enters any block of its own, so without isolation it registers in the
*caller's* active `BlockScope` frame and is reverted at the caller's block exit —
`sub f($n){ my $r=0; { $r=10; f($n-1) if $n>0 }; $r }` returned 0 instead of 10.
The loop-local twin has its own repro in the same comment block
(`sub f($n){ my $r=0; my @w=(1,); while @w.splice { f($n-1) if $n>0; $r+=10 }; $r }`).

Every call path bought that isolation the same way:

```rust
let saved_loop_local_vars = std::mem::take(&mut self.loop_local_vars);
// ... run the callee ...
self.loop_local_vars = saved_loop_local_vars;
```

Four paths do this (`vm_call_light`, `vm_call_light_typed`, `vm_call_fast`, and
`push_call_frame`/`pop_call_frame`), plus `run()`.

### The cost, measured

ADR-0077's cross-check flagged this cluster and put it out of scope so that
Slice 2 would measure one thing. Re-measured with callgrind on `aaee634`
(`--profile profiling`, JIT on, `fib(22)`, 57 312 calls, 147 698 110 Ir total):

| what, inside `call_compiled_function_positional_light_at` | Ir | share |
| --- | ---: | ---: |
| inlined `core::mem` (the `take`/`replace` header moves) | 4 814 208 | 3.26% |
| inlined `alloc::vec` | 4 699 492 | 3.18% |
| out-of-line `Vec::drop`, **171 936 calls = 3 per call** | 3 266 784 | 2.21% (incl.) |
| inlined `alloc::raw_vec` | 2 063 232 | 1.40% |
| out-of-line `Vec::clone`, **57 312 calls = 1 per call** | 2 177 856 | 1.47% (incl.) |

Not all of the inlined rows is this cluster — the same function also moves an
`Env` and manipulates the operand stack — but the two call counts are
unambiguous. Three `Vec`s are dropped per call, and they are not the slot array
(ADR-0077 retired that one); they are these frame fields, destroyed by the
assignment that puts the caller's vector back.

The `Vec::clone` row is a pure accident of an earlier migration: `vm_call_light`
restored `active_loop_rw_param_names` with `saved.clone()` on all three exit
paths, cloning an always-empty `Vec` once per call, where the other four fields
moved. Nothing needed the clone — see the handle contract below, which is what
proved it.

**What this cluster is *not*.** It is not allocation: `fib(22)` performs 20 708
allocations either way, and that number does not move at all here. A callee's
`mem::take`n vector starts at capacity 0 and only allocates if the callee
actually opens a scope frame, which `fib`'s body does not. The cost is the
header traffic and the out-of-line calls themselves — which is why an
instruction count, not an allocation count, is the oracle for this change.

## Decision

Give the five fields the representation ADR-0077 gave the slot array: one
shared `Vec`, and a per-call base marking where the executing call's frames
begin.

```rust
pub(crate) struct ScopeStack<T> { frames: Vec<T>, base: usize }
```

`push_frame()` moves the base to the current length and returns the caller's
base as a handle; `pop_frame(handle)` truncates back and restores it. Isolation
becomes a pair of integer moves. The buffer is never handed to anyone, never
dropped, and never re-allocated — a callee inherits its caller's spare capacity.

`VmCallFrame`'s five `Vec` fields become five `Option<ScopeFrame>` handles
(40 bytes of `usize` instead of 120 bytes of `Vec` header, and nothing to drop).

### The contract, copied deliberately from `Locals`

- **`Deref` yields `[base ..]`**, so `.iter()`, `.last_mut()`, `.is_empty()` and
  `.len()` speak about the executing call's frames and never reach a suspended
  caller's — exactly what `mem::take` gave for free.
- **`pop()` is bounded by the base.** `mem::take` made "pop one frame too many"
  harmless because the callee's vector was empty; a shared vector would eat the
  caller's frame instead. Two of the five stacks pop unconditionally
  (`self.block_declared_vars.pop().unwrap_or_default()`), so this floor is
  load-bearing, not defensive.
- **A frame handle is not `Copy`, and `pop_frame` consumes it.** ADR-0077 found
  this the hard way: closing twice truncates the *caller's* frames, and
  `vm_call_light` closes on three exclusive exit branches (a type-check failure,
  a panic-unwind arm, the normal tail) where only the move proves they really
  are exclusive. It also removed the vestigial `.clone()` above: the handle is
  moved on all three paths, and the compiler accepted it, which is the proof the
  clone was never needed.

### This closes a latent GC-root gap

`gc_roots` visited `&self.loop_local_saved_env` — the live field only. Under
`mem::take`, a suspended caller's saved loop-local values were sitting in a Rust
local inside `call_compiled_function_positional_light_at`, invisible to the root
scan for the whole duration of the call. They are now in the shared vector and
`gc_roots` visits `all_frames()`, so every live call's frames are rooted. The
window/`all_frames` distinction is the one way this change could have broken
silently in the other direction, and it is pinned by a unit test that asserts
the two disagree.

## What was deliberately not done

- **`frame_authoritative` / `frame_owned` keep their `mem::take`.** They are not
  stacks of frames but whole-set registers that several sites *replace*
  wholesale (`vm.frame_authoritative = frame_authoritative_set(...)`). A base
  index means nothing for a value that is assigned rather than pushed, so they
  need a different treatment, or none.
- **The isolation sets are unchanged.** `vm_call_fast` and `run()` isolate four
  of the five (not `active_loop_rw_param_names`); the two light paths isolate
  all five. Making them uniform would be a behavior change wearing a
  refactor's clothes, so each site opens exactly the frames it opened before.
- **Panic-unwind parity.** The light paths restore these fields with plain
  statements, so a Rust panic through the callee body skips the restore and
  leaves the base where the callee left it. That is exactly what `self.locals`
  already does after ADR-0077 (nothing re-bases it either —
  `recover_call_frames_after_panic` only pops `call_frames`, which the light
  paths do not push), and the observable result is the same as before this
  change: the caller resumes with an empty-looking scope stack. Closing that gap
  is one change for both, and it belongs with `locals`, not here.

## What this cost, in retired instructions

Measured with callgrind on `--profile profiling` builds of the same tree, the
change applied and stashed, outputs verified identical first — the oracle
[#7579](https://github.com/tokuhirom/mutsu/issues/7579)'s method notes prescribe,
and the one ADR-0077 had to fall back on after finding the bench CI cannot
resolve a change of this size on its runner (see ADR-0077, "The bench CI could
not resolve Slice 2").

| program | JIT on | | JIT off | |
| --- | ---: | ---: | ---: | ---: |
| `fib(22)` | 147 698 110 → 138 929 481 | **−5.94%** | 253 401 683 → 244 632 361 | **−3.46%** |
| `tak(14,7,0)` | 1 705 079 510 → 1 642 944 324 | **−3.64%** | 2 727 205 630 → 2 665 070 843 | **−2.28%** |
| a `while` loop (control) | 851 658 050 → 851 662 279 | +0.0005% | 1 346 361 430 → 1 346 360 853 | −0.00004% |

`Vec::drop`, `Vec::clone`, `Vec::truncate` and the inlined `raw_vec` are all
absent from the light call path's profile afterwards. What replaces them is
`scope_stack.rs` inlined into that function at 2 407 104 Ir (1.73%) — ten frame
opens and closes per call for about 42 instructions in total.

**`pop_frame` needs the same emptiness guard `Locals::push_frame` has.** The
first version truncated unconditionally, and `Vec::truncate` promptly appeared
in the profile at 2 292 520 Ir (1.61%) — it is out-of-line, because it drops a
range of `T`. The overwhelmingly common case is a callee that opened no scope
frame at all, so `pop_frame` tests the length first. That one branch is the
difference between −3.65% and −5.94% on `fib(22)`, and it is the whole lesson of
this measurement: replacing a cheap-looking operation with another cheap-looking
operation buys nothing unless the *call* goes away too.

The control is a `while` loop with no sub call in it, which this change cannot
touch: it moved by 4 229 instructions out of 851 million (and by 577 the other
way with the JIT off), which is the check that the deltas above are the call
path and not a code-layout lottery.

## Consequences

- The per-call cost of scope isolation drops from five `Vec` header moves, five
  restores, three `Vec` drops and one `Vec` clone to five pairs of integer
  writes and five length compares — about 42 instructions in total.
- A callee that *does* open scope frames stops allocating and freeing a buffer
  for them per call: the vectors grow once to the deepest nesting the program
  reaches and stay there. (`fib` never allocated here, which is why the
  allocation count is unchanged on it — the win measured above is the header
  traffic and the out-of-line calls, and this consequence is the one the
  benchmark set does not show.)
- The interpreter gains the same invariant `Locals` has: **no cached pointer
  into a scope stack across anything that can open a frame.** Nothing caches one
  today, and `Deref` hands out a slice that borrows `self`, so the borrow checker
  keeps it that way.
- One more field family stops being something another component can be *handed*.
  A future consumer that wants an owned copy must ask for one explicitly.
