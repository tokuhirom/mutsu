# Frame slots share one stack

ADR-0077 Slice 2. Until now each call got its own `Vec<Value>` of slots,
borrowed from `locals_pool`: pop a vector, `clear` it, `resize` it with
`Value::NIL`, `mem::take` the caller's aside, run the body, push the used one
back. [#7562](https://github.com/tokuhirom/mutsu/issues/7562) put that at ~5.7%
of `bench-fib`'s profile, and a callgrind cross-check on the current head found
the two surviving halves at 2.03% (`Vec::resize`) and 1.95% (`recycle_locals`)
of retired instructions, once per call each — to hold one value, since `fib`'s
only local is its parameter.

Every live frame's slots now live in one contiguous vector, and the executing
frame is the window at the top of it:

```rust
pub(crate) struct Locals { slots: Vec<Value>, base: usize }
```

A call extends the vector, a return truncates it. `locals_pool`,
`take_locals_from_pool`, `recycle_locals` and every `mem::take(&mut
self.locals)` are gone, and `VmCallFrame` carries a frame handle instead of a
whole vector.

Keeping the stack *inside* `Locals` rather than as a second `Interpreter` field
is what made this a local change: there is still one `locals` field, `Index`
became `slots[base + slot]`, `Deref` became `slots[base..]`, and nothing has to
borrow two interpreter fields at once. Slice 0's newtype then paid off exactly
as designed — the ~330 `self.locals[i]` sites and the ~100 that read through
`Deref` did not move again. The ~40 structural sites reduced to three shapes:
open/close a frame, replace the executing frame's slots, or copy a frame out
and back for a snapshot that outlives it (a suspended `gather` coroutine, an
inline `CATCH` handler frame, a hyper/race worker's seed).

## The handle is not `Copy`, and that was learned the hard way

The first version of this returned a bare `usize` from `push_frame` and
documented `pop_frame` as idempotent. Its own unit test failed: closing a frame
twice truncates the **caller's** slots, because the second call truncates to a
base that is already the caller's.

The old code had been protected from that by accident — it moved a
`Vec<Value>`, so a double restore did not compile — and several paths, such as
`vm_arith_int_ops`, close a frame on more than one exclusive exit branch, where
that move was the only proof the branches really were exclusive. Replacing the
vector with a plain integer silently discarded the proof. So `push_frame`
returns a non-`Copy` `CallerFrame` that `pop_frame` consumes, restoring exactly
the guarantee, and `#[must_use]` immediately found the hyper/race worker
pushing a root frame it never closes — legitimate, since a worker VM is
discarded whole, and now stated as `install_root_frame` rather than left as a
warning.

## The GC hazard

`gc_roots` used to visit `&self.locals` plus each call frame's `saved_locals`
separately. `Deref` now yields only the executing frame, so a root scan reading
through it would miss every suspended caller's slots and free live values while
the program kept running — the one way this slice could have broken silently. It
visits `all_slots()` instead, which also makes the scan a single slice visit
rather than one per frame. `t/locals-frame-stack-gc-roots.t` pins the
end-to-end property: values, cyclic objects and containers held only by frames
suspended under a deep call survive, including past the retired pool's 64-entry
bound.

That bound is worth a note of its own. `LOCALS_POOL_MAX` was 64, so a recursion
deeper than that missed the pool *structurally* — the descent always found it
empty — and paid a malloc and a free per call. No benchmark measured it
(`fib`'s depth is 30), and it is simply gone now: the stack grows once.

## The JIT

Slice 0 left a `const { assert!(size_of::<Locals>() == size_of::<Vec<Value>>()) }`
next to `vm_jit_layout`'s existing probe assertion, precisely so this slice
could not land without dealing with Tier B. It fired, as intended. The GetLocal
emitter now loads `locals_base` alongside the `Vec` header words on every slot
access — a push can reallocate the stack and every call moves the base, so
neither may be cached — reads the absolute element `base + idx`, and
bounds-checks against `len - base` behind a `base <= len` guard. The
call- and loop-shaped benchmarks agree under `MUTSU_JIT=on` and `off`.

## What is deliberately not in it

The strongest form of the fix — `locals_base = args_base`, where a callee whose
locals are exactly its leading parameters pays *nothing*, because the argument
the caller pushed already sits in the cell the slot wants — needs locals to live
on the *operand* stack. ADR-0077 framed the choice between one stack and two as
a measurement; it is really a consequence of the `Deref` contract. "The
executing frame is everything above `base`" stops being true the moment an
operand push can land inside the window, so a fused stack needs a per-frame
length as well as a base, and every `.len()` / `.iter()` / `&self.locals` site
starts meaning something subtly different. That is a second, larger change, and
it gets its own slice and its own measurement rather than riding along here.

Two detached copies also remain on purpose. `vm_call_method_compiled` reads the
enclosing frame's slots and writes a block's free-variable writes back into
them before restoring the whole array, so the copy is the semantics rather than
an artifact of the old representation — writing through to the live region below
`base` changes behavior on the panic path, where the restore never runs. And the
shared-container propagation in `vm_var_assign_coerce`, which walked
`call_frames` writing `frame.saved_locals[i]`, now derives each saved frame's
`[base, end)` region (walking downwards, a frame's region ends where its own
base begins, and the topmost ends at the executing base), collects the absolute
indices during the walk, and applies them afterwards.
