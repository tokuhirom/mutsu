# The frame's slot array gets a type of its own

ADR-0077 Slice 0. `Interpreter::locals` was a bare `Vec<Value>` handed out by
`locals_pool`, referenced at 464 sites across 62 files. The ADR wants to replace
that representation with a window into one contiguous locals stack
(`{ stack, base }`), and the first job is to give the representation a single
home so the swap does not have to touch every slot access.

The ADR's own plan for this slice was an accessor layer — `local(i)`,
`set_local(i, v)`, `locals_slice()` — applied mechanically to the 330
`self.locals[i]` sites. That was the wrong shape: it is a large churn commit in
service of a boundary a newtype provides for free. What shipped instead is
`src/runtime/locals.rs`:

```rust
#[repr(transparent)]
pub(crate) struct Locals(Vec<Value>);
```

with `Index`/`IndexMut`, `Deref`/`DerefMut` to `[Value]`, and a hand-written
`Clone` whose `clone_from` keeps `Vec`'s buffer reuse (the derived one would
inherit the allocating default, and the hyper/race worker seeding reuses the
vector on purpose). `Index` is written out rather than left to `Deref` so that
Slice 2 can address `stack[base + i]` with one bounds check instead of slicing
the window and then indexing it.

So `self.locals[i]`, `.len()`, `.get()`, `.iter()` and every
`&self.locals` → `&[Value]` coercion keep working verbatim — 18 files changed,
+94/−39, against the ~330 edits the accessor plan implied. What *did* have to
change is exactly the structural set, and each one now has a name:
`nils(n)` for the inline-exec sites that install a fresh un-pooled frame,
`resize_slots(n)` for the top-level `run` entry that sizes a frame and then
seeds it from `env`, `refill(n)`/`release()` as the pool's whole API, and
`from_vec`/`to_vec` for the owned snapshots that outlive a frame — a suspended
`gather` coroutine, an inline `CATCH` handler frame, a hyper/race worker's slots
crossing a thread boundary.

Two things came out of the slice that the plan had not anticipated.

**The args-scratch pool had to be separated first.** Three sites in
`vm_call_func_ops.rs` called `take_locals_from_pool(0)` and `extend`ed it —
borrowing the *locals* pool as an argument buffer for the named and spec light
call paths. A window into a shared stack cannot be handed out as an owned
buffer, so that conflation is a hard blocker for Slice 2, not a cosmetic one
(it is ADR-0077's open question 3). Those sites now take from their own
`args_scratch_pool: Vec<Vec<Value>>`, with the same bound and the same
clear-before-return discipline. Behavior is unchanged; the cost is up to 64 more
retained buffers.

**The JIT coupling now trips at compile time.** `vm_jit_layout` probes
`Vec<Value>`'s word layout once and applies those offsets at
`offset_of!(Interpreter, locals)`, which Tier B's GetLocal fast path reads on
every slot access. That is sound only while `Locals` is `#[repr(transparent)]`
over the vector, so a `const { assert!(size_of::<Locals>() ==
size_of::<Vec<Value>>()) }` now sits next to the existing probe assertion. When
Slice 2 gives `Locals` a base index, the build fails until
`vm_jit_tier_b`'s emitter learns about it — rather than silently emitting native
code against the wrong words.

The module doc records the two contracts Slice 2 has to preserve, since code all
over the VM already depends on them: `Index` addresses slot `i` of the *current*
frame, so it must add the base; and `Deref` yields exactly the current frame's
slots, so `.len()`, `.get()` and `.iter()` speak about this frame and nothing
below it. The second is what turns ADR-0077's open question 1 — one stack or two
— into a real question: a locals region sharing the operand stack would need a
per-frame length there, not "everything above `base`".

No behavior change and no measurable perf change: every accessor is `#[inline]`
over the same `Vec`, and the representation is byte-identical.
