# The locals frame gets an ADR, and three of its four blockers turn out not to exist

[#7562](https://github.com/tokuhirom/mutsu/issues/7562) recorded that ~5.7% of
`bench-fib`'s profile goes to managing the per-call `locals` array — a
`Vec<Value>` borrowed from `locals_pool`, `clear`ed, `resize`d with
`Value::NIL`, swapped in with `mem::take`, and pushed back on return. For `fib`,
whose only local is the parameter `$n`, that is six `Vec` operations per call to
hold one value. The issue asked for a `Proposed` ADR before any code, because
`self.locals` is touched at hundreds of sites and the JIT knows the field's
offset.

That ADR is now written:
[ADR-0077](../../docs/adr/0077-locals-are-a-window-into-a-contiguous-stack.md).
The decision is the representation every register VM uses — one contiguous
`locals_stack: Vec<Value>` addressed by a per-frame `locals_base`, with the
frame released by `truncate` — and the ADR carries the migration order, the
measurement protocol, and the open questions Slice 2 has to answer.

The more useful outcome is what the audit found. #7562 gave four reasons the
change was large; **three of them do not survive reading the code**:

- **Nothing hands the locals vector to another owner.** The issue expected
  `mem::take(&mut self.locals)` to be load-bearing in the closure and thread
  capture paths. All 47 ownership sites are instead the same shape: stash the
  current array, install a fresh (usually empty) one, run something inline,
  restore. A spawned interpreter is built with `locals: Vec::new()` and
  inherits nothing. Under a shared stack those sites get *simpler* — "install a
  fresh locals array" is "push a new base". Only
  `vm_hyper_race_parallel.rs`'s snapshot-diff needs a real copy, and it is cold.
- **The JIT is not a blocker.** Tier B reloads both the `Vec`'s data pointer and
  its length out of the interpreter on *every* slot access
  (`vm_jit_tier_b.rs:557`), so reallocation is already safe for it. The edit is
  one extra load plus `+ base * 8`, on a path that already does two loads, a
  bounds check and a NaN-box kind probe.
- **The per-frame GC visit gets cheaper, not harder.** `gc_roots.rs` walks
  `call_frames` and visits each frame's `saved_locals` separately today; one
  contiguous stack is a single `visit_slice`.

What does remain hard is borrow structure: sites that want a caller slot and a
callee slot at once hold two owned `Vec`s today and will hold two disjoint
regions of one. Hence Slice 0 of the migration is a pure accessor refactor
(`local(i)` / `set_local(i, v)` / `locals_slice()`) across the 331 plain-index
sites, which turns those sites into compile errors instead of subtle aliasing
and shrinks the representation swap from 507 sites to about 50.

The ADR also records the strongest form of the fix, which the already-shipped
*args* half of the same cluster
(`news/2026-09/positional-light-call-binds-from-the-stack.md`) makes reachable:
parameter slots come from the compiler's `local_map` and an ordinary sub
declares its parameters first, so `param_local_slots` is normally the identity
prefix. When it is, the argument the caller pushed already sits in the cell the
local slot wants — `locals_base = args_base`, and a callee whose locals are
exactly its parameters pays *nothing* for its locals frame.

The cluster was re-measured before the ADR was written, since the 5.7% figure
predates the September call-path sweep. `perf` was unavailable in the container
used, so the cross-check is callgrind instruction counts (exact and
load-independent — right for "is this still worth attacking", wrong for a
wall-clock claim, and not a substitute for the bench-CI number Slice 2 owes).
On `fib(22)` with the JIT on, `take_locals_from_pool`'s `resize` is 2.03% and
`recycle_locals` 1.95% of retired instructions, each once per call: **~4.0%**,
still there.

Two things the re-measurement corrected or added. #7562's
`<Vec<T,A> as Drop>::drop` row is *not* the locals vector — that one is
recycled, not dropped; `Vec::drop` runs three times per call on the *other*
`mem::take`n frame fields, which this ADR deliberately leaves alone so Slice 2
measures one thing. And the pool misses about a third of the time even in
`fib`'s tree recursion (20 871 allocations for 57 312 calls); a linear recursion
deeper than `LOCALS_POOL_MAX = 64` misses it *structurally*, paying a malloc and
a free per call, which no current benchmark measures.

No behavior change; the ADR is `Proposed` and #7562 stays open for the
implementation slices.
