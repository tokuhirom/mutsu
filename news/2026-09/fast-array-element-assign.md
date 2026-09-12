# `@a[$i] = $v` stops re-deriving the array's declaration on every store

`bench-threads` is the only row in `bench-history.tsv` where mutsu is slower than rakudo, and the
root cause turned out to be mostly not about threads at all
([#8069](https://github.com/tokuhirom/mutsu/issues/8069)): an ordinary indexed element store —
one thread, no `start` anywhere in the program — cost **13,069 instructions, 20 `Symbol::intern`
calls, 6 heap allocations and 1940 ns**, against roughly 40 ns of marginal cost in rakudo.

The Associative half of that statement had had a fast path for a long time
(`try_fast_hash_element_assign`). The Positional half had none, so **every** `@a[$i] = $v` went
through `exec_index_assign_expr_named_op_inner`, which re-derives the target's whole *declaration*,
by string key, on each write: is this a sigilless alias, is it shaped, is it `:=`-bound, does it
have a type or key constraint, a default, a readonly flag, a bound index, a `Proxy` element, a lazy
tail. Each question is a fresh `String`, a fresh intern and a hash probe of the frame `Env` — and
not one of them is a property of *this store*. They are all properties of *that container*, settled
when the binding was declared.

No benchmark in `benchmarks/` exercised indexed assignment, which is why a ~46x gap on one of the
most common statements in the language had stayed invisible: `bench-array` measures `push` / `map` /
`grep` / `sort`, `bench-hash` measures whole-hash construction and lookup, and neither writes
through a subscript in a loop. `benchmarks/bench-index-store.raku` (#8086) closed that hole just
before this landed, so the store now has a bench-CI row to be measured against.

## What landed

`try_fast_array_element_assign` (`src/vm/vm_var_assign_element_fast.rs`) is the Positional twin of
the hash lane. It answers the declaration questions the way the rest of the VM already answers its
own hot probes — from the container's *embedded* metadata (`ArrayData::has_type_meta`, `ArrayKind`,
the element slot's own shape) and from the monotonic `env::*_possible()` latches, which are false
for any program that never declared the feature. A plain array store therefore asks no name-keyed
env question at all and reaches a `Vec` slot write.

It fires for a positional subscript on an `@`-sigiled name with a plain non-negative in-range `Int`
index, a plain rvalue, and a plain mutable untyped `Array`/`ItemArray` target whose destination slot
is not itself a container. Everything else — autovivification past the end, typed and native arrays,
shaped arrays, `:=`-bound and `Proxy` elements, `Nil` rvalues, `is default(...)`, slices, `Range`
and `Whatever` subscripts, `List` targets, `:delete`d indices — makes it return `None`, and the full
store runs unchanged. Like its hash twin the lane never *errors*: anything it is not certain about
falls through to the path that owns the rule.

Two decline routes are worth naming, because reading env directly is exactly what makes them
necessary and the suites caught both:

- **A module routine's own `our @arr`.** `env_root_descended_mut_tracked`, the write chokepoint the
  full store funnels through, resolves a name in a strict precedence order — a captured unit lexical,
  then the running routine's package `our` mirror, then env — because the bare env key belongs to
  whatever scope *loaded* the module. Reading env without those two probes made the module's
  `@arr[0] = $v` write the loading script's same-named array
  (`t/modules/our-container-bare-name-resolution.t`). Both probes open with their own emptiness
  gate, so a program with no unit lexicals and no `our` variables pays two `is_empty` checks.
- **Any program that has spawned a second VM mutator thread.** There an element store is routed by
  the name-keyed cross-thread lanes, or excluded by ADR-0068's `ContainerStructGuard` — five
  documented routes whose interaction this lane reproduces none of, and an unguarded
  `gc_contents_mut` under twenty concurrent writers is a `double free or corruption (out)`, which is
  what `t/concurrency/concurrent-lane-decline-routes.t` produced. Taking the guard alone would buy
  exclusion but not the lane's accumulate-instead-of-snapshot semantics, so the lane stands down
  entirely once `multi_mutator_threads_live()` is set: one relaxed atomic load, the same latch the
  guard itself consults.

Two mechanical pieces came with it:

- A `SHAPED_ARRAY_DIMS_SEEN` latch for `__mutsu_shaped_array_dims::*`, joining the six latches
  already in `src/env.rs`. That probe ran on every element store and on every multi-dim assignment,
  costing a `format!` plus an interning env lookup for a key a program without a single `my @a[2;3]`
  can never hold. There is exactly one insert site for the key and it goes through the String-keyed
  `Env::insert`, so `note_env_key` catches it and the usual monotonic-over-set argument applies.
- Container identity (§3) is preserved: the store writes **through** the backing node rather than
  copy-on-writing it, so a `:=` alias, a `\(@a)` capture and an array passed to a sub all observe
  the write; Raku's `=` copy semantics stay enforced at copy time by `detach_shared_container`.

## Measured

All figures are before/after on the SAME base (`a8298ea0`), not against the issue's original
snapshot -- upstream's `MetaNs` key memoization had already trimmed the before side since #8069 was
filed, so its 14,053 / 22 / 8.1 are no longer the number to compare against.

| per in-range store | before | after |
| --- | ---: | ---: |
| wall clock | 1940 ns | **351 ns** |
| instructions | 13,069 | **2,231** |
| `Symbol::intern` calls | 20.0 | **0** |
| heap allocations | 6.0 | **1.0** |

Wall clock is the best of five in-process rounds differencing the same loop with and without the
store (release build, 4 cores): the 400,000-store loop itself goes from 1054 ms to 425 ms.
Instruction and intern counts are callgrind differences on a `--profile profiling` build; allocation
counts come from the `alloc-stats` feature's per-opcode scope (`op:IndexAssignExprNamed`). The last
three are deterministic and load-independent. The one remaining allocation is ~2 bytes and sits in
the opcode's shared preamble, not in the store itself.

A note on the *marginal* figure, since it drifts for a reason that has nothing to do with this
change: it is a subtraction, and `2fe3a800` ("decide a plain scalar store's flavour once instead of
2,000 lines") made the subtrahend -- the identical loop with `$s = $i +& 15` in place of the store --
drop from ~270 ms to ~185 ms. On that later base the same store measures ~600 ns marginal while its
absolute loop time is unchanged at 425 ms. The loop times and the deterministic counters are the
figures that mean the same thing from one base to the next.

End to end, `benchmarks/bench-index-store.raku` goes from ~1.70 s to ~0.95 s, against rakudo's
~0.73 s on the same container -- a ratio of 2.3 down to 1.3. (Re-checked at 0.95 s on `30491c43`.)

## What this does not do

**It does not move `bench-threads`.** That row is a concurrent program, and the lane declines for
the whole process once a thread is spawned (above), so the benchmark that motivated the
investigation is untouched; §3 and §4.2-4.4 are what address it. What this change buys is the
single-threaded half — every program that writes an array element, which is most of them.

Beyond that, this is #8069 §2 only, and only for the Positional single-index shape. The issue's §4.1 asks for a
**resolved container descriptor** addressed by slot, so that the descriptor is the *only* path
rather than a fast path bolted beside a name-keyed one — and it asks for its own ADR, because the
invariant it establishes ("no store-time probe may be keyed by a variable name") has to be enforced
or the probes grow back one feature at a time. That remains open, as do §4.2 (collapsing the
name-keyed cross-thread lane into the container), §4.3 (lock-free container reads), §4.4 (biased
reference counting) and §4.5 (letting the JIT see the store). The shared-container scaling numbers
in §3 are untouched by this change: it is the single-threaded half of the row.

Pinned by `t/collections/fast-array-element-assign.t`, whose 34 assertions all pass under rakudo as well —
every construct in it either goes through the fast lane and must still be right, or makes it decline
and must still reach the path that handles it.
