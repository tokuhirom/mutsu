# The "native" array `.map` loop was up to 9x slower than the shared loop it replaced

Investigating `todo/perf/uniname-sort-performance-gap.md` — a report that
`(0..0x1FFFF).sort(*.uniname.chars)` was ~18x slower than raku — turned up
something much broader than the reported symptom, and disproved both of the
ticket's stated hypotheses along the way.

## The reported gap was a debug-build artifact

The ticket measured only `target/debug/mutsu` (12.5s vs raku's 0.7s) and
predicted a release build "would likely still be several seconds". It is not:
the release build ran the repro in **0.78s** against raku's 0.71s. Neither of
the ticket's two root-cause candidates held either:

- `.sort(&block)` with a 1-arity block **already** does a Schwartzian
  decorate/sort/undecorate (`runtime/methods_collection_ops/sort.rs`). A
  side-effect counter confirms it calls the key extractor exactly *n* times for
  *n* elements — the same as raku — for bare blocks, `WhateverCode`s and pointy
  blocks alike.
- `.uniname` is an indexed table lookup (`unicode_names2`), not a scan.

## What the profile actually showed

Breaking the repro down exposed a much stranger shape: the *same work* was
several times cheaper in a `for` loop than in `.map`, and cheaper again in
`.grep` than in `.map` — and cheapest of all when the receiver was a `List`
rather than an `Array`:

```
Array.map({ $_.Int })    7.03 us/elem
List.map({ $_.Int })     0.93 us/elem
```

Both produce identical results. The only difference is the code path: a
concrete `Array` was being intercepted by `vm/vm_native_map.rs`'s
`try_native_array_map`, while a `List` fell through to the shared
`eval_map_over_items_rw` loop.

The intercepting loop calls the general closure-call machinery once per element
(`vm_call_on_value` → `call_compiled_closure_with_topic`: a scoped env child,
the full captured-env merge, `authoritative`/`owned` capture installs,
per-instance state lookups and an exit writeback diff). Every bit of that is
loop-invariant. The shared loop compiles the block body once and rebinds only
the param and topic per iteration, re-entering through `run_reuse`.

`docs/vm-decoupling.md` Step 6 introduced that interception in 2026-06 as an
explicitly **"metric-only" decoupling** — its goal was to zero the `map` entry
in the interpreter-fallback counter, and no timing was taken. The counter was
measuring module location rather than tree-walking: the shared loop is not a
tree-walker either, it runs the same compiled bytecode.

## The fix

`try_native_array_map` is now restricted to the one thing it does uniquely:
Raku's rw binding of `$_` to the source element (`@a.map({ $_++ })`,
`@a.map(-> $x is rw { $x++ })`). It captures the block's final `$_` directly
rather than relying on the shared loop's assignment mirror, which is why it — and
only it — also covers prefix `++$_`/`--$_` and `tr///`, and can re-tag the
rebuilt array with the source's element type. Every read-only block goes back to
the shared loop.

Measured over a 131072-element array (release build, us/elem):

| block | before | after | raku |
|---|---|---|---|
| `map({ $_ })` | 2.15 | **0.24** (9.0x) | 0.60 |
| `map({ $_ + 1 })` | 2.18 | **0.26** (8.5x) | 0.56 |
| `map({ $_.Int })` | 6.87 | **0.81** (8.5x) | 0.78 |
| `map({ $_.succ })` | 7.20 | **0.86** (8.4x) | 0.53 |
| `map({ abs($_) })` | 5.46 | **1.12** (4.9x) | 0.61 |
| `map(*.uniname.chars)` over the full Unicode range | 0.862s | **0.230s** (3.7x) | 0.056s |

`@array.map` with a plain block is now faster than raku's, having been ~4x
slower.

## Two bugs the routing change surfaced

Moving read-only array maps onto the shared loop exposed two defects that the
interception had been masking:

1. **`$_` inside a `WhateverCode` was bound to the element instead of the
   caller's topic.** `eval_map_over_items_rw`'s fast loop inserted the element
   as the topic unconditionally, where its `List` sibling
   (`eval_map_over_items`) and the `.grep` loop both route the bind through
   `bind_loop_topic`. So `@a.map(* eq $_)` compared every element against
   itself. Fixed by using `bind_loop_topic` here too — and, correspondingly, by
   *not* writing a caller-topic `$_` back into the source, since it is not an
   alias for the element.

2. **A read-only `.map` destroyed the source array's per-slot metadata.** The
   rw writeback in `methods_mut_dispatch.rs` rebuilt the source with
   `Value::real_array(items)` unconditionally, even when nothing had been
   mutated. That dropped the `ArrayData` `initialized` bitmap, so a `:delete`d
   slot stopped reading as a hole and a later trailing-element `:delete` could
   no longer truncate the array (`roast/S32-array/delete.t`, which calls a
   read-only `@a.map({ $_ // "Any()" })` in between two deletes).
   `eval_map_over_items_rw` now reports whether it wrote any element back; the
   source is left completely alone when it did not, and rebuilt through
   `Value::array_data_like` (which preserves the container metadata) when it
   did.

Verified with the full `t/` suite (3499 files, 34682 tests) and a 379-file
targeted roast sweep of every whitelisted file mentioning `map`/`sort`/`is
rw`/`*.`. Pinned by `t/map-array-shared-loop.t`, which asserts Array/List
parity across every block shape the routing change moved.

## The generalizable lesson

A per-element `call_compiled_closure_with_topic` costs roughly an order of
magnitude more than a compile-once + `run_reuse` loop. A future "run the loop in
the VM" step must move the *loop*, not merely relocate the per-element call —
and must be justified with a timing, not only a counter delta.
