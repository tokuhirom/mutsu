# `.sort(&key-extractor)` extracts its keys in one batch, and is now faster than raku

The last piece of the investigation that started at
`todo/perf/uniname-sort-performance-gap.md`. That ticket reported
`(0..0x1FFFF).sort(*.uniname.chars)` as ~18x slower than raku and blamed either
a missing Schwartzian transform or a linear `.uniname` lookup. Both hypotheses
were wrong (see
`news/2026-08/native-array-map-loop-was-9x-slower-than-the-shared-loop.md`), and
the headline number was a debug-build artifact — but a real gap did survive, and
this closes it.

## The finding

`sort_items_generic`'s arity-1 branch *is* a correct Schwartzian transform: a
side-effect counter confirms it calls the key extractor exactly *n* times for
*n* elements, same as raku. But it invoked it through
`SortCaller::call_callable` — `vm_call_on_value` →
`call_compiled_closure_with_topic` — once per element. That is the same
per-element cost that made the native array `.map` loop 4-9x slower than the
shared compile-once loop: a scoped env child, the full captured-env merge,
per-instance state lookups and an exit writeback diff, all of it loop-invariant.

The sibling `{ .method }` form already sidestepped it via
`detect_simple_mapper_block` → `call_method`, which is why `sort(*.Int)`
measured 0.83 us/elem while `sort(*.uniname.chars)` — whose key is not a single
0-arg method — paid the full closure call. Of that repro's 0.905s, the sort
itself was 0.002s; essentially all of it was key extraction.

## The fix

Key extraction with a 1-arity callable is *exactly* a `.map` over the items with
that callable, so it now runs as one. `SortCaller` gained a `map_keys` batch
method, implemented by both `InterpCaller` and `VmSortCaller` (they both wrap
`&mut Interpreter`) through a shared `sort_keys_batched`, which delegates to
`eval_map_over_items` — the loop that compiles the block body once and rebinds
only the param/topic per iteration.

It returns `None`, meaning "use the per-element loop", whenever the batch form
would not faithfully stand in for it:

- a non-`Sub` callable, which `eval_map_over_items` treats as a smartmatch
  pattern rather than a mapper;
- a `Pair`/`ValuePair` element, which `call_callable` feeds through
  `pair_as_positional` while the map loop topicalizes it instead (so hash and
  Set/Bag/Mix sorts keep the old route);
- a result that is not a concrete list of exactly `items.len()` values — a key
  extractor returning a `Slip` would otherwise be flattened into the key vector;
- an error, since the per-element loop turns a failing key into `Nil` rather
  than aborting the whole sort.

## Measured (release, 131072 elements)

| | before | after | raku |
|---|---|---|---|
| `@cps.sort(*.uniname.chars)` | 0.905s | **0.239s** (3.8x) | 0.417s |

`.sort` with a key extractor is now **1.7x faster than raku**, having been 2.3x
slower. Together with the `.map` fix, the original ticket's repro
(`(0..0x1FFFF).sort(*.uniname.chars)[*-1].chr.uniname`) went from a 10s-timeout
classification to comfortably beating the reference implementation.

## Verification

`t/sort-key-extractor-batch.t` (23 tests, validated against `raku` first) pins
both routes against each other and against raku: every callable shape
(WhateverCode / bare block / pointy / placeholder / bare-method), the
exactly-*n*-calls property, stability across equal keys, `Pair` and `Set`
elements, a `Slip`-returning extractor, the outer-topic rule for a `$_`-reading
WhateverCode, `:k`, negative keys, empty/single/all-equal sources, and that
two-arg comparators are untouched. The `t/` suite (3500 files) and a 151-file
whitelisted roast sweep of every file mentioning `sort`/`Whatever` pass.
