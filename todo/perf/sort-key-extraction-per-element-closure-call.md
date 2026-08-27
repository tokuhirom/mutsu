# `.sort(&key-extractor)` computes each key through the full per-element closure-call machinery

(Was `uniname-sort-performance-gap.md`. That ticket reported
`(0..0x1FFFF).sort(*.uniname.chars)` as ~18x slower than raku and hypothesized
either a missing Schwartzian transform or a linear `.uniname` lookup. **Both
hypotheses were disproved** and the headline number was a debug-build artifact —
see `news/2026-08/native-array-map-loop-was-9x-slower-than-the-shared-loop.md`.
This is the residue that survived that investigation.)

## The finding

`sort_items_generic`'s arity-1 (key-extractor) branch is a correct Schwartzian
transform — it calls the block exactly once per element — but it invokes it
through `SortCaller::call_callable`, i.e. `vm_call_on_value` →
`call_compiled_closure_with_topic`, once per element. That is the same
per-element cost that made the native array `.map` loop 4-9x slower than the
shared compile-once/`run_reuse` loop.

```rust
// runtime/methods_collection_ops/sort.rs, the `Some(c)` (arity <= 1) branch
let keys: Vec<Value> = items
    .iter()
    .map(|item| caller.call_callable(&c, vec![item.clone()]))
    .collect();
schwartzian_by_keys(items, &keys);
```

The sibling `{ .method }` form already avoids it (`detect_simple_mapper_block`
→ `caller.call_method`), which is why `sort(*.Int)` measures 0.84 us/elem while
`sort(*.uniname.chars)` — whose key is not a single 0-arg method — pays the full
closure call.

Key extraction here is *exactly* a `.map` over the items with the same callable,
so it should reuse the same compile-once loop that `.map` now uses.

## Measured (release, 131072 elements)

| | mutsu | raku |
|---|---|---|
| `@cps.sort(*.uniname.chars)` | 0.905s | 0.396s |
| `@cps.map(*.uniname.chars)` (same 131072 key computations) | 0.230s | 0.056s |
| `@cps.sort` (no key) | 0.002s | 0.121s |

The sort itself is free; ~0.9s of the 0.905s is key extraction, against 0.23s
for the identical work done through `.map`. So roughly **0.67s (74%) is pure
call-machinery overhead**, and closing it would put `.sort` with a key extractor
at ~0.24s — comfortably under raku's 0.396s.

## Sketch

Add a `SortCaller` method that extracts all keys in one batch, e.g.

```rust
fn map_keys(&mut self, callable: &Value, items: &[Value]) -> Option<Vec<Value>>;
```

implemented by both `InterpCaller` and `VmSortCaller` (they both wrap
`&mut Interpreter`) by delegating to `eval_map_over_items`. Fall back to the
existing per-element loop — by returning `None` — whenever the batch form would
change semantics:

- the returned key count differs from `items.len()` (a key extractor returning a
  `Slip` would be flattened by the map loop),
- any element is a `Pair`/`ValuePair` (`call_callable` runs them through
  `pair_as_positional`; the map loop topicalizes them instead),
- the map loop returns `Err` (today each key error is swallowed to `Nil`
  per element; one batched error must not abort the whole sort).

## Affected files

- `src/runtime/methods_collection_ops/sort.rs` — `SortCaller`, `InterpCaller`,
  `sort_items_generic` / `sort_indices_generic`.
- `src/vm/vm_native_sort.rs` — `VmSortCaller`.

## Repro

```raku
my @cps = (0..0x1FFFF).List;
my $t = now; my @s = @cps.sort(*.uniname.chars); say +(now - $t);
my $u = now; my @k = @cps.map(*.uniname.chars).List; say +(now - $u);
```

## Separate, smaller residue found in the same measurement

`.uniname` itself costs 1.9 us/call in mutsu against raku's 0.25 us — a 5x gap
that is *not* the call machinery (it shows up in a plain `for` loop too). It is
small in absolute terms and independent of the above; profile
`builtins/unicode.rs::unicode_char_name_by_codepoint` (which allocates a fresh
`String` per call) before assuming a cause.
