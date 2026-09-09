# Type-name string scans taken off the instance-dispatch hot path

[#7696](https://github.com/tokuhirom/mutsu/issues/7696) measured a flat ~7us
constant on every `Buf` method call, sitting in front of the buffer code rather
than in it. Profiling the dispatch path found the constant, but not where the
ticket expected it.

## What the profile actually said

`callgrind` on `my $big = Buf.new(1 xx 1000); for ^2000 { $big.push(1) }`
(143.9M instructions) with caller attribution:

- **52% of the whole program** was `try_native_method_raw` →
  `type_matches_value`. `vm_native_dispatch` probes every instance receiver
  with `type_matches_value("Real", …) || type_matches_value("Numeric", …)`
  before it will decline a method, and each probe walks the receiver class's
  MRO and its composed-role closure. Two walks per `push`, ~40 `type_matches`
  calls between them.
- **~13% was substring searching**, and it belonged to `type_matches`, not to
  the `Buf` predicates. `type_matches` asked `str::contains("::")` three times
  and `contains('[')` once per call; `str::contains(&str)` builds a two-way
  searcher per call, and on names this short that setup *is* the cost. At
  313,719 searches for 80,000 `type_matches` calls it was the single largest
  line item.
- **~15% was malloc/free**, 132,580 allocations for 2000 pushes — 63 per push.
  Almost all of it was `Symbol::resolve()` handing out a fresh `String` where
  `as_str()` would have handed out the interned `&'static str`, plus the
  `Vec<String>` work stacks and `HashSet<String>` seen-sets of the role walks.

The ticket's suggested first step — making `is_buf_like_class` /
`is_blob_like_class` / `is_buf_or_blob_class` answer from the interned symbol
instead of scanning the name — turned out to target the wrong thing: those three
predicates together are **0.15%** of the loop. The `is_contained_in` cost the
ticket attributed to them is `type_matches`'s.

## What changed

All of it is local, and none of it caches a type relation:

- `type_matches` hoists its three `"::"` searches into two byte scans
  (`runtime::utils::has_double_colon`, which already existed from
  [#7554](https://github.com/tokuhirom/mutsu/issues/7554)) and its `[` search
  into `has_bracket` / `split_once_bracket`, new single-byte twins in the same
  module.
- `parse_parametric_type_name`, `parse_generic_constraint`,
  `parse_coercion_type` and `is_known_type_constraint` now let their O(1)
  `ends_with(']')` / `ends_with(')')` reject an unparameterized name *before*
  scanning for the opening bracket, rather than after.
- The `ValueView::Instance` arm of `type_matches_value` resolves its class name
  once, as `as_str()` rather than `resolve()`, for all four of its uses; it had
  been allocating a `String` per use. It also reuses the `Arc<[Symbol]>` MRO it
  already fetched instead of re-entering the registry for a second
  String-keyed lookup.
- `Registry::composed_roles_seed` returns `Vec<Symbol>` instead of
  `Vec<String>`, so the three role walks that consume it keep a `Copy` work
  stack and a `HashSet<Symbol>` seen-set — no allocation per role pushed and
  none per `seen` insert.

## Result

Instruction count on the profiled push loop: **143.9M → 114.7M (−20.3%)**,
deterministic. Wall clock in this container, best of five (a slower box than
the ticket's, so the absolute numbers are higher):

| | before | after |
| --- | --- | --- |
| `Buf.new` + 100 pushes | 1.011 ms | 0.861 ms |
| 10 pushes onto a 1000-byte buf | 0.1182 ms | 0.1014 ms |
| 10 pushes onto a 4000-byte buf | 0.1183 ms | 0.1019 ms |

`type_matches` is on every signature bind, so the win is not `Buf`-specific,
but it is small elsewhere: the standard benchmarks move by less than the noise
floor except `bench-ctor` (−1.7%). The figures above are local A/B runs on one
binary pair, quoted for this change only — the numbers of record stay the bench
CI's `bench-history.tsv`.

## What is left

The 52% item is untouched, and it is the larger half. Declining a native method
for an instance receiver should not need two full type-graph walks when the
receiver's class is already known; the answer is a property of the class, not
of the call. Caching it needs an invalidation signal that covers `classes`,
`roles`, `role_parents`, `class_composed_roles` and `subsets` — `method_generation`
covers none of those — and a hand-enumerated list of ~30 mutation sites is
exactly the "correct only under an incomplete static analysis" shape CLAUDE.md
rules out, since a missed site is a silent wrong type answer rather than a
detectable failure. It is filed with the measurement as
[#7712](https://github.com/tokuhirom/mutsu/issues/7712), rather than guessed at
here.
