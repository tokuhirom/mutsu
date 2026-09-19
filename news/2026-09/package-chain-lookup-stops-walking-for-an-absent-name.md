# A package-chain lookup stops walking for a name no package holds

`lookup_in_package_chain` was the single hottest function in the interpreter
on a `JSON::Fast.from-json` parse — 5.7% of instructions retired, from roughly
648,000 chain walks to parse 100 flat JSON records ([#8830]). #8811 had already
made each walk cheaper by taking the `StrSearcher` setup out of its
`rsplit_once("::")`; it stayed #1 anyway, because the per-call cost was never
the problem. The **call count** was.

## The tables were indexed the wrong way round for the question

`unit_lexicals`, `package_lexicals`, `package_type_aliases`,
`module_scope_lexicals` and `module_imported_lexical_names` are all
`package -> name -> V`. Every query over them runs the same shape:
`lookup_in_running_package` takes up to four candidate packages (the method
class, the frame's lexical package, the frame's package, the current package),
probes each under its unparameterized base name as well, and walks each one up
its `::` chain — so a single failed resolution costs up to eight chain walks,
several hash probes deep each.

The walk is over *packages*. What decides the answer is the *name*. Nothing in
the structure could answer "no package has this name" without visiting them
all.

And on this workload nearly every query was exactly that miss. JSON::Fast
declares `my int` 36 times and `my str` 5 times, and native type names are not
in the class registry, so `package_type_alias`'s `has_type_direct` shortcut
declines and every `int`/`str` type check walked the whole chain looking for a
module import alias that a native type can never have. 140,460 calls to
`package_type_alias` and 70,312 to `module_imported_lexical` produced 648,000
walks that could not have succeeded.

Note what does *not* fix this. `unit_lexicals.is_empty()` and its siblings
already exist and already fire; the calls that remain are the ones where the
table is non-empty and the answer still has to be computed. Another
`is_empty` gate moves nothing.

## The filter

`PackageKeyed<V>` (`src/runtime/package_keyed.rs`) is now a newtype over the
same two-level map, carrying the union of every inner map's keys.
`contains_name` answers the whole query in one hash probe, and it is exact
rather than approximate: `table[pkg][name]` can only be `Some` for a name some
inner map holds, so a miss in the union is a miss at every tier of every
candidate. `lookup_in_package_chain`, `lookup_in_package_chain_mut` and
`lookup_in_running_package` ask it before starting; the last of those asks it
once for all eight walks rather than once per walk.

**Why it cannot drift.** The union is cached in a `OnceLock` that `DerefMut`
drops. The newtype derefs to the raw map, so every existing call site — all
nineteen `cow_table_mut(...).entry().or_default()` / `get_mut` / `extend`
writes, and every read — compiles unchanged, and there is no way to add a name
without going through `DerefMut` and invalidating the cache. The one accessor
that deliberately does not invalidate, `get_value_mut`, cannot change the key
set by construction: it hands out `&mut V` for an entry that already exists.
That one exists because `unit_lexical_slot_mut` is the container-write
chokepoint and runs on every element assignment; invalidating there would make
the read side rebuild the union on the very next free-variable read.

Two searchers on the same paths went with it, both replaced by byte scans that
`src/runtime/utils/str_scan.rs` already had: `candidate.split_once('[')` in
`lookup_in_running_package` (a `CharSearcher`, built up to four times per
lookup) and the `name.contains("::")` guard at the head of
`package_type_alias` / `module_scope_lexical` / `module_imported_lexical` (a
`StrSearcher`, on every type check in the program).

## Measurements

Instruction counts under `callgrind`, 100-record parse, `--profile profiling`
build. These are deterministic per code path, which is what makes them
A/B-able; whole-program totals still move a little run-to-run because
per-process `HashMap` seeds change probe counts.

| | before | after | |
| --- | ---: | ---: | ---: |
| **program total** | 2,975,774,664 | 2,643,873,087 | **-11.2%** |
| `lookup_in_package_chain` | 170,064,462 | **0** | -100% |
| ... walks entered | 647,838 | **0** | |
| `CharSearcher::next_match` | 73,079,481 | 5,056,671 | -93.1% |
| `<&str as Pattern>::is_contained_in` | 29,158,833 | 11,984,742 | -58.9% |
| `__memcmp_avx2_movbe` | 123,308,907 | 100,471,522 | -18.5% |
| `LocalKey::with` | 263,475,108 | 238,161,884 | -9.6% |
| `package_type_alias` (self) | 22,329,116 | 13,126,717 | -41.2% |

The function does not merely get cheaper; it is never entered. `LocalKey::with`
falls out with it because `running_package_candidates` reads the current
package off a thread-local symbol mirror, once per candidate list that is now
never built.

Wall clock on #8673's reproduction (synthetic SPDX-shaped document, 727
records, release builds of the same tree with and without the change, five
runs each, one box): **2.596s -> 2.335s, -10.0%**. `raku` on the same runs is
0.041s, so the ratio moves from ~63x to ~57x. The trend belongs to the bench
CI series rather than to these local runs.

## What this does not fix

The costs this change does not touch are now the top of the profile, unchanged
to the instruction: `Env::get_sym_with_fallback` (139.2M), `exec_one_dispatch`
(107.8M), the allocator (~132M in `_int_free` alone), and
`resolve_lexical_type_key` (60.3M self, 125.5M inclusive for **7,236 calls** —
a `format!("{qualified}\u{0}")` followed by a linear scan over every key in
the class, role, enum and subset registries, once per miss). That last one is
the same disease in a different table and wants the same treatment; it is left
for its own slice.

`#8830`'s larger claim — that a lexical and a type constraint should resolve to
a slot or an id at *compile* time — also still stands. This is the cost of
asking the question at runtime, removed for the cases where the answer is no;
it is not the compile-time answer.

Pinned by the five unit tests in `src/runtime/package_keyed.rs`, which cover
the filter's exactness, its visibility after an insert through `DerefMut`, the
value-only update that deliberately keeps the cache, removal, and cloning. The
behaviour itself is pinned where it already was — `t/modules/` (above all
`cross-module-short-name-types.t`) and the roast suite exercise the resolution
paths this filter fronts, and a wrong filter would fail them loudly.

[#8830]: https://github.com/tokuhirom/mutsu/issues/8830
