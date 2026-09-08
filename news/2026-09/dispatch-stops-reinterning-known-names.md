# Method dispatch stops re-interning names it already holds

`bench-ctor`'s round 7, second slice (issue #7568). Round 6 closed with three
named residuals, each a site that re-derives a `Symbol` from a name its caller
already had: `native_lever_a_user_override`, the routine-frame push's
`lexical_package`, and the name-keyed env reads in
`get_env_with_main_alias_inner`. A fresh callgrind profile confirmed the
ranking — those three were the top three `Symbol::intern` callers by call count,
at 20, 18 and 16 interns per constructed object. This closes all three.

A `Symbol::intern` is not free: a thread-local round trip, a string hash, and a
`memcmp` against the interned copy. It never shows up as a hot symbol of its own
(round 6's lesson), so it has to be counted, not looked for.

## What changed

**The lever-A augmented-native gate no longer interns at all.**
`native_lever_a_user_override` guards every native method call ("has anyone
`augment`ed `Array` with their own `sort`?"). It is memoized, but it interned
*both* halves of the memo key on every call just to ask. Two changes:

- The memo's type half is now keyed by the **address** of the type name rather
  than by a `Symbol` for it. `value_type_name` returns a `&'static str` — a
  string literal, or `RakuAstClass::printed_name`'s — so the same address always
  denotes the same text, which is all a cache key must guarantee. The converse
  does not hold, so two equal `&'static str`s at different addresses would take
  two entries; both then answer identically, which costs a slot and not
  correctness.
- A new `native_lever_a_user_override_sym` takes the method name already
  interned. Both compiled dispatch entries (`try_compiled_method_or_interpret_inner`
  and its mut twin) hold a `method_sym`, and the two literal call sites
  (`.map` in the map/grep reification, `.gist` in the `note`/`say` path) now use
  well-known symbols.

**`MethodDef::lexical_package` is a `Symbol`, not a `String`.** Every method
dispatch pushes it into the `RoutineFrame` — and `push_method_routine_with_location`
takes `Symbol`s, so the `String` field was re-interned per call. It is pure
declaration data, so the intern moved to registration, where the name is read
once. `sub_data.package` / `proto.package` were already `Symbol`s and now pass
straight through; the `"GLOBAL"` literals became `wk::global_package()`.

**`self` is read through a well-known symbol.** `self` is the most-read name on
the dispatch path — a mutating method's receiver, every `$!attr` cell read, the
"used where no `self` is available" diagnostics — and all 19 call sites read it
by string through `get_env_with_main_alias`, which interned `"self"` each time
for the `env` probe. A new `get_env_self()` passes `wk::self_()` down to the
probe. The by-name entry point is unchanged for every other caller: the shared
body takes an `Option<Symbol>` and interns only when it is `None`.

## Measured

`benchmarks/bench-ctor.raku`, 5000 constructions, release, callgrind
(deterministic instruction counts; this container has no `perf`). Both binaries
are clean full rebuilds of the same base:

**1,371,615,890 -> 1,352,401,080 instructions, −1.40%.**

`Symbol::intern` calls over the run: **501,103 -> 386,108, −22.9%** — from 100
name re-derivations per constructed object to 77. Per site:

| intern site | before | after |
|---|---:|---:|
| `native_lever_a_user_override`    | 110,000 | 0 |
| `get_env_with_main_alias_inner`   |  45,000 | 10,000 |
| `call_compiled_method_fast` (the frame push's `lexical_package`) | 90,000 | 80,000 |

(Measured after rebasing onto a `main` that had meanwhile grown its own
`get_env_with_main_alias_sym` — a *required*-`Symbol` entry point for opcode
callers holding a `const_sym`. The two changes are complementary, not
overlapping: `get_env_self` is now one line on top of that entry point rather
than a second `Option<Symbol>` body, and the absolute intern reduction here is
unchanged at ~115,000.)

**Wall clock did not move**: an interleaved same-session A/B (`taskset -c 2`,
best of 9) reads 0.423s before and 0.424s after — indistinguishable. That is
consistent rather than contradictory: what went away is thread-local hits and
small hash probes, which retire cheaply and predict well, so ~5k fewer
instructions per construction hides inside the memory stalls that dominate this
bench. The instruction count is the honest measure of this change; treat it as
paying down a per-dispatch tax whose wall-clock share grows as the stalls above
it are removed, not as a speedup you can quote. Wall-clock authority is the
bench CI (`bench-history.tsv` on `bench-data`) either way.

## Still open on this axis

`call_compiled_method_fast` is now the largest remaining intern caller (16 per
construction): the frame push's `method_name`, the `owner_class` re-intern, and
an attribute-name intern per attributive parameter bind. Threading a `method_sym`
through `call_compiled_method{,_fast}` reaches all of them, but it changes both
signatures at ten call sites, several of which hold only a `&str` — so it is its
own slice, not a rider on this one.
