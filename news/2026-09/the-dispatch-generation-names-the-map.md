# The dispatch generation names the functions map instead of counting writes to it

`fn_resolve_gen` was a step counter. Every write to `Registry::functions`
incremented it, and six name-keyed dispatch memos — `fn_resolve_cache`,
`multi_compiled_key_cache`, `multi_candidates_cache`, `declared_fn_cache`,
`multi_fn_cache` and the base-name key index `fn_keys_by_base` — dropped
*wholesale* on the increment.

Counting steps answers "has the map changed since" correctly in one direction
and wrongly in the other. A map that goes away and **comes back** reads as two
different states, so every memo built before the round trip is thrown away even
though the map they describe is back, byte for byte.

That round trip is not an edge case. It is what a routine-local `my sub` does on
every single call. Entering `JSON::Fast`'s `unjsonify-string` installs
`JSON::Fast::fetch-codepoint`; the scope restore on the way out takes the key
away again. Three `from-json` decodes of a document with escaped strings moved
the generation 362 times — for a program that declares its routines once. The
registry ends each excursion in exactly the state it started in, and the six
caches are gone twice per call.

[#8314](https://github.com/tokuhirom/mutsu/issues/8314) split this out of
[#8300](https://github.com/tokuhirom/mutsu/issues/8300), which had narrowed
`fn_keys_by_base` to per-base-name eviction and deliberately left the generation
contract alone: the fix changes an invalidation contract six caches depend on,
and a wrong version stamp is a silent mis-dispatch rather than a test failure.

## The version lives in the map

`Registry::functions` is now an `Arc<FunctionTable>` — the map plus a version
stamp, drawn from a process-global counter and renewed by the single write path
(`Registry::functions_mut`). Reads are unchanged: the table derefs to the map, so
`registry.functions.get(&key)` reads exactly as it did.

Two properties follow *by construction*, with no call site cooperating:

1. **A version names one content state.** The counter never repeats, so a stamp
   is minted for one map and never re-used for another. This is what rules out
   the naive fix — restoring a saved counter on a scope restore — under which two
   different excursions both land on `G+1` and excursion A's memos are served
   during excursion B.
2. **A restore restores the version.** `registry.functions = saved` moves the
   whole `Arc` back, stamp included, so the pre-excursion version returns with
   the pre-excursion content and nothing has to notice that a restore happened.

That covers the *exit* half of the cycle. The *entry* half — the re-install on
the next call — is covered by a transition memo (`FunctionTableTransitions`),
which records the one fact a stamp cannot carry: *installing this key with this
definition, into the map named `V`, yields the map named `W`*. The re-install
uses the very `Arc<FunctionDef>` the previous call installed (`prepared_fn_defs`
caches the derived definition, so the re-install is a refcount bump), so the
write lands in a state the map has already been in, and the stamp now says so
rather than naming a new one.

Reuse is sound by induction rather than by comparing content: a version names one
content, a recorded transition was observed to take that content to the content
named `W`, and the same content under the same write yields the same content. The
base case is that fresh stamps are unique. A debug-only audit re-derives a content
hash for every stamp handed out and panics if one version ever names two different
maps, so a mistake in that reasoning fails CI rather than mis-dispatching.

## Memos are tagged per entry, not per table

With both halves in place the generation alternates between exactly two values
across a routine's calls, which is only useful if an entry can outlive a
generation. `runtime::gen_cache::GenCache` tags each **entry** with the
generation it was computed under instead of stamping the whole table and
`clear()`ing on a mismatch. An answer computed outside the routine sits there,
dormant, while the routine runs, and is live again the moment the map is
restored.

The table stays keyed by `K` alone, so an entry computed under a new generation
replaces rather than accumulates on top of the same key's older answer: the size
is bounded by the number of distinct keys, exactly as it was before, not by the
number of generations. The cost of that choice is that a key probed under two
alternating generations recomputes on each switch.

## What a non-map change still costs

`Interpreter::invalidate_fn_resolution` keeps its old meaning for a change the
functions map cannot express — a routine wrapped or unwrapped, a lexical import
scope popping, a proto marker moving. Such a change leaves no trace in the map,
so version movement cannot retire the affected answers. It therefore does two
things: it empties the tagged memos outright (a later scope restore could
otherwise bring back a version they were tagged with, undoing the announcement
for the caches but not for the interpreter), **and** it gives the map a version
it has never had, so the caches that self-refresh off the generation rather than
being cleared there — `light_call_cache`, `pos_light_call_cache`,
`otf_call_cache`, `func_multi_resolve_cache`, the ADR-0066 callsite inline-cache
epoch — see the change too.

Mirroring the unchanged version instead was the one regression this work
produced, and it is worth recording: a `&wrapped.wrap(...)` became invisible to a
call site that had already run, because nothing about the functions map moved
(`t/routines/call/call-inline-cache.t` test 6). `fn_resolve_gen ==
functions_version()` is now a whole-program invariant, which is also what makes
the mirror easy to reason about — there is exactly one name for "which map is
installed".

## Measurement

30 `from-json` decodes of a document with escaped strings, release build. These
counters are deterministic and load-independent.

| | before | after |
|---|---|---|
| generation moves | 2,468 | **1,647** |
| base-name index lookups | 45,798 | **12,462** |
| tagged-memo probes / hits / **stale** | — | 21,302 / 18,688 / **0** |
| install transitions reused / minted | — | 778 / **1** |

`stale 0` is the result: across the whole run, not one memo was discarded for
naming a different map. The generation still *moves* twice per excursion — that
is the map genuinely alternating between two states — but both states now have
stable names, so the moves no longer cost anything.

Two new `MUTSU_VM_STATS=1` lines report this directly:
`gen-tagged-dispatch-memos: probes= hits= stale=` and
`fn-table-transitions: reused= minted=`. A `stale` count that grows with the
number of *calls* rather than with the number of *declarations* is the shape this
work removed.

Wall clock is left to the bench CI, per the repository's rule that recorded
figures come from `bench-history.tsv` rather than from a local run;
`benchmarks/bench-json-fast.raku` records the META6 shape on every push to main.

## What is left

The three hottest call caches — `light_call_cache`, `pos_light_call_cache` and
`otf_call_cache` — are still stamped per table rather than per entry, so they are
still emptied on each of the two transitions a routine's call makes. Converting
them is the same mechanical change, but it interacts with ADR-0066's callsite
inline cache, whose epoch is bumped from the wholesale clear those caches
currently perform; that coupling wants its own slice rather than a rider on this
one.
