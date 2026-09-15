# The base-name dispatch index is evicted per name, not thrown away

`fn_keys_by_base` is the index every name-keyed candidate gather runs on: registry
function keys grouped by the base name they reduce to. It was guarded by
`fn_resolve_gen` like the five other dispatch caches, so *any* change to the
registry's functions map dropped the whole index and every subsequent gather
refilled it one full registry scan at a time.

That is expensive because `fn_resolve_gen` moves far more often than routines are
declared. [#8314](https://github.com/tokuhirom/mutsu/issues/8314) measured the
cycle: `JSON::Fast`'s `unjsonify-string` declares an inner `sub fetch-codepoint`,
so **every call** registers it (one bump) and the scope restore removes it again
(another bump). The registry ends each excursion in the state it started in, and
the index is gone twice over.

## What changed

Invalidation is pushed rather than polled. `invalidate_fn_resolution_for_keys`
names the registry keys a mutating site actually touched and evicts only those
base names; `invalidate_fn_resolution` stays the wholesale form for a site that
cannot say. The index no longer carries a generation of its own, and the five
remaining caches (`fn_resolve_cache`, `multi_compiled_key_cache`,
`multi_candidates_cache`, `declared_fn_cache`, `multi_fn_cache`) are untouched —
they still drop wholesale on every bump.

The six hot mutating sites now name their keys: the `RegisterSub` install, the
derive-once `my sub` re-install, `insert_multi_overload`, the lexical-shadow
`retain`, the typed-multi entry insert, and the plain insert. The scope restore in
`restore_registry_snapshot` diffs the two maps it already holds, which is exact
and costs one pass over a ~40-key map per restore — where the wholesale drop it
replaces cost a full registry re-scan *per base name per resolution* until the
index refilled.

Every key an installation can produce — `Pkg::name`, `Pkg::name/<arity>`,
`Pkg::name/<arity>:<types>`, a `__m<n>` multi tiebreak, an export alias in another
package — reduces to the same base name under `function_key_base_name`, so naming
one key evicts all of them.

## Measured

`MUTSU_VM_STATS=1` on three `from-json` decodes of a JSON document whose strings
carry escapes (which is what routes `JSON::Fast` through `unjsonify-string`), on a
release build:

| | before | after |
|---|---|---|
| index rebuild scans | 529 | **21** |
| registry keys visited by those scans | 19,303 | **667** |
| index entries thrown away | 527 | **6** |

`fn-resolve-gen-bumps` is unchanged at 362: this does not reduce the bumps, it
stops one cache from paying for them. Local wall clock on
`benchmarks/bench-json-fast.raku` shows no change outside run-to-run noise; the
bench CI history is the authority there.

## Why not the cheaper hypothesis

The issue asked first whether the enter/leave cycle could be made a no-op —
if `restore_registry_snapshot` left a per-call lexical sub in place, the
re-registration would report `Unchanged` and neither half would bump. That was
measured and **falsified**: keeping the excursion's new function keys does remove
the enter-side bumps (118 → 41, and the derive-once path's 77 → 0), but a
routine-local `my sub` that outlives its scope is visible to code that never
declared it. `t/routines`, `t/modules` and `t/vm/scope` fail in eight places,
`our-sub-sibling-block-redeclaration` and `pseudo-core-stash` among them. The
removal is load-bearing.

## Soundness

A missed key is a stale index, and the debug-only audit added by
[#8300](https://github.com/tokuhirom/mutsu/issues/8300) is what turns that into a
located panic instead of a silent mis-dispatch: `fn_base_name_registered`
re-scans the registry on every resolution in a debug build and asserts the cached
key list matches. The full `prove t/` suite (4,309 files, 45,966 tests) runs
debug and passes, so every mutating site this change converted is checked
against a fresh scan tens of thousands of times.

Pinned directly by `base_name_index_survives_an_unrelated_registration` in
`src/runtime/dispatch_resolve.rs`.

## Still open

[#8314](https://github.com/tokuhirom/mutsu/issues/8314) stays open. The
generation contract itself is unchanged — `fn_resolve_gen` still counts steps
rather than identifying map content, and the other five caches still drop
wholesale 362 times for a program that declares its routines once. Narrowing
them the same way needs its own audit first: the existing one checks a probe's
key *narrowing* against a full scan, not a memo cache's survival across
generations.
