# ADR-0102: Dispatch keys are narrowed before they are spelled

- **Status**: Accepted
- **Date**: 2026-09-13
- **Issue**: [#8300](https://github.com/tokuhirom/mutsu/issues/8300)
- **Builds on**: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md), [ADR-0084](0084-the-frame-env-is-not-the-programs-symbol-table.md)

## 1. Context

mutsu registers a routine in `Registry::functions` under a `Symbol` whose *text*
encodes everything dispatch needs to know about it:

```text
GLOBAL::plain                    a plain `sub`
GLOBAL::plain/2                  arity-keyed
GLOBAL::plain/2:Int,Str          arity + argument type signature
GLOBAL::plain/2#1f3a…            arity + one candidate's body fingerprint
GLOBAL::plain/2__m3              the third `multi` candidate at that arity
```

That is a reasonable encoding. What was not reasonable is that six call sites
each re-derived their own spelling of it with `format!`, and several of them
then compared the result against **every** registered key. #8300 measured the
consequence on three `from-json` decodes of a 2,380-byte META6 document
(callgrind, release build, 1.20 billion Ir):

| | inclusive Ir | what it was doing |
| --- | --- | --- |
| `alloc::fmt::format::format_inner` | 9.75% | building dispatch keys, nothing else in the top callers |
| `find_compiled_function_inner` | 7.90% | up to fifteen `format!`ed probe keys per dispatch |
| `bare_name_packages` | 5.07% | a fresh `Vec<String>` per call, 24,461 calls |
| `multi_candidates_over` | 4.28% | a `format!`ed prefix per package, then a key walk |
| `Registry::has_multi_function` | 2.89% | full functions-map scan, `format!`ed prefixes |
| `Registry::has_multi_candidates` | 1.65% | the identical scan, for a different caller |

The profile is flat, which is the finding: there is no hotspot to fix, only a
mechanism that is string-keyed end to end.

Two further measurements taken for this ADR (both now permanent
`MUTSU_VM_STATS=1` counters) say where the *remaining* cost lives, and they
matter for what this ADR decides **not** to do yet. Over the same three decodes:

```
fn-keys-by-base: lookups=13244 scans=11550 scan_keys=438501 invalidated_entries=1927
fn-resolve-gen-bumps total=1128 by site (top 3):
  vm_register_sub_ops.rs:408=376  accessors_misc.rs:311=338  registration_sub.rs:896=335
```

`fn_resolve_gen` is bumped 1,128 times by a program that declares its routines
once. Every bump drops six name-keyed caches **wholesale**. The three sites
responsible are one cycle: JSON::Fast's `to-json` and `unjsonify-string` declare
inner `sub`s, so entering such a routine registers them (two of the sites) and
leaving it restores the registry snapshot that removes them again (the third).
The index is therefore rebuilt ~1,900 times for a program whose routine set
never really changes.

## 2. Decision

**A dispatch key is narrowed before it is spelled, and spelled in one place.**

Three rules, in the order a lookup should apply them:

1. **Narrow with the base-name index, never with a scan.** Any probe or gather
   asking "which registered keys could answer this name" consults
   `Interpreter::fn_keys_for_base`, which maps a base name to the handful of
   keys carrying it. Walking `Registry::functions` in a per-dispatch path is
   prohibited.
2. **Test a key's shape in place; do not build the string you were going to
   compare against.** `dispatch_key::key_is_candidate_of(key, pkg, name)`
   answers `key.starts_with(&format!("{pkg}::{name}/"))` without the `format!`
   and without the two-way substring searcher `str::starts_with` constructs.
3. **When a key's text really is needed, build it through
   `runtime::dispatch_key`.** That module is the single written form of the key
   grammar above. It builds into a reusable per-thread scratch buffer and hands
   the text straight to `Symbol::lookup` (a probe, which never grows the symbol
   table) or `Symbol::intern` (a registration). No dispatch path allocates a
   `String` to look a routine up.

Supporting these, two derived values stop being recomputed:

- `bare_name_packages` is a **pure function** of `(current package, innermost
  frame's lexical package)` — the codebase already asserted this where it keyed
  memos on that pair. It is now memoized on exactly that pair as
  `Arc<[Symbol]>`. A pure function of two inputs needs no invalidation, which is
  why this is the one memo here with no generation guard at all.
- `Interpreter::invalidate_fn_resolution()` is the single entry point for
  `fn_resolve_gen`. `#[track_caller]` attributes every bump to its source line,
  so the over-invalidation above is measurable rather than inferred.

### Soundness of the narrowing

Rule 1 replaces "scan every key for the prefix `{pkg}::{name}/`" with "scan the
keys the index filed under `name`'s base name". These agree because every key
matching that prefix reduces, under `function_key_base_name`, to that same base
name: the arity suffix it strips is the `/<digit>` immediately after `{name}`,
which registration always writes, and the package prefix it strips is `{pkg}::`.
The *query* name goes through the same extraction, so a qualified `Foo::bar` and
a bare `bar` land in the bucket holding the keys that answer them. An operator
name's own `/` survives because it is never digit-followed (`infix:</>`).

That argument is **checked, not trusted**: in debug builds
`Registry::any_candidate_key_of` still runs the full scan and asserts the two
answers agree, and `fn_base_name_registered` already audits the index itself
against a fresh scan. CI runs `prove t/` on a debug binary in both the
`gc-stress-tap` and `jit-stress-tap` jobs, so a divergence fails the suite at
the site rather than surfacing as a wrong dispatch.

That audit found a **latent bug older than this ADR** on its first full run.
`insert_multi_overload` adds a key to the functions map and never called
`invalidate_fn_resolution()`; the registration path bumps only on its
*single*-sub branch, so a `multi` declaration left every name-keyed cache
holding a key set that no longer matched the registry. Nothing had reached it,
because no probe consulted the index in that window — until these probes did,
and `preregister_inline_package_subs` (which registers a `multi trait_mod:<is>`
and then asks `has_multi_candidates("trait_mod:<is>")` about the candidate it
has just inserted) tripped the assertion. This is the argument for auditing an
index rather than reasoning about it: the failure mode is a silently missing
candidate, which no ordinary test asserts against.

Where a `&self` caller cannot reach the lazily-filled index — the EVAL-time
undeclared-name checks — it passes `None` and pays the full scan explicitly
(`has_multi_function_unindexed`). That is a per-compilation-unit cost, not a
per-call one, and making it explicit keeps the hot path honest.

## 3. What this does NOT decide

**Callsite inline caching is not adopted, and the invalidation redesign is
deferred.** #8300 listed two directions; this ADR takes the first (interned,
narrowed, allocation-free keys) and explicitly leaves the second open, because
the measurement above says the blocker is not the cache *shape* but the
`fn_resolve_gen` *contract*:

A cache keyed by callsite is worth no more than the generation it hangs off. As
long as entering and leaving a routine that declares an inner `sub` bumps the
generation twice, any cache guarded by it is dropped twice per call, and a
callsite cache would simply be the seventh such cache.

The shape of a real fix is visible but not yet justified by evidence we have:
`Registry::functions` is a copy-on-write `Arc<HashMap>`, and
`restore_registry_snapshot` restores *the very same `Arc`* the snapshot took. So
"the functions map is back to a state we have already seen" is decidable by
identity, and a generation that returns to its previous value on such a restore
would let pre-excursion cache entries survive. Doing that soundly needs a
version that identifies map *content* rather than counting steps — a monotonic
`+1` would let two different excursions collide on the same stamp and serve one
excursion's entries during the other — and needs entries tagged per-generation
rather than dropped wholesale. That is a correctness-critical change to the
invalidation contract and deserves its own ADR, written against the
`fn-resolve-gen-bumps` histogram this one adds. Filed as
[#8314](https://github.com/tokuhirom/mutsu/issues/8314).

Two adjacent problems are also explicitly out of scope:

- `Env::get_sym_with_fallback` (3.5% of the same profile) is #8300's third
  question — whether env symbol lookup shares this key space. It does not share
  it today, and nothing here changes that.
- `resolve_lexical_type_key` (4.1%, and now the largest single remaining item)
  is the *type* registry doing the same thing this ADR forbids for the function
  registry: 2,320 calls scanning `classes`/`roles`/`enum_types`/`subsets` for a
  `{name}\0` prefix, 859,945 key comparisons, essentially always failing because
  the program declared no lexical type at all. The same index-then-compare
  treatment applies, but it is a different registry with a different
  invalidation story, and folding it in would make one PR span two subsystems.
  Filed as [#8315](https://github.com/tokuhirom/mutsu/issues/8315).

## 4. Consequences

Measured on the same three-decode workload, release build, callgrind (a
deterministic count, not wall clock):

| | Ir | |
| --- | --- | --- |
| before | 1,199,391,173 | |
| after | 1,022,064,127 | **−14.8%** |

`format_inner` falls from 9.75% to 4.78% inclusive, `find_compiled_function_inner`
from 7.90% to 4.76%, `multi_candidates_over` from 4.28% to 2.54%, and
`bare_name_packages` leaves the profile entirely.

A trivial `sub plain($a, $b) { $a + $b }` loop is **unchanged** (129.86M →
129.61M Ir), and that is not a disappointment but a correction to #8300's
framing: such a call is served by
`call_compiled_function_positional_light_at`, which already bypasses key
construction. The keys cost what they cost on the calls that miss the light
path — `multi` dispatch, `nqp::` ops, calls into module code — which is what the
META6 shape is made of and why that is the benchmark to watch.

`benchmarks/bench-json-fast.raku` records the META6 shape on every push to main,
so the bench CI history (`bench-history.tsv` on the `bench-data` branch) is
where this shows up as a trend rather than as a local number.

The cost side: four dispatch predicates that were `&self` are now `&mut self`
(they fill the lazy index), which is why the EVAL-time probes needed the
explicit `_unindexed` spelling. The debug-build audits make a debug `prove t/`
slower than before, in exchange for the narrowing being verified across the
whole suite rather than argued for in a comment.
