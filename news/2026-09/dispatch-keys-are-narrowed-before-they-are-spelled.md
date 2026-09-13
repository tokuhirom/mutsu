# Dispatch keys are narrowed before they are spelled

mutsu resolved a function call by **building key strings with `format!` and
comparing them with `memcmp`**, on every call. Six call sites each wrote their
own spelling of the registry key grammar, and several of them then compared the
result against every registered routine key. [#8300](https://github.com/tokuhirom/mutsu/issues/8300)
measured the bill on three `from-json` decodes of a 2,380-byte META6 document —
the shape `mzef` walks for every metadata read — and found no hotspot at all,
just a dispatch mechanism that was string-keyed end to end:
`alloc::fmt::format::format_inner` at 9.75% inclusive with every top caller
building a dispatch key, `find_compiled_function_inner` at 7.90%,
`bare_name_packages` at 5.07% for 24,461 fresh `Vec<String>`s,
`Registry::has_multi_function` and `has_multi_candidates` at 2.89% and 1.65%
each scanning the whole functions map.

That flatness is why this took an ADR rather than a patch:
[ADR-0102](../../docs/adr/0102-dispatch-keys-are-narrowed-before-they-are-spelled.md)
records the rule the six sites now share. A dispatch key is **narrowed before it
is spelled**: a probe asks the base-name key index which handful of keys could
possibly answer, tests each key's shape *in place* rather than building the
prefix it was going to compare against, and only materializes text through the
new `runtime::dispatch_key` module — the key grammar written once, building into
a reusable per-thread scratch buffer that goes straight to `Symbol::lookup`. No
dispatch path allocates a `String` to look a routine up any more.

Two derived values stopped being recomputed alongside it. `bare_name_packages`
is a pure function of `(current package, innermost frame's lexical package)` —
the codebase already asserted as much wherever it keyed a memo on that pair — so
it is memoized on exactly that pair, as `Arc<[Symbol]>`, with no generation
guard, because a pure function of two inputs has nothing to invalidate. And
`fn_resolve_gen` acquired a single named entry point,
`Interpreter::invalidate_fn_resolution()`, whose `#[track_caller]` attributes
every bump to its source line.

**Result: 1,199,391,173 → 1,022,064,127 retired instructions, −14.8%** on that
workload (callgrind, release, a deterministic count). `format_inner` halves to
4.78%, `find_compiled_function_inner` to 4.76%, `multi_candidates_over` to
2.54%, and `bare_name_packages` leaves the profile.

A trivial `sub plain($a, $b) { $a + $b }` loop is unchanged (129.86M → 129.61M
Ir), and that corrects the issue's framing rather than falling short of it: such
a call is served by `call_compiled_function_positional_light_at`, which already
bypassed key construction. Keys cost what they cost on the calls that *miss* the
light path — `multi` dispatch, `nqp::` ops, calls into module code — which is
exactly what the META6 shape is made of, and why `benchmarks/bench-json-fast.raku`
is the row to watch rather than a micro-benchmark.

## The narrowing is checked, not argued

Replacing "scan every key for the prefix `{pkg}::{name}/`" with "scan the keys
the index filed under this base name" is sound because every key matching that
prefix reduces under `function_key_base_name` to that same base name — the arity
suffix it strips is the `/<digit>` registration always writes after the name, and
an operator name's own `/` survives because it is never digit-followed
(`infix:</>`). Rather than leave that as a comment, debug builds still run the
full scan and assert the two answers agree, next to the audit
`fn_base_name_registered` already ran against the index itself. CI runs
`prove t/` on a debug binary in both stress jobs, so a divergence fails at the
site instead of surfacing as a wrong dispatch.
`t/routines/dispatch/multi-candidate-key-narrowing.t` pins the shapes an
off-by-one would break: a name that is a proper prefix of another, an operator
carrying its own slash, enclosing-package and package-qualified candidates, and
a `my sub`/`multi sub` re-declared on every call.

That audit immediately earned its keep by finding a **latent bug that predates
this work**. `insert_multi_overload` adds a key to the functions map and never
announced it: the registration path bumps `fn_resolve_gen` only on its
*single*-sub branch, so a `multi` declaration left every name-keyed cache
holding a key set that no longer matched the registry. Nothing reached it
before, because no probe consulted the index in that window. The moment the
existence probes started doing so, `preregister_inline_package_subs` —
which registers a `multi trait_mod:<is>` and then asks
`has_multi_candidates("trait_mod:<is>")` about the candidate it has just
inserted — tripped the assertion with a located panic instead of a silently
missing candidate. Both that site and the sibling `entry().or_insert` arm now
announce their mutation.

## What the measurement found next

Two counters added for this work are now permanent under `MUTSU_VM_STATS=1`, and
they name the next problem precisely. Over the same three decodes:

```
fn-keys-by-base: lookups=13244 scans=11550 scan_keys=438501 invalidated_entries=1927
fn-resolve-gen-bumps total=1128 by site (top 3):
  vm_register_sub_ops.rs:408=376  accessors_misc.rs:311=338  registration_sub.rs:896=335
```

A program that declares its routines once bumps `fn_resolve_gen` **1,128 times**,
and every bump drops six name-keyed caches wholesale. The three sites are one
cycle: JSON::Fast's `to-json` and `unjsonify-string` declare inner `sub`s, so
entering such a routine registers them and leaving it restores the registry
snapshot that removes them again. So the index is rebuilt ~1,900 times for a
routine set that never really changes.

That is why ADR-0102 deliberately does **not** adopt callsite inline caching:
a cache keyed by callsite is worth no more than the generation it hangs off, and
a seventh cache dropped twice per call is not a fix. The real fix is a
generation that identifies map *content* — `Registry::functions` is
copy-on-write, and a snapshot restore puts back the very same `Arc`, so "we have
seen this state before" is decidable by identity — with entries tagged per
generation rather than dropped wholesale. That is a correctness-critical change
to the invalidation contract and gets its own ADR, written against the histogram
this work added: [#8314](https://github.com/tokuhirom/mutsu/issues/8314). The
cheaper hypothesis to test first is recorded there too — whether the enter-side
registration can simply stop being an `Installed` event, which would remove both
halves of the cycle and need no version redesign at all.

The other thing the profile now shows is
[#8315](https://github.com/tokuhirom/mutsu/issues/8315): `resolve_lexical_type_key`
is the *type* registry doing exactly what ADR-0102 forbids for the function
registry — 2,320 calls, 859,945 key comparisons, scanning all four type maps for
a `{name}\0` prefix that a program declaring no lexical type never has. It is
now the largest single remaining item, at 4.12%.
