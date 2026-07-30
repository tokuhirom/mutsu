# Module export scan memoized — grammar-heavy module load no longer slower than raku

`todo/tickets/grammar-heavy-module-load-slower-than-raku.md` reported that
loading `Template::HAML` (30 modules, grammar-heavy) cost mutsu ~0.8s per
process against raku's ~0.3s, inverting mutsu's fast-startup headline, with the
cost fixed per process rather than proportional to the work done.

## Root cause

The cost was not in the grammar at all. The parser's `use` handling
(`register_module_exports` in `parser/stmt/simple/module_exports.rs`) scans the
used module's source file for `is export` subs, declared type names, and enum
values — by reading the file and running a full `parse_program_partial` over
it, with no cache. The scan is recursive (the nested parse of module A hits A's
own `use B` lines, each triggering its own scan), and the `LOADING_MODULES`
guard only breaks cycles within one scan stack — it does not deduplicate
diamond dependencies. In a dependency graph like Template::HAML's, where
`Template::HAML::X` is `use`d by 14 sibling modules, the same file was
re-read and re-parsed once per reachable `use` mention: strace showed
`X.rakumod` opened **222 times**, 751 `.rakumod` opens in total, for a single
`use Template::HAML`.

## Fix

The scan result (exports + type names + enum values, own and transitive) is now
memoized per resolved file path in a thread-local cache; a cache hit replays
the stored registrations into the importer's current scope without any I/O or
parsing. Two correctness guards:

- A scan truncated by the `LOADING_MODULES` recursion guard (a real `use`
  cycle) is not cached, since it is missing the cycle partner's transitive
  contribution; importers outside the cycle still get a fresh, complete scan.
- The scan now saves and restores `PACKAGE_PATH` around the nested parse (the
  nested `reset_user_subs` used to clear it and nothing put it back), so a
  cache hit — which skips the nested parse entirely — is indistinguishable
  from a miss, and a `use` inside a `package Foo { ... }` body no longer wipes
  the composition path for the rest of the body.

Pinned by `t/module-export-scan-cache.t` (miss and hit blocks assert exported
subs, exported operators, type terms, and enum values all replay). The test
also surfaced a pre-existing re-`use` method-dispatch bug, recorded in
`todo/tickets/reuse-in-block-class-method-dispatch.md`.

## Results (release build, 2026-07-30, Template::HAML dist)

| | before | after | raku |
| --- | --- | --- | --- |
| `use Template::HAML` (warm precomp) | 0.80s | **0.29s** | 0.22s |
| same, cold (`MUTSU_PRECOMP=0`) | 2.15s | **0.40s** | 13.2s (first, no precomp store) |
| `t/0030-tags.rakutest` | 0.87s (ticket) | **0.16s** | 0.27s |
| `t/0040-haml-render.rakutest` | 2.73s (ticket) | **0.33s** | 0.92s |
| `.rakumod` file opens per load | 751 | **105** | — |

The test-file runs are now faster than raku; the pure-`use` line is within
1.3× of raku's precompiled load (the residual is the runtime's per-module
load, no longer the parser scan).
