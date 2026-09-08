# The vendored `Test` provider regresses four bundled-library suites

The `Bundled-library test suites` gate (`scripts/battery-testsuite.sh`, run inside
CI's `test` job) is the last thing red on the vendored-`Test`-by-default switch
(#7523). It reports **282/312 with 9 regressed whitelisted files**.

These are *not* `Test` compatibility gaps — they are general interpreter bugs the
real module's code shapes reach and mutsu's native provider never did, which is
the same pattern the rest of that campaign followed (`is rw` slice writeback, the
deferred-grep capture merge, the EVAL/import loss). Each one below is a separate
root cause and wants its own fix.

## How to tell a provider-caused failure from an environment one

Run the file under both providers; the switch is the control, not any single
commit on the branch:

```
cd tmp/battery-testsuite/<Battery>
                     target/release/mutsu -I lib t/<file>     # vendored (default)
MUTSU_REAL_TEST=0    target/release/mutsu -I lib t/<file>     # native
```

Passing under `MUTSU_REAL_TEST=0` and failing by default means the switch caused
it. (Reverting only one fix commit is the *wrong* control and gave a false
"not mine" reading once already.)

## 1. `NativeLibs t/01-basic.t` test 6 — `::('NativeCall')` goes missing

Measured: `visible` under the native provider, `MISSING` under the vendored one.

```raku
use Test;
use lib <tmp/battery-testsuite/NativeLibs/lib>;
use-ok 'NativeLibs';
use NativeLibs;
say "NativeCall pkg: ", (::('NativeCall') ~~ Failure ?? 'MISSING' !! 'visible');
```

The *package* lookup fails while `::('NativeCall::EXPORT::ALL')` still resolves,
so this is the package/type sibling of the routine-registry bug fixed in
`news/2026-09/module-loaded-in-an-eval-keeps-its-imports.md` — a different store
(`module_package_globals` / the class registry) with the same hole.

**Bisected, and the obvious repro does NOT reproduce it.** None of these are
enough on their own — each answers `visible`:

- `EVAL 'use NativeLibs; 1'` at file scope, then `use NativeLibs`
- the same `EVAL` from inside a plain sub
- the same, wrapped in `try { ... }` (the shape `use-ok` actually has)
- `use Test` plus any of the hand-rolled loaders above

It reproduces **only with `Test`'s own `use-ok`**, i.e. when the EVAL runs inside
a routine that belongs to *another loaded module*. That is the lead worth
pulling: the module load is presumably attributed to `Test`'s package/scope
rather than to the requesting one. Version adverbs (`:v<0.0.9>`, `:ver<0.0.9>`)
are irrelevant — dropping them changes nothing.

## 2. `Digest t/rfc4231.t` — HMAC returns wrong bytes

Measured: 0 failures under the native provider, 3 under the vendored one. This
one is alarming because it is a **wrong computed value**, not a reporting
difference:

```
# expected: Blob[uint8].new(149,233,160,219,150,32,149,173,...)
#      got: Blob[uint8].new(118,223,40,84,193,151,143,42,...)
```

Almost certainly the same family as the deferred-grep capture-merge bug: only
observable once `reflective_name_access_possible()` has latched, which every file
loading the real `Test` does.

**Not yet reduced.** The isolated shapes all give the *correct* answer — the
131-byte-key (longer than block size) case direct, via the `constant %sha224 =
hash => &sha224, block-size => 64` flattening, with `use Test` loaded, and inside
a `subtest`. Failures start at the 5th of the file's 7 subtests, so something
accumulates across them; reduce by bisecting the real file's subtests rather than
rebuilding it from scratch.

## 3-4. `Cro::HTTP http-middleware.rakutest`, `http2-request-parser.rakutest`

Not yet reduced. `http2-request-parser` fails one assertion of 60;
`http-middleware` dies with `X::Cro::HTTP::Error::Client`. Both also fail with
the EVAL/import fix reverted, so they belong to the switch rather than to that
commit; confirm with the provider A/B above before investigating.

## The `DBIish` rows are environment, not this

The five `DBIish` rows in the gate's CI output (`25-mysql-common`,
`35-pg-common`, `55-oracle-common`, `70-sqlcipher-memory`, `71-sqlcipher-common`)
need live database servers and cannot be judged from a container without them.
Establish whether they move under the provider A/B on a runner that has the
services before treating them as this campaign's work.

## Do not re-baseline the gate to get green

The whitelist records these files as passing. Lowering it to accommodate the
switch would hide exactly the compatibility regression the gate exists to catch —
and BATTERIES.md's whole argument for rung 2 is that the real module must run
verbatim. If the trade-off is ever worth making, it is the repository owner's
call, not a step to take while chasing a green check.
