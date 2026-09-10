# The vendored upstream `Test` module is the default provider

A bare `use Test` now loads `modules/Rakudo-Core/lib/Test.rakumod` — rakudo's own
`Test`, vendored verbatim (md5 `f34dec45d52ad099c37f42fdbd93e277`, 953 lines of
ordinary Raku, never renamed or shimmed). Mutsu's native TAP provider
(`src/runtime/test_functions.rs`) is no longer what the suite stands on; it
survives only behind `MUTSU_REAL_TEST=0`, as the escape hatch the dual-provider
sweeps drive, and is scheduled for deletion (#7566).

This is `BATTERIES.md` rung 2 in its intended form. The campaign's real product
was never the flip itself but the ~130 general interpreter fixes the module
forced along the way: it exercises phasers, `EVAL`, `callframe`, custom traits,
`nqp::` ops, subtests and lazy evaluation, so every gap it hit was a gap in
mutsu, not in `Test`. Those all landed independently. What remained was the
switch.

## Why the first attempt was withdrawn, and what changed

The flip was tried on 2026-09-07/08 and pulled back the next day. Three of
#7554's four completion criteria were met; the fourth — the `Bundled-library
test suites` gate — reported 282/312 with nine regressed whitelisted files. None
of them was a `Test` compatibility problem. They were unrelated interpreter gaps
that only the real module's code shapes reached, which is the same pattern the
rest of the campaign followed, except that these did not have fixes small enough
to ride along.

They were root-caused and fixed one at a time, and each is a general fix that
stands on its own merits:

| gate row | root cause | fix |
| --- | --- | --- |
| `Digest rfc4231.t` | the sub registrar's idempotent re-registration fast path accepted "something is registered under this name" as proof of identity, so `sha256` ran its compression loop calling `sha512`'s 64-bit `rotr`/`Σ0` | #7653 |
| `Cro::HTTP http-middleware.rakutest` | a tail block is inlined as its body's return value, and inlining dropped its phasers — so every `LEAVE $service.stop()` in a tail block was compiled away and the previous subtest's server kept answering | #7663 |
| `NativeLibs 01-basic.t` | a custom `sub EXPORT` above a `unit module` line was never invoked, plus `.dispatcher` answering a candidate instead of the proto, NativeCall's missing `trait_mod:<is>` export surface, and a `Signature` literal losing its parameters across the precompilation cache | #7714 |
| four `DBIish` `*-common` rows | `need Foo` left `Foo`'s own methods unable to resolve the subs its compunit had imported | #7810 |
| `NativeLibs 01-basic.t` again | a module first loaded inside an `EVAL` lost its package name and `EXPORT::` stash in the importer | #7814 |
| `Cro::HTTP http2-request-parser.rakutest` | not mutsu's: the upstream file calls `ok` from inside `start` blocks, so two concurrent HTTP/2 streams interleave its TAP. Reported as croservices/cro-http#217 and excluded from the gate until that lands; the chase produced five interpreter perf fixes anyway (#7786, #7796, #7800, #7801, #7812) | #7667 |

Three of those rows were, before the fixes, **passing under the native provider
for the wrong reason** — a file-scope `use` leak supplying what rakudo gets from
a custom export, a `--log=` regex matching `--level=trace`, a native `use-ok`'s
leak standing in for an `EXPORT::ALL` stash. That is the pattern to expect when
resuming work like this: a green row under the native provider is not evidence
that the mechanism under it is right.

## The measurement that authorized the flip

`main` at `ab431dd3`, release build, both runs back to back in the same
container:

```
MUTSU_REAL_TEST=1  scripts/battery-testsuite.sh  ->  291/311, GATE PASSED
                   scripts/battery-testsuite.sh  ->  289/311, GATE PASSED
```

The vendored module passes two files *more* than the provider it replaces
(`DBIish 48-sqlite-errors`, `Log::Async 01-basic`, neither whitelisted), and no
whitelisted file regresses under it. The only row where it does worse is
`Log::Async 04-filter` (9/10 native, 7/10 vendored) — not whitelisted, failing
under both, and left as a known residual gap.

The six `2x-mysql-*` rows are worth recording, because #7555 had them down as
unjudgeable without live database servers. They are not: they failed identically
under both providers with `NativeCall: symbol 'mysql_init' not found`, and
installing `libmariadb3` — the client library, no server — makes all six reach
DBIish's own `connect-or-skip` and pass 109/109 under both. The missing piece was
never a server.

The other two criteria were re-verified on the same build:

| suite (vendored, release) | result |
| --- | --- |
| `prove -j4 -e target/release/mutsu -r t/` | 3946 files / 41966 tests, one failing file |
| `make roast` | 1436 files / 218939 tests, three failing files |

The three roast failures are the documented container-environment set
(`docs/agent-environments.md`): two `uid 0` vs `chmod` files and one sandboxed
network socket file. The single `t/` failure was
`t/is-deeply-user-raku-diagnostic.t`, the one file whose expectation genuinely
differs between the providers — rakudo and the vendored module send a non-TODO
failure's diagnostic to `$failure_output` (STDERR), the native provider to
STDOUT. Its two assertions moved from `:out` to `:err`, which is exactly what
the file's own comment had asked whoever did the flip to do.

## What the switch costs, and what it buys

Suite cost on a 4-core box: `t/` 94 s wallclock, `make roast` 303 s — the 2.0x
and 1.24x #7554 predicted. The `t/` doubling is the standing price of running
Raku's own `Test` as Raku code; five callgrind passes already took the
per-assertion cost from 492k to 235k instructions.

What it buys is that mutsu's test suite no longer stands on a mutsu-specific
reimplementation of the thing it is testing against. A `Test` bug is now a bug in
rakudo's `Test`, and every `t/` and roast file exercises the real module's
phasers, `EVAL`, `callframe` and trait machinery on every run.

## Provider steering, for anyone writing a test

`MUTSU_REAL_TEST` is read once at startup and now means the opposite of what it
did: unset selects the vendored module, and `0` / `false` / the empty string
select the native provider. Two `t/` files that steered their `is_run` children
by *deleting* the variable had to say `= '0'` instead — deleting it now lands the
child on the default, which is the half they were trying to contrast with.
