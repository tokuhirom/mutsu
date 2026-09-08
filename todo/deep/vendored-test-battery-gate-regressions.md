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

## 1. `NativeLibs t/01-basic.t` test 6 — root-caused: custom `sub EXPORT` does not install type objects

Measured: `visible` under the native provider, `MISSING` under the vendored one —
but the provider is a red herring. **Two independent mutsu divergences were
cancelling each other out**, and the `use-ok` EVAL merely stopped the cancellation.

`NativeLibs.pm6` makes `NativeCall` visible to its importer with a custom export
sub, `sub EXPORT(|) { Map.new('NativeCall' => NativeCall, ...) }`, placed above
its `unit module` line.

Measured truth table (fixtures under `tmp/ex/`, `tmp/pre/`, plain `use`, no EVAL
and no `Test` anywhere):

| module shape | rakudo | mutsu |
| --- | --- | --- |
| custom `sub EXPORT` returning a Map/Hash of a **type object** | `visible` | `MISSING` |
| `use X` above the `unit module` line | `MISSING` | `visible` |

So mutsu (a) does not implement the custom-`EXPORT` half and (b) leaks a module's
file-scope `use`d packages into its importer, which rakudo does not. Test 6 was
passing under the native provider **for the wrong reason** — (b) supplied the
answer rakudo gets from (a). `use-ok`'s EVAL rolls the (b) leak back, and the
absence of (a) becomes visible.

### The actual defect

`install_export_symbol` (`src/runtime/runtime_module_export_sub.rs:149`) installs
every exported symbol with `self.env.insert(env_key, value)`. That is right for
`$scalar`/`@array`/`%hash`/`&sub`, but a **sigilless type object** is not read out
of `env` — a bare type name and `::('Name')` resolve through the type/class
registry. The entry lands somewhere nothing reads, so:

```raku
# tmp/ex/CustomExp2.rakumod:
#   use v6;
#   sub EXPORT(|) { my %h = 'MyAlias' => Int; %h }
#   unit module CustomExp2;
use lib <tmp/ex>;
use CustomExp2;
say (try { MyAlias.gist }) // 'MISSING';   # rakudo: (Int)     mutsu: MyAlias
say ::('MyAlias') ~~ Failure;              # rakudo: False     mutsu: True
```

mutsu answers the bare name with the **bareword string** `MyAlias`, i.e. the name
never resolved at all. Note this reproduces with a plain `Hash` as well as a
`Map`, so it is not the `ValueView::Hash`-only match in `install_export_map`.

**The fix**: when an exported symbol is sigilless and its value is a type object,
register it in the type/class registry under that name (what `::()` and a bare
type name consult), not only in `env`.

### The one-line fix, and why it must NOT land alone (measured)

`apply_module_export` looks the sub up with `self.resolve_function("EXPORT")`,
which finds only `GLOBAL::EXPORT`. A `unit module Foo;` file runs its body under
package `Foo`, so its export sub registers as `Foo::EXPORT` and the lookup misses
it — meaning the custom export is **never called at all** for the whole
`unit module` family. Measured, with a `note` inside the sub to prove invocation:

| `sub EXPORT` placement | invoked? | bare name |
| --- | --- | --- |
| file with no `unit module` | yes | `(Int)` — the only working shape |
| above the `unit module` line | **no** | `MyAlias` (a bareword string) |
| below the `unit module` line | **no** | `MyAlias` |

Also consulting the module-qualified name makes all three run and all three
answer `(Int)`, matching rakudo. **That change was written, built and then
reverted deliberately** — it is correct in isolation and makes things worse in
practice:

- `NativeLibs t/01-basic.t` does not go green, it moves: test 6 passes, and the
  now-running `EXPORT` dies at test 1 with `No such method 'dispatcher' for
  invocant of type 'Any'`. Its first statement is
  `&trait_mod:<is>.candidates.first: { .signature ~~ :(Routine, :$native!) }`,
  which finds no candidate, so `$exp` is `Any`.
- Worse, it regresses `t/prelude-helper-not-block-lexical.t` — DBIish's
  `install-driver` starts failing for both SQLite and mysql — because running
  `NativeLibs`'s EXPORT turns a silent no-op into a hard error partway through
  the module's import.

So this fix is **gated on multi-dispatch introspection**: `&trait_mod:<is>`
needs `.candidates` returning real candidates, `.signature` smartmatchable
against `:(Routine, :$native!)`, and `.dispatcher`. Land those first, then the
`EXPORT` lookup fix, then re-measure both files together. Shipping the lookup
fix on its own converts a wrong-but-quiet answer into a loud failure across every
`unit module` that has a custom `EXPORT`.

Divergence (b) — the file-scope `use` leak — is a separate correctness bug and
should be fixed on its own. Fixing it will make this test fail under the *native*
provider too until (a) lands, which is correct: the test asks for the custom
export.

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
