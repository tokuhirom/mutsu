# Two multi candidates' same-named inner subs share one registry key

A `sub` declared inside a routine body is registered as
`<current package>::<name>`. Two candidates of the same `multi` in the same
package that each declare a helper of the same name therefore write the same
key, and one body ends up executing the other's helper. `Digest::SHA2` is the
live case: `sha256` and `sha512` each declare `rotr`, `Σ0`, `Σ1`, `σ0`, `σ1`,
and mutsu runs **sha256's 32-bit `Σ0`/`Σ1`/`σ0`/`σ1` inside sha512's 80-round
compression**, so `sha512` returns a wrong digest.

This has nothing to do with the vendored `Test` module — it was found through
that work (`todo/deep/vendor-real-test-module.md`) but reproduces in a plain
script with no `Test` at all.

## Repro

```sh
cargo build --release
scripts/battery-testsuite.sh >/dev/null 2>&1     # fetches the battery clones
cd tmp/battery-testsuite/Digest
```

```raku
# hmac-seq.raku
use lib <lib>;
use HMAC;
use Digest::SHA2;
sub hex-to-blob { blob8.new: $^str.comb(/../).map({:16($_)}) }
constant %sha224 = hash => &sha224, block-size =>  64;
constant %sha384 = hash => &sha384, block-size => 128;
constant %sha512 = hash => &sha512, block-size => 128;

my ($k1, $m1) = Blob.new(1..25), Blob.new(0xcd xx 50);
say "1 sha224: ", hmac(key => $k1, msg => $m1, |%sha224)
    eqv hex-to-blob("6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a") ?? "OK" !! "WRONG";

my ($k2, $m2) = Blob.new(0x0c xx 20), "Test With Truncation";
say "2 sha384: ", hmac(key => $k2, msg => $m2, |%sha384).subbuf(0,16)
    eqv hex-to-blob("3abf34c3503b2a23a46efc619baef897") ?? "OK" !! "WRONG";
say "3 sha512: ", hmac(key => $k2, msg => $m2, |%sha512).subbuf(0,16)
    eqv hex-to-blob("415fad6271580a531d4179bc891d87a6") ?? "OK" !! "WRONG";
```

```
mutsu:  1 sha224: OK    2 sha384: OK    3 sha512: WRONG
raku:   1 sha224: OK    2 sha384: OK    3 sha512: OK
```

All three lines are load-bearing: drop either of the first two and the third is
correct. Deterministic; identical under `MUTSU_JIT=off`, `MUTSU_GC=off` and
`MUTSU_GC=on`.

## Evidence

Instrumenting the vendored `Digest::SHA2` so each helper announces itself
(`note "512-BigS0"` / `note "256-BigS0"`, …) on the failing run:

```
576 256-BigS0     576 256-BigS1     448 256-smS0     448 256-smS1
320 512-BigS0     320 512-BigS1     256 512-smS0     256 512-smS1
```

The sha512 work is 8 blocks × 80 rounds = 640 `Σ0` calls, and only **320** of
them reach sha512's own `Σ0`. The other 320 land in sha256's, which is exactly
the 576 − 256 excess on the sha256 side (256 is subtest 1's honest sha256
work). The `sha384` hmac gets its own helpers; the plain `sha512` hmac gets
sha256's throughout — which is why `sha384` passes and `sha512` fails.

Everything the routine is *handed* is right: instrumentation shows the correct
`$data` bytes, the correct `$initial-hash` (`cbbb9d5dc1059ed8…` for sha384,
`6a09e667f3bcc908…` for plain sha512), the correct round-constant table
(`428a2f98d728ae22, 7137449123ef65cd, b5c0fbcfec4d3b2f`), and the right multi
candidate with the right `&hash`. The per-block trace diverges at the *output*
of the first block, with identical inputs.

Registration logging (an `eprintln!` on the `functions` insert and the
lexical-shadow `retain` in `registration_sub.rs`, filtered to names containing
`Σ`) shows why:

```
DBGREG shadow-remove Digest::SHA2::Σ0
DBGREG insert key=Digest::SHA2::Σ0 pkg=Digest::SHA2
DBGREG shadow-remove Digest::SHA2::Σ1
DBGREG insert key=Digest::SHA2::Σ1 pkg=Digest::SHA2
DBGREG shadow-remove Digest::SHA2::Σ0        <- the other candidate, same key
DBGREG insert key=Digest::SHA2::Σ0 pkg=Digest::SHA2
DBGREG shadow-remove Digest::SHA2::Σ1
DBGREG insert key=Digest::SHA2::Σ1 pkg=Digest::SHA2
```

Both registrations happen at hoist time, before either body runs, and both use
`format!("{}::{}", self.current_package(), name)` — the package, with nothing
identifying the enclosing routine. Renaming sha512's four helpers makes the
file pass; renaming sha256's does not, so the surviving entry is sha256's and
sha512's body is the one that loses.

## Why the registry is reached at all

A body normally calls its own inner sub through a compiled/lexical path and
never consults the package-keyed registry, which is why `sha256` stays correct
and why a bare `sha224(...)` call before the sha512 work does **not** trigger
the bug. It takes the `hmac` shape to knock sha512's body onto the registry
fallback: `hmac` lives in a different module and calls its hash through a
`:&hash` named-Callable parameter inside a `reduce` block. Neither
`my &h = &sha224; h($blob)` nor `reduce -> $m, $i { h($m) }, $blob, 0` is
enough on its own — both leave the file passing — so the trigger is that
indirection combined with the two earlier hmacs.

Finding which cache or fallback flips between the sha384 hmac and the sha512
hmac is the remaining work. Start by logging the resolved def identity (not
just the name) at the `Σ0` call site inside sha512's inner `reduce` block.

## The fix

Key a routine-body-local `sub` by its enclosing routine, not by the package
alone — it is not `our`, is not exported, and is not visible outside the body,
so it has no business sharing a package-level name with a sibling candidate's
helper. Related design gap:
`todo/deep/module-package-sub-invisible-from-method-body.md`, which is about
the same key being *too narrow* from a method body; this is the same key being
*too wide* across two bodies.

It is filed deep rather than as a ticket because the key shape is load-bearing
for a lot of dispatch, including the lexical-shadow removal at
`registration_sub.rs`'s `allow_lexical_shadow` branch and every name-keyed
resolution cache built on top of it.

## Not a recent regression

`hmac-seq.raku` fails identically on a release build of `3595340`, which
predates the 2026-09-06/07 perf passes on
`todo/deep/vendor-real-test-module.md` and the name-keyed resolution
memoizations they added. So this is not fallout from those, and the fix is the
key-shape change above rather than a cache-invalidation patch. How much further
back it goes was not measured.
