# Two multi candidates' same-named inner subs share one registry key

A `sub` declared inside a routine body is registered as
`<current package>::<name>`. Two candidates of the same `multi` in the same
package that each declare a helper of the same name therefore write the same
key, and one body ends up executing the other's helper. `Digest::SHA2` is the
live case: `sha256` and `sha512` each declare `rotr`, `Σ0`, `Σ1`, `σ0`, `σ1`,
and mutsu runs **sha256's 32-bit `rotr` inside sha512's 80-round compression**,
so `sha512` returns a wrong digest.

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

**The load-bearing collision is `rotr`, not the Greek names.** Renaming
sha512's helpers *including* `rotr` makes the repro pass; renaming only the
Greek four on either side does not. Instrumenting both `rotr`s (`note "R256"` /
`note "R512"`) on the failing run, with sha256's Greek helpers renamed so the
Σ/σ dispatch is provably clean:

```
5248 R256      2944 R512
```

sha512's own work is 8 blocks × 80 rounds × ~10 `rotr` calls ≈ 6400; it gets
2944. The rest run sha256's `uint32` version, whose 32-bit `+<` produces a
different digest. (With the Greek names left colliding too, the same
instrumentation shows 320 of sha512's 640 `Σ0` rounds landing in sha256's `Σ0`
— the collision is per name, and every shared name contributes.)

Everything the routine is *handed* is right: instrumentation shows the correct
`$data` bytes, the correct `$initial-hash` (`cbbb9d5dc1059ed8…` for sha384,
`6a09e667f3bcc908…` for plain sha512), the correct round-constant table
(`428a2f98d728ae22, 7137449123ef65cd, b5c0fbcfec4d3b2f`), and the right multi
candidate with the right `&hash`. The per-block trace diverges at the *output*
of the first block, with identical inputs.

## The registration side

Both declarations register under `format!("{}::{}", self.current_package(),
name)` — `Digest::SHA2::rotr`, with nothing identifying the enclosing routine.
Logging every entry to `register_sub_decl_with_metadata` for `rotr` shows 14
registrations and only **2 inserts**:

```
enter depth=0 traits=["__lexical_hoist","__hoisted"]   insert Digest::SHA2::rotr
enter depth=0 traits=["__lexical_hoist"]               unchanged
enter depth=0 traits=["__lexical_hoist","__hoisted"]   unchanged
enter depth=0 traits=["__lexical_hoist"]               unchanged
enter depth=1 traits=["__lexical_hoist","__hoisted"]   insert Digest::SHA2::rotr
enter depth=1 …                                        unchanged (×9)
```

The skips come from the site-fingerprint fast path in `registration_sub.rs`
("if `registered_fn_fingerprints[fq_sym]` already equals this declaration's
`site_fingerprint`, there is nothing to re-derive"). The fingerprints of the
two `rotr`s do differ (`12908437786956835830` vs `11610335429252450779`), and
the map is keyed by the shared `fq_sym` alone — so the entry tracks *whichever
body registered last*, and the other body's calls resolve to it. Changing a
helper's body does not help (the key is the name); adding a third body that
declares the same names does not dislodge the winner either.

## The fix

Key a routine-body-local `sub` by its enclosing routine, not by the package
alone — it is not `our`, is not exported, and is not visible outside the body,
so it has no business sharing a package-level name with a sibling candidate's
helper. `registered_fn_fingerprints` needs the same treatment: keyed by the
shared name, it cannot express "this body's copy is installed".

The cheaper-looking alternative — let a body's own in-sequence registration
always reinstall, instead of being skipped by the fingerprint fast path — only
narrows the window: two bodies would still fight over one key, and a closure
that outlived its declaring frame would still resolve to whoever wrote last.
Prefer the key change.

Related design gap:
`todo/deep/module-package-sub-invisible-from-method-body.md`, which is about
the same key being *too narrow* from a method body; this is the same key being
*too wide* across two bodies.

It is filed deep rather than as a ticket because the key shape is load-bearing
for a lot of dispatch, including the lexical-shadow removal at
`registration_sub.rs`'s `allow_lexical_shadow` branch, the site-fingerprint
fast path above, and every name-keyed resolution cache built on them.

## Why it takes the `hmac` shape to surface

A body normally reaches its own inner sub without a name lookup, which is why
`sha256` stays correct and why neither a bare `sha224($blob)` nor
`my &h = &sha224; reduce -> $m, $i { h($m) }, $blob, 0` triggers anything. It
takes `hmac` — a different module calling its hash through a `:&hash` named
Callable inside a `reduce` block — plus the two earlier hmacs to put sha512's
body on the name-resolution path. Establishing exactly which resolution step
that is, is the remaining diagnostic work; log the resolved def identity (not
the name) at the `rotr` call site inside sha512's `Σ0`.

## Not a recent regression

`hmac-seq.raku` fails identically on a release build of `3595340`, which
predates the 2026-09-06/07 perf passes on
`todo/deep/vendor-real-test-module.md` and the name-keyed resolution
memoizations they added. So this is not fallout from those, and the fix is the
key-shape change above rather than a cache-invalidation patch. How much further
back it goes was not measured.
