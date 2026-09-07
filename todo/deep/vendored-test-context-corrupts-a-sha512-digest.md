# Under the vendored `Test`, `sha512` returns a wrong digest for correct inputs

`Digest::SHA2`'s `sha512` computes a **different digest for byte-identical
input** depending on what the process did earlier, but only when the assertions
run through the vendored upstream `Test.rakumod` (`MUTSU_REAL_TEST=1`, the mode
`todo/deep/vendor-real-test-module.md` is trying to make the default). Under the
native TAP provider the same file is correct.

This is the largest single blocker left for that ticket: it fails the
bundled-library gate (`scripts/battery-testsuite.sh`), which is a CI step, so
the provider switch cannot land while it stands.

## Repro

```sh
cargo build --release
# fetch the battery clones once
scripts/battery-testsuite.sh >/dev/null 2>&1
```

Then, with `$D = tmp/battery-testsuite/Digest`:

```raku
# tmp/probe3.t
use Test;
use lib <lib>;
use HMAC;
use Digest::SHA2;

sub hex-to-blob { blob8.new: $^str.comb(/../).map({:16($_)}) }
constant %sha224 = hash => &sha224, block-size =>  64;
constant %sha384 = hash => &sha384, block-size => 128;
constant %sha512 = hash => &sha512, block-size => 128;

subtest {
  my ($key, $msg) = Blob.new(1..25), Blob.new(0xcd xx 50);
  is hmac(:$key, :$msg, |%sha224),
     hex-to-blob "6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a";
}

subtest {
  my ($key, $msg) = Blob.new(0x0c xx 20), "Test With Truncation";
  is hmac(:$key, :$msg, |%sha384).subbuf(0,16), hex-to-blob "3abf34c3503b2a23a46efc619baef897";
  is hmac(:$key, :$msg, |%sha512).subbuf(0,16), hex-to-blob "415fad6271580a531d4179bc891d87a6";
}
```

```sh
MUTSU_REAL_TEST=1 target/release/mutsu -I $D/lib tmp/probe3.t   # the sha512 row FAILS
MUTSU_REAL_TEST=0 target/release/mutsu -I $D/lib tmp/probe3.t   # passes
raku            -I $D/lib tmp/probe3.t                          # passes
```

The whole file is `roast`-free upstream code: `tmp/battery-testsuite/Digest/t/rfc4231.t`
reproduces it too (3 failures of 7 under the vendored module, 0 under the native
one).

## What the bisection established

Minimising `rfc4231.t` down to two `subtest` blocks:

- **Both blocks are needed.** Remove the first `subtest` and the second passes.
  Any one of its four `is hmac(...)` assertions is enough — which hash it uses
  does not matter.
- **The second block needs `sha384` followed by `sha512`.** Every other
  combination of its four assertions passes; `{384, 512}` is the minimal failing
  pair, and it is the `sha512` row that reports the wrong value.
- **It is deterministic.** Identical failure count under `MUTSU_JIT=off`,
  `MUTSU_GC=off`, `MUTSU_GC=on`, and across repeated runs.
- **The same computation outside `Test` is correct.** Running the same two
  blocks as plain code with `say` — with or without `use Test`, inside or
  outside a `subtest` — matches rakudo exactly. Calling `sha384` then `sha512`
  at file scope is correct. Calling `sha256`/`sha224` in a loop is correct.

## What is NOT wrong

Instrumenting `Digest::SHA2` inside the failing run shows the inputs are
right:

- `$data` is byte-identical to the standalone run (dumped as hex, 84 bytes,
  same bytes) and `$data.elems` matches.
- `$initial-hash` is the correct IV in both branches — `cbbb9d5dc1059ed8...`
  when `sha384` supplies it, `6a09e667f3bcc908...` for a plain `sha512` — so
  this is **not** a defaulted-named-parameter leak.
- The round-constant table `(BEGIN blob64.new: map { frac 3√$_, 64 }, @primes[^80])`
  starts `428a2f98d728ae22, 7137449123ef65cd, b5c0fbcfec4d3b2f`, which is
  correct.
- `constant @primes = grep *.is-prime, 2 .. *` re-indexes correctly
  (`@primes[^8]` and `@primes[8..^16]` are stable across repeated reads).
- Multi dispatch picks the right candidate: instrumenting all three `hmac`
  multis shows the expected candidate, key length, message length, block size
  and `&hash.name` on every call.

So identical inputs and identical constants produce a different digest. The
divergence is inside the body, which leaves the `(state buf64 $w .= new)`
scratch buffer and the `my uint64 ($T1, $T2) = map *%2**64, ...` native
wrapping as the two candidates. Renaming `$w` in the `sha512` multi (to rule
out a name-keyed `state` collision with the `sha256` multi's own `$w`) does
**not** fix it; replacing `state` with `my` makes it strictly worse (20
failures instead of 8), which is expected for any correct implementation and
only confirms the buffer is load-bearing.

## Why it is deep

The failure needs a specific interleaving of closure and `state` contexts three
levels deep (`subtest` block → `hmac`'s `reduce` block → `sha512`'s nested
`reduce` block) and does not reduce to a small synthetic program: a hand-written
`state` inside a nested `reduce` inside a sub persists correctly across calls,
and a sub with a `BEGIN`-defaulted named parameter binds correctly when the
named is supplied and then omitted. Finding it needs the debugger on the real
repro (`rust-gdb -batch` on the `state` slot write/read for the `$w` in
`sha512`), not another synthetic reduction.

## Related

`todo/deep/vendor-real-test-module.md` — the provider switch this blocks. The
other bundled-library regression found in the same sweep is a separate root
cause and is recorded there.
