# Remaining blockers for the `Digest` distribution

grondilu's `Digest` dist (`libdigest-raku`, Artistic-2.0) provides `Digest::MD5`,
`Digest::SHA1`, `Digest::SHA2`, `Digest::SHA3`, `Digest::RIPEMD` and `HMAC`, and
is the dependency of `Digest::HMAC:ver<1.0.7>:auth<zef:jjmerelo>`. Seven general
interpreter bugs found while running it were fixed (see
`news/2026-08/digest-dist-seven-fixes.md`), then four more that its roast
fallout exposed (`news/2026-08/digest-dist-followup-four-fixes.md`), then four
more behind MD5's wrong digest (`news/2026-08/digest-md5-four-fixes.md`).
`Digest::MD5`, `Digest::SHA1` and `Digest::SHA2`'s `sha224`/`sha256` now produce
correct digests, and the dist's `t/md5.t` passes in full. Blockers 1
(`news/2026-08/for-modifier-placeholder-scope.md`) and 4
(`news/2026-08/named-params-do-not-narrow.md`) are fixed; two remain, each an
independent general bug.

Reproduce with the vendored-in-zef-store copy:

    D=~/.zef/store/libdigest-raku/74E0CB00D9501F6422E8C95959D6C212224112F7
    timeout 300 ./target/debug/mutsu -I $D/lib $D/t/md5.t

## 1. A placeholder is invisible inside a `for` statement modifier — FIXED

Fixed by the `is_statement_modifier` field on `Stmt::For`; see
`news/2026-08/for-modifier-placeholder-scope.md`. The wrong digest that remained
after it turned out to be four more independent bugs — wide-`Buf` `write-uint*`
addressing, `Xxx` per-element thunking, `polymod` precision, and `.roll` on a
`Str` range — all fixed in `news/2026-08/digest-md5-four-fixes.md`. `t/md5.t`
passes in full.

## 2. `rmd160` is correct once per process — MOSTLY FIXED

The original symptom — a `WhateverCode` reaching an `@`-sigil parameter — was a
chain of six general bugs, all reduced and fixed:

- `news/2026-08/slurpy-single-argument-rule-and-friends.md`:
  `map`/`grep`/`Array.new` ignoring the slurpy single-argument rule (which fed
  the destructure the *first element* of a tuple instead of the tuple),
  `*.comb».parse-base(16)` not currying because a hyper method call was invisible
  to the Whatever machinery, and an `@` parameter in a sub-signature rejecting a
  `Seq`/`Range`.
- `news/2026-08/start-block-destructured-array-param.md`: a destructured `@`
  parameter frozen at the first spawn's value on the shared-var name lane (so the
  second `start` branch computed the wrong half of the round), and `|$blob32`
  slipping the buffer instead of its elements (so the digest render numified the
  whole Blob to 0 and emitted four zero bytes).

`rmd160` now returns the correct digest for every RFC vector — but only for the
**first** call in a process. Its output stage rotates the five hash words with
`map { $_[[^5].rotate(++$)] }`, and mutsu never resets an anonymous `$` state
variable when its enclosing routine is re-entered, so later calls in the same
process rotate by the wrong amount and return a correct-but-rotated digest. That
is an independent bug with its own minimal repro:
`todo/tickets/anonymous-state-var-not-reset-per-routine-call.md`. Fixing it should
take `t/ripemd.t` to a full pass (its `'a' x 1_000_000` vector is slow in a debug
build — use a release binary).

## 3. `sha512` / `sha384` return an empty digest

`sha512("abc")` returns eight zero bytes. The `√` FatRat operator and the
`blob64` initial-hash constants are correct (verified against raku), so the fault
is in the block pipeline: `blob64`, `state buf64 $w`, the `$H[]` zen slice, the
`(8*$data).polymod(256 xx 15).reverse` length encoding, or `map * mod 2**64` over
a Blob. `sha384` is `sha512` with a different initial hash, so it falls with it.
(The wide-`Buf` and `polymod` fixes of 2026-08-04 did not move this one; the rest
of `t/sha.t` — SHA-1, `sha224`, `sha256` — passes.)

## 5. `read-ubits` / `write-bits` on a wide buffer

Not a `Digest` blocker, found while fixing the byte-addressed accessors. The bit
accessors now index the buffer's raw storage, so a bit-write no longer destroys
a `buf16`/`buf32`/`buf64`'s element width, but they still diverge from MoarVM,
where a bit offset appears to select whole elements:

    buf32.new(0x11223344, 0x55667788).read-ubits(8, 8)   # raku: 0x55667788
    my $c = buf32.new(0, 0); $c.write-bits(8, 8, 0xAB)   # raku: (0, 0xAB)

Width-1 buffers — every practical use — are unaffected, which is why this was
left out of that fix.

## 4. `HMAC`'s named-parameter multis dispatch as ambiguous — FIXED

Fixed by excluding named parameters from narrowness; see
`news/2026-08/named-params-do-not-narrow.md`. `hmac(key => "Jefe", …)` now
produces the RFC 2202 vector. The residual declaration-order tie-break is
`todo/tickets/multi-tie-break-declaration-order.md`.

<details><summary>original report</summary>

    hmac key => "Jefe", msg => "…", hash => &sha1, block-size => 64
    # mutsu: Ambiguous call to 'hmac()'
    # raku:  effcdf6ae5eb2fa2d27416d5f184df9c259a7c79

`lib/HMAC.rakumod` declares

    multi hmac(Str :$key,     :$msg, :&hash, :$block-size) { samewith key => $key.encode, … }
    multi hmac(    :$key, Str :$msg, :&hash, :$block-size) { samewith :$key, msg => $msg.encode, … }
    multi hmac(Blob :$key is copy, Blob :$msg, :&hash, :$block-size) { … }

With both `:$key` and `:$msg` passed as `Str`, candidates 1 and 2 each constrain
one named parameter and leave the other untyped. Rakudo resolves this (it runs
candidate 1, then `samewith` reaches candidate 2, then 3); mutsu calls it
ambiguous. The narrowness comparison for *named* parameters is the thing to fix.

</details>

## Also relevant: `Digest::HMAC` itself

The jjmerelo `Digest::HMAC` module (23 lines) already runs correctly on mutsu —
RFC 2202 vectors match — as long as the `&hash` callback is not a `proto`. A
separate bug makes a `proto` bound to a `&`-parameter whose name collides with a
builtin resolve to the builtin instead:

    proto p1(|) {*}
    multi p1(Str $s) { "proto-ok" }
    sub call-hash(&hash) { hash("x") }   # falls through to the `hash` builtin
    sub call-cb(&cb)     { cb("x") }     # correct
    say call-hash(&p1);   # mutsu: "Odd number of elements found where hash initializer expected"

A `multi` or plain `sub` in the same position resolves correctly; only a `proto`
value is missed. This blocks the natural `hmac-hex($key, $msg, &md5)` spelling
with the bundled `OpenSSL::Digest` (whose `md5`/`sha1` are `my proto sub`s).
