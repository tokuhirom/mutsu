# Remaining blockers for the `Digest` distribution

grondilu's `Digest` dist (`libdigest-raku`, Artistic-2.0) provides `Digest::MD5`,
`Digest::SHA1`, `Digest::SHA2`, `Digest::SHA3`, `Digest::RIPEMD` and `HMAC`, and
is the dependency of `Digest::HMAC:ver<1.0.7>:auth<zef:jjmerelo>`. Seven general
interpreter bugs found while running it were fixed (see
`news/2026-08/digest-dist-seven-fixes.md`), then four more that its roast
fallout exposed (`news/2026-08/digest-dist-followup-four-fixes.md`);
`Digest::SHA1` and `Digest::SHA2`'s `sha224`/`sha256` now produce correct
digests. Blocker 1 below is fixed
(`news/2026-08/for-modifier-placeholder-scope.md`); three remain, each an
independent general bug.

Reproduce with the vendored-in-zef-store copy:

    D=~/.zef/store/libdigest-raku/74E0CB00D9501F6422E8C95959D6C212224112F7
    timeout 300 ./target/debug/mutsu -I $D/lib $D/t/md5.t

## 1. A placeholder is invisible inside a `for` statement modifier — FIXED

Fixed by the `is_statement_modifier` field on `Stmt::For`; see
`news/2026-08/for-modifier-placeholder-scope.md`. `Digest::MD5` now runs to
completion, though it still produces a wrong digest (a further bug, not yet
reduced — `md5("abc")` gives `fe2be2927d9087ecb52bcb1fedc50c16` instead of
`900150983cd24fb0d6963f7d28e17f72`).

## 2. A WhateverCode cannot bind to a `@`-sigil parameter

`Digest::RIPEMD`:

    X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '@';
      expected Positional but got WhateverCode (WhateverCode.new)

from `rmd160`'s destructuring loop signature `-> [&f, $r, @K, $s] { … }` fed by a
list whose elements include `*`-derived WhateverCodes. Not yet reduced to a
minimal repro.

## 3. `sha512` / `sha384` return an empty digest

`sha512("abc")` returns eight zero bytes. The `√` FatRat operator and the
`blob64` initial-hash constants are correct (verified against raku), so the fault
is in the block pipeline: `blob64`, `state buf64 $w`, the `$H[]` zen slice, the
`(8*$data).polymod(256 xx 15).reverse` length encoding, or `map * mod 2**64` over
a Blob. `sha384` is `sha512` with a different initial hash, so it falls with it.

## 4. `HMAC`'s named-parameter multis dispatch as ambiguous

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
