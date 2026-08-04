# Remaining blockers for the `Digest` distribution

grondilu's `Digest` dist (`libdigest-raku`, Artistic-2.0) provides `Digest::MD5`,
`Digest::SHA1`, `Digest::SHA2`, `Digest::SHA3`, `Digest::RIPEMD` and `HMAC`, and
is the dependency of `Digest::HMAC:ver<1.0.7>:auth<zef:jjmerelo>`. Seven general
interpreter bugs found while running it were fixed (see
`news/2026-08/digest-dist-seven-fixes.md`), then four more that its roast
fallout exposed (`news/2026-08/digest-dist-followup-four-fixes.md`), then four
more behind MD5's wrong digest (`news/2026-08/digest-md5-four-fixes.md`).
`Digest::MD5`, `Digest::SHA1`, all four `Digest::SHA2` digests and
`Digest::SHA3` now come out correct, and the dist's `t/md5.t` passes in full.
Blockers 1 (`news/2026-08/for-modifier-placeholder-scope.md`), 3
(`news/2026-08/buf-wide-element-assign-saturation.md`), 4
(`news/2026-08/named-params-do-not-narrow.md`) and 6
(`news/2026-08/multi-named-narrowness-declaration-order.md`,
`news/2026-08/samewith-inside-lazy-gather.md`,
`news/2026-08/digest-sha3-runs.md`) are fixed. What remains is blocker 2's
anonymous-`$`-state residue and the non-blocking wide-buffer bit accessors (5).

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

The anonymous-state half is FIXED
(`news/2026-08/anon-state-per-routine-call.md`): the output stage's
`map { $_[[^5].rotate(++$)] }` counter now resets per `rmd160` call, so
repeated calls with the same single-block input agree
(`rmd160("abc")` twice → `8eb208f7…` twice).

What remains is a SECOND, independent freeze that the rotate bug was masking:
the `start` blocks in the compression loop capture the reduce callback's
`@words` parameter frozen at its first binding — the known `@`/`%` shared-var
lane limitation, `todo/tickets/shared-var-lane-freezes-a-reused-array-name.md`
(minimal repro
`reduce -> $h, @words { $h + await start { [+] @words } }, 0, (1,2), (3,4)`
→ 6 instead of 10). Consequences: a multi-block message (>55 bytes) digests
wrongly even on the first call, and any later call in one process returns the
first call's digest regardless of input. Fixing that ticket is what takes
`t/ripemd.t` to a full pass (its `'a' x 1_000_000` vector is slow in a debug
build — use a release binary).

## 3. `sha512` / `sha384` return a wrong digest — FIXED

The symptom in the original report ("eight zero bytes") had already moved on by
the time this was picked up: `sha512("abc")` returned a full 64 bytes, just the
wrong ones. A per-round trace of the 80-round compression put the divergence at
`t = 25`, where the message schedule word read back as `7FFFFFFFFFFFFFFF` — an
`i64::MAX` saturation marker. `$w[$t] = ...` on a `state buf64 $w` ran the value
through `to_int` before storing it, which saturates a `BigInt`, so every schedule
word at or above 2**63 was clamped. Fixed by storing the element unconverted and
letting `encode_elems` do the width masking; see
`news/2026-08/buf-wide-element-assign-saturation.md`. `sha384`, `sha512` and the
rest of `t/sha.t`'s SHA-1/SHA-2 subtests now pass.

## 6. `Digest::SHA3` — FIXED

`sha3_256("abc")` returns `3a985da7…31532`, matching rakudo. Six general fixes:

- the named-only multi dispatch (`Keccak`'s two candidates differ only in an
  extra `:$outputByteLen`) —
  `news/2026-08/multi-named-narrowness-declaration-order.md`;
- `samewith` inside the lazy `gather` that `Keccak`'s wide candidate ends with —
  `news/2026-08/samewith-inside-lazy-gather.md`. Its "Unexpected named argument
  'delimitedSuffix' passed" face was the same bug: the dynamic dispatch stack
  named `sha3_256` (the routine doing the forcing) instead of `Keccak`, so the
  redispatch went to the wrong routine rather than failing;
- `@a[$x;$y] += 1` compiling to an unconditional `X::Assignment::RO` —
  `news/2026-08/multidim-subscript-compound-assign.md`;
- a `given` statement modifier stealing the modified statement's placeholders, a
  multi-dimensional subscript not being a list-assignment target, a sized buffer
  (`Buf[uint8]`) being invisible to the multi-dispatch type-distance table, and a
  Range subscript not being a slice on a `Buf` — `news/2026-08/digest-sha3-runs.md`.

Three smaller residues found while fixing the above, none on the `Digest`
critical path:

- A `with` statement modifier still hides its statement's placeholders:
  `sub w1 { "a=$^a topic=$_" with $^n }; w1(3, 4)` is `a=3 topic=4` in rakudo but
  `a=True topic=3` in mutsu. `with` desugars to
  `Given { is_statement_modifier, body: [DoStmt(If { cond: $_.defined, … })] }`,
  and the synthetic `If` is opaque to both the placeholder collector and the
  compiler's placeholder binding (which binds `$^a` to the condition value,
  hence `True`). The `given` form is fixed; making the synthetic `If`
  transparent needs a marker on it, since a genuine nested `if` block inside the
  modified statement must stay a scope.

- A gather created inside a module routine and forced from the *consumer's*
  top-level scope cannot resolve a module-private name:
  `Digest::SHA3::Keccak(...)` called directly from a script dies with
  `Unknown function: Keccak` when its `samewith` fires. Going through the
  exported `sha3_256` works, because the force then happens with the module's
  scope in view. The samewith context capture records only the routine NAME;
  making it carry the declaring package needs the package-qualified proto
  dispatch below to work first.
- `T::K8::Keccak(...)` — a package-qualified call to a module's `proto` from
  outside the module — reports `No matching candidates for proto sub:
  T::K8::Keccak` even though the identical unqualified call inside the module
  resolves. Independent of `samewith`; reproduced with a two-candidate proto
  and no `gather` at all.
- `say` swallows an exception raised while `.gist` forces a lazy `Seq`, which is
  what made the original `samewith` failure print an empty line instead of an
  error: `todo/tickets/say-swallows-an-exception-from-gist.md`.

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
produces the RFC 2202 vector. The residual declaration-order tie-break is fixed
too — `news/2026-08/multi-named-narrowness-declaration-order.md`.

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
