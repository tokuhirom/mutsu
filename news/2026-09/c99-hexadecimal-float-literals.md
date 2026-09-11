# C99 hexadecimal float literals (`0x1.8p+1`)

mutsu now parses C99 hexadecimal floating-point literals — a hex mantissa with
an optional `.` fraction and a mandatory `p`/`P` binary exponent, yielding a
plain `Num`. This is the literal syntax rakudo tracks as
[rakudo/rakudo#6524](https://github.com/rakudo/rakudo/issues/6524) and that the
2026-09-07 roast re-vendor added eighteen subtests for in
`S02-literals/numeric.t`.

Before this, the number lexer recognised `0x` plus hex digits as an `Int` and
stopped, so `say 0x1.8p+1` lexed as a hex integer followed by a postfix method
call and died with `No such method '8p' for invocant of type 'Int'`. Because a
parse error aborts compilation, that single unparsable literal cost the whole
89-subtest file, not just the eighteen new subtests.

## The ambiguity, and what resolves it

After a `.` following hex digits, the lexer has to choose between a hexfloat
fraction and a method call on a hex integer — `0x1.abs` scans `ab` as perfectly
good hex digits. The `p`/`P` exponent is the only disambiguator, and it can
appear after a fraction (`0x1.8p+1`), directly after the integer part
(`0x1p-2`), or with no integer part at all (`0x.8p+1`).

The new `src/parser/primary/hexfloat.rs` therefore scans the whole candidate
literal speculatively and commits only once it has seen the `p`/`P` *and* at
least one exponent digit; anything short of that returns a non-fatal parse error
and the input falls through to the existing radix-integer path unchanged. A
fraction is taken only when the `.` is followed by a hex digit, which keeps
`0x1.p3` a method call too. `0x1.abs` still evaluates to `1`, and `0x1.8` — a
hex float with no binary exponent — is still a compile error.

## Correct rounding, in one step

Rust's `f64` has no hexfloat parser, so the conversion is ours. A naive
`mantissa * 2f64.powi(exp)` double-rounds and gets the interesting cases wrong,
which is exactly what the spec subtests probe. Instead the mantissa digits
(underscore separators stripped) become an exact `BigUint`, the fraction width
and the binary exponent collapse into a single scale, and the exact value
`m * 2^exp` is rounded **once** to the nearest `f64` with ties to even.

The rounding quantum is `max(leading_exponent - 52, -1074)`, which is what makes
the subnormal grid fall out of the same code path as the normal one: at
`0x1.8p-1074` the exact value sits midway between two subnormals and ties-to-even
picks `1e-323`, while `0x0.8p-1074` ties down to zero. Exponents are clamped
before any shifting, so `0x1p+9999` gives `Inf` and `0x1p-9999` gives `0e0`
without materialising anything large. Underscores are legal in both the mantissa
(`0xde_ad.be_efp+0`) and the exponent (`0x1p+1_0`).

With this in place all 89 subtests of the re-vendored
`roast/S02-literals/numeric.t` pass, including the eighteen new ones — a file
the local rakudo v2026.07 oracle still rejects outright with `===SORRY!===`.

Pinned by `t/lang/literal-hex-float.t` (29 assertions) and four unit tests in
the new module.
