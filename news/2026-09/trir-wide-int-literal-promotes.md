# TRIR: an integer literal wider than 32 bits is an `Int`, not a native `int`

TRIR lowered every integer literal to a native `int` operand, so native-int
arithmetic against a large literal wrapped at the int64 edge:

```raku
sub f(int $a) { $a + 9223372036854775807 }
say f(1);   # rakudo: 9223372036854775808 -- TRIR said -9223372036854775808
```

With `MUTSU_TRIR=off` the same routine promoted correctly, so the two execution
tiers disagreed. Measuring rakudo showed where the line is: a literal that fits
in signed 32 bits is a native operand (`$a + 2147483647` wraps), and a wider one
is an `Int` (`$a + 2147483648` promotes). TRIR's `compile_binary` now declines
`+`, `-` and `*` when either operand is such a wide literal, and the untyped
path's `Int` arithmetic answers. Comparisons cannot overflow, so they stay
native.

Pinned by `t/vm/codegen/adr0110-trir-wide-int-literal.t`, which runs
`t/fixtures/trir-wide-int-literal.raku` with TRIR on and off and checks both
against rakudo's transcript. Closes #9234.

While measuring, the reverse gap turned up in the untyped tier: it never wraps
native `int + int` (or `int + small literal`), where rakudo and TRIR do. That is
filed as #9270.
