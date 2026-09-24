# Native `int` parameters wrap on the untyped path too

```raku
sub n2(int $a) { $a + 1 }
say n2(9223372036854775807);   # rakudo and TRIR: -9223372036854775808
```

With `MUTSU_TRIR=off`, and in every routine TRIR declines, this printed
`9223372036854775808`. The untyped compiler already had a native wrapping
operation (`NativeIntArithmetic`) for `+`/`-`/`*` whose operands it knows are
native. But it knew that only for `my int $x` declarations, which record their
type in the compiler's `local_types`. A routine's `int $a` parameter was never
recorded, so `$a + 1` compiled to the generic `Add`, which promotes to a big
`Int`. `sub n4(int $a) { my int $x = $a + 1; $x }` then died with "Cannot unbox
64 bit wide bigint into native integer".

- Sub and routine-closure bodies now seed `local_types` with their
  native-integer parameters (`seed_native_int_param_types`). Only native
  integer types are seeded, because the compile-time literal checks read the
  same map.
- An integer literal counts as a native operand only while it fits in 32 bits.
  This is the rule #9234 gave TRIR: `$a + 2147483648` promotes in rakudo, so a
  wider literal must not select the wrapping operation.

`t/fixtures/trir-wide-int-literal.raku` gained the narrow rows, and
`t/vm/codegen/adr0110-trir-wide-int-literal.t` requires rakudo's transcript
with TRIR both on and off (#9270).
