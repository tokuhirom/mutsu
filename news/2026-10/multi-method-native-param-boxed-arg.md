# A method call prefers a boxed candidate to a native-parameter one

A method call boxes every argument, and rakudo tries the candidates a boxed argument fits before
falling back to one whose parameter is native (`int`, `uint`, `num`, `str`): `multi method m(int $v)`
against `multi method m(Int:D $v)` answers the `Int` one for `m(5)`. mutsu ranked the native
candidate narrower by its declared type, so it won, or made a larger candidate set (NativeCall's
`UIntTypedCArray.ASSIGN-POS`) report `Ambiguous call`. A native-parameter candidate is now a rival
only when no other candidate matched, so `my $c = CArray[uint64].new; $c[0] = 0xAAAAAAAAAAAAAAAA`
stores the value instead of dying with `Cannot unbox 64 bit wide bigint into native integer` (#14670).

A `uint64` field of a `CStruct` set from a value above `i64::MAX` (`$attr.set_value($s, 2**64 - 1)`)
kept only `i64::MAX`; its low 64 bits are now stored.
