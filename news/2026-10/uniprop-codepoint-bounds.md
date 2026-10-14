# `uniprop` bounds-checks integer codepoints

`uniprop` read an integer argument with `as u32` and never checked it, so
`(-1).uniprop` answered `Cn`, `0x110000.uniprop` answered `Cn` and
`uniprop(2**32 + 65)` wrapped around to `A` and answered `Lu`. A `BigInt` was
not even treated as a codepoint by the routine form: `uniprop(2**70)` looked up
the digit `1`.

Now a negative codepoint raises rakudo's
`Codepoint -1 (0x-1) is out of bounds in 'uniprop'`, a codepoint past
`0x10FFFF` answers each property's default (`""` for the general category,
`False` for a binary property, `Nil` for `Numeric_Value`, `<unassigned>` for
`Name`), and one of `2**31` or more answers `""`, as MoarVM does.

The `uniprop($x)` and `uniprop($x, $prop)` routines now call the method
handlers instead of keeping their own copy, so `uniprop(True)` reads codepoint
1 (`Cc`) like `True.uniprop` (#13173). The same codepoint problem in
`unival`, `univals` and `unimatch` is tracked in #13174.
