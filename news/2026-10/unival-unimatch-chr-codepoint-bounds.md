# `unival`, `unimatch` and `chr` bounds-check integer codepoints

Following `uniprop` (#13173), the other Unicode routines that read an `Int` as
a codepoint now check it the way rakudo does (#13174):

- a negative codepoint dies with `Codepoint -1 (0x-1) is out of bounds in
  'unival'` (or `'unimatch'`, `'chr'`), where `(-1).unival` was `Nil`,
  `unimatch(-1, "Lu")` was `False` and `chr(-1)` reported
  `0xFFFFFFFFFFFFFFFF`;
- `unival` dies for `2**31` and up (a `BigInt` included; `(2**70).unival`
  read the digit `1`), answers `Nil` past `0x10FFFF` and `NaN` for a
  surrogate;
- `unimatch` is `False` for `2**31` and up. It reads a surrogate as `Cs`, and
  a codepoint past `0x10FFFF` as `Cn` in `No_Block`, with an unassigned
  codepoint's other property values, so `0x110000.unimatch("Cn")` is now
  `True`;
- `chr` past `0x10FFFF` reports MoarVM's
  `chr codepoint 1114112 (0x110000) is out of bounds`.

`univals` now reads the string form of an `Int`, as rakudo's does:
`65.univals` is `(6 5)`, not the value of `A`.

The `unival`, `univals`, `unimatch` and `chr` routines now call their method
handlers instead of keeping their own copies, so `unimatch(True, "Cc")` reads
codepoint 1 like `True.unimatch("Cc")`, and passing `Nil` dies with
`Cannot resolve caller` as in rakudo.
