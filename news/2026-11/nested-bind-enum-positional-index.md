# A nested bind through an enum index binds the element the enum names

`@a[B][9] := $v`, with `B` a numeric enum value, bound `@a[9]` instead of
`@a[1][9]`. The nested-element store keys each subscript level by its text, and
`"B"` names no element of an Array, so the store fell through to the next
level's key. Positional levels now take the `Int` their index names first (an
enum's value, a `Bool`, a non-integer real), as the two-level store already
did for `Bool` and `Rat`.

Terminal::ANSIParser hit it with `@actions[$_][0x7F] := &ignore-byte` over its
`DecodeState` enum: the DEL byte inside a control sequence was never ignored
(`ESC [ DEL A` parsed as `CSI(27, 91, 127)` plus a stray `A`), and `@actions`
grew to 128 entries, so the next loop over it ran eight times too often. With
the fix `make-ansi-parser` x100 went from 2.67 s to 1.43 s (rakudo 0.38 s).
