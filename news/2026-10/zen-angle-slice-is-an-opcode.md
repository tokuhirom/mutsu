# `$x<>` is the zen slice opcode, not a synthetic method

The parser turned an empty angle subscript (`$x<>`, `$x«»`) into a call of a
synthetic method, `__mutsu_zen_angle`, which the native method cascade answered
by name. It is now the same `Expr::ZenSlice` node `$x[]` produces and compiles to
the one `DeitemizeZen` opcode, so the pseudo-method and its cascade arms are gone
(ADR-11276 slice 5).

The two spellings now agree with each other and with Rakudo: `(1..3)<>` is the
Range (it was expanded to a List), and `$x[]` on an itemized Hash, Slip or
`$`-held Seq strips the item (`$%h[].raku` is `{:a(1)}`, not `${:a(1)}`).
