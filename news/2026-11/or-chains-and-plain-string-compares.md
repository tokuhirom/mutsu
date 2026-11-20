# `||` chains drop their cleanup ops, and `eq` borrows plain strings

`$a || $b` compiled to `Dup; JumpIfTrue; Pop; Pop; <b>; Jump; Pop`. The
`Dup` and the cleanup block were left over from a popping jump: `JumpIfTrue`
peeks, so a true left operand already stays on the stack as the value. The
chain is now `JumpIfTrue; Pop; <b>`, and `//` (with the peeking
`JumpIfNotNil`) and the `[||]`/`[//]` reductions get the same shape. A
five-way `$ch eq ' ' || $ch eq "\n" || ...` is 13 ops instead of 37.

The comparison itself was the larger cost. `eq`, `ne`, `lt` and the rest on
two plain strings viewed each operand (an Arc guard apiece) and then ran the
general leaf: a Buf probe per side, two `str_context_cow` conversions and,
for `ne`, a Bool round trip. They now borrow both strings and ask the shared
`str_prim::str_eq` / `str_order` directly, which is what the leaf computed
for two strings.

MCP::Server::Tool::Web's `is-space` (five `eq` in an `||` chain) went from
11,170 to 10,100 instructions per call, its `read-name` character loop from
13,010 to 12,500 per character, and html-to-text from 4.207G to 4.162G
instructions per 0.05 of the fixture (-1.1%; callgrind slopes, profiling
build).
