# `nqp::` list ops share one index rule, and `AT-POS`/`ASSIGN-POS` are `[]`/`[]=`

The positional `nqp::` ops (`atpos`, `atpos_i`/`_n`/`_s`, `bindpos` and its typed twins, `splice`,
and TRIR's typed `atpos_i`) each resolved their index separately. Every one of them either clamped
a negative index to 0 or read it as absent. `nqp::bindpos($l, -1, $v)` therefore overwrote the
*first* element, where MoarVM writes the last. `runtime::nqp_backing` now holds the single rule
(`resolve_index`: negative counts from the end, and an index before the start dies with
MoarVM's `Index out of bounds`), along with `elem_at`, `bind_elem` and `atpos_i`, which every one
of those ops calls. `bindpos_i`/`_n` also convert the value they store, as `bindpos_s` already did.

The Raku-level methods had drifted from the subscripts they are supposed to be:

- `@a.AT-POS(-1)` answered `Nil` where `@a[-1]` is an `X::OutOfRange`.
- `my Int @i; @i.AT-POS(5)` answered `Any` where `@i[5]` is `Int`.
- `"abc".AT-POS(1)` indexed a character, where the one-element-list rule makes it out of range.
- `ASSIGN-POS` rebuilt the whole array and re-bound it, so every gap it grew claimed to `:exists`.

`.AT-POS` on an Array/List or Str now runs the CORE `postcircumfix:<[ ]>` itself.
`ASSIGN-POS` shares `ArrayData::store_element` with the `[]=` opcode's fast lane, so it writes in
place and leaves a grown gap as a hole.

`t/vm/nqp-list-index-parity.t` pins 27 rakudo-measured rows (ADR-0118 §2.2).
