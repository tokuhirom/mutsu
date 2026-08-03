# Four general fixes make grondilu's `Digest::MD5` produce correct digests

`Digest::MD5` from grondilu's `libdigest-raku` ran to completion after the
`for`-modifier placeholder fix (`news/2026-08/for-modifier-placeholder-scope.md`)
but produced a wrong digest — `md5("abc")` gave `fe2be2927d9087ecb52bcb1fedc50c16`
instead of `900150983cd24fb0d6963f7d28e17f72`. Reducing the module's expression
tree piece by piece against `raku` turned up four independent interpreter bugs,
none of them specific to MD5. All four are fixed; `t/md5.t` in that distribution
now passes in full, and `t/sha.t` is down to its documented `sha384`/`sha512`
blocker.

## 1. Byte-addressed `Buf` accessors ignored the element width

`read-int*` / `read-uint*` / `read-num*` and their `write-*` counterparts were
handed a *projection* of the buffer — one byte per element, taking each
element's low byte — rather than its real storage. On every width-1 buffer
(`Buf`, `Blob`, `utf8`, `buf8`) that projection is the storage, so the bug was
invisible there; on a `buf16`/`buf32`/`buf64` it was destructive. A write read
the wrong bytes, and committing the result back stored one element per byte, so
`buf32.new(0x80636261, 0x11223344).write-uint64(2, 24, LittleEndian)` flattened
the existing elements to `0x61, 0x44`.

Rakudo addresses the buffer's actual storage, and the offset those methods take
counts **elements**: the byte position is `offset * width`. So on a `buf32`
offset 1 is element 1, and on a `buf16` a four-byte write at offset 1 spans
elements 1 and 2. The growth rule is MoarVM's, which mixes the units — a write
past the end resizes the buffer to `offset + size` *elements*, so
`buf32.new(1,2).write-uint32(2, $v)` leaves six elements, not three. That reads
like an `MVMArray` slip, but it is observable and `Digest::MD5` runs into it, so
mutsu matches it rather than inventing a tidier rule.

`value_buf.rs` grew `buf_raw_bytes` / `set_buf_raw_bytes` / `make_buf_from_raw_bytes`
alongside the existing one-byte-per-element helpers, and the six write dispatch
sites and five read arms were moved onto them, with the offset scaled by
`buf_elem_width`. `apply_write_int` / `apply_write_num` take the width and share
one `write_byte_offset` helper. The `nqp::writeuint` path keeps width 1: its
`buf_bytes_mutate` carrier hands over one byte per element already, so its offset
is a plain byte offset.

Pinned by `t/buf-wide-read-write-int.t`.

## 2. `Xxx` / `Zxx` thunked the whole left list instead of each element

`xx` re-evaluates its left operand once per repetition. Under a cross or zip
meta-op that re-evaluation is *per element* of the left list: `($i++, 100) Xxx 3`
is `((0,1,2), (100,100,100))` — one `Seq` per left element — and leaves `$i` at
3. mutsu wrapped the entire left side in one thunk and repeated that, producing
the transposed `((0,100), (1,100), (2,100))`. `Zxx` had the same shape bug in a
subtler form: it re-ran the whole-list thunk `count` times per position and
picked element `i` out of each result, so `($v++, 5) Zxx (2,3)` printed the right
values but ran the side effect five times instead of two.

The compiler now rewrites a list-literal left operand into a list of
argument-less thunks, one per element (`Compiler::per_element_thunks`), and the
carriers repeat one element at a time: the new `__mutsu_cross_xx` (left element
outer, count inner) and a rewritten `__mutsu_zip_xx`. A left side that is *not* a
list literal — `$i++ Xxx 3`, `@a Xxx 3`, `(...).list Xxx 3` — is a single
already-evaluated value in Rakudo too, and still falls through to the ordinary
`MetaOp` path. `__mutsu_reverse_xx` keeps serving `Rxx`, whose left side really
is one whole expression.

This is what generated MD5's per-round message-index table,
`16 X[R%] flat ($++, 5*$++ + 1, 3*$++ + 5, 7*$++) Xxx 16`.

Pinned by `t/metaop-xx-per-element-thunk.t`.

## 3. `polymod` had no arbitrary-precision path

`polymod`'s exact path was capped at `i128`, so an invocant wider than that fell
through to an `f64` loop that cannot represent the number at all: a finite
divisor list produced zeros and an infinite one produced nothing.
`parse-base('900150983cd24fb0d6963f7d28e17f72', 16).polymod(256 xx *)` — how the
`Digest` test builds its expected `Blob` — returned an empty `Seq`, which is why
every MD5 comparison read `expected: 'Blob:0x<>'`.

Integer operands now decompose in `BigInt` arithmetic, ahead of the rational
path, in both the finite and the infinite-divisor forms. Non-integer invocants
keep the rational and float behaviour they had.

Pinned by `t/polymod-bigint.t`.

## 4. `.roll($n)` on a non-numeric `Range` always returned the start element

The `GenericRange` sampler enumerated and picked from the range only when the
start was numeric; anything else answered the start element. So
`("a".."z").roll(8).join` was `aaaaaaaa` every time — which made the `Digest`
test's "hash 100 random strings" subtest hash the same string a hundred times.
`.pick` was unaffected.

Any endpoint pair is now enumerated via `.succ` and sampled from, so a rolled
element keeps its type (`(1.1..3.1).roll` still yields `Rat`s). An unbounded end
(`1..*`, `'a'..Inf`) keeps answering the start element rather than trying to
reify the range — previously the numeric branch would have attempted exactly
that.

Pinned by `t/range-roll-non-numeric.t`.

## Not fixed here

`read-ubits` / `write-bits` now address the buffer's raw storage rather than the
low-byte projection, so a wide buffer at least survives a bit-write intact. They
still diverge from MoarVM on wide buffers, where a bit offset appears to select
whole elements (`buf32.new(0x11223344, 0x55667788).read-ubits(8, 8)` is
`0x55667788` in Rakudo). Width-1 buffers — every practical use — are unaffected.
Recorded in `todo/tickets/digest-dist-blockers.md`.
