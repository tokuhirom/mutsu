# `read-ubits` / `write-bits` bit offsets diverge from MoarVM on wide buffers

Split out of `todo/tickets/digest-dist-blockers.md` (2026-08-17) — found while bundling the
`Digest` dist ([docs/batteries/digest.md](../../docs/batteries/digest.md)); not a `Digest` blocker
(the dist does not exercise this path), a narrow general gap.

## Status

The bit accessors were fixed to index the buffer's raw byte storage, so a bit-write no longer
destroys a `buf16`/`buf32`/`buf64`'s element width. What remains: on a buffer whose element width
is greater than 1 byte, a bit *offset* diverges from MoarVM, where the offset appears to select
whole elements rather than raw bits:

```raku
buf32.new(0x11223344, 0x55667788).read-ubits(8, 8)   # raku: 0x55667788
my $c = buf32.new(0, 0); $c.write-bits(8, 8, 0xAB)   # raku: (0, 0xAB)
```

Width-1 buffers (`buf8`/`Blob`) — every practical use, including every `Digest` call site — are
unaffected, which is why this was never a bundling blocker.

## Affected files

Buffer bit-accessor implementation (`read-ubits`/`read-bits`/`write-ubits`/`write-bits` methods on
`Buf`/typed buffers) — search for where the existing byte-storage indexing fix landed and work out
the width-aware offset semantics MoarVM actually uses for a >1-byte element buffer.
