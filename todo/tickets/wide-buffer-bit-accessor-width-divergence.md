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

## Update (2026-08-18): likely not worth chasing -- looks like leaked MoarVM memory layout, not a real semantic

Probed further against real `raku` to find the actual rule (`bytes/rust src/builtins/buf_bits.rs`
already implements a clean, self-consistent "raw byte slice, big-endian bit order" model for
width-1 buffers, which is correct):

```
$ raku -e 'my $b = buf32.new(0x11223344, 0x55667788);
for 0,1,4,7,8,15,16,31,32,33,40 -> $pos {
  for 1,4,8,16,32 -> $bits {
    next if $pos + $bits > 64;
    say "pos=$pos bits=$bits -> " ~ $b.read-ubits($pos, $bits).base(16);
  }
}'
pos=0 bits=1 -> 0
pos=0 bits=4 -> 4
pos=0 bits=8 -> 11223344
pos=0 bits=16 -> 1177777788
pos=0 bits=32 -> DIES: "Can only read 1..16 bits from position 0 in buffer '$b', you tried: 32"
```

`read-ubits(0, 8)` (asking for 8 bits) returns `0x11223344` — a 32-bit-magnitude number, not an
8-bit one. `read-ubits(0, 16)` returns `0x1177777788`, a ~40-bit number that isn't a clean
sub-range of either element either. And `bits=32` is rejected outright with a "1..16" range limit
that has no documented rationale. None of this is explainable as "bit offset into a flat byte
buffer, endian-adjusted" — it looks like MoarVM's `read_ubits` for a non-uint8 native-typed buffer
does raw, alignment-sensitive pointer arithmetic over its internal C representation and leaks
whatever bytes end up adjacent, rather than implementing a well-defined bit-addressing scheme. This
reads as leaked implementation-defined/undefined behavior (likely even non-portable across
MoarVM versions or host byte order), not a real Raku language semantic worth byte-for-byte
replication. Combined with zero roast coverage and zero real dist impact (per the "Status" section
above), this is a poor investment — recommend leaving unfixed unless a roast test or real dist
surfaces a concrete, well-defined case.
