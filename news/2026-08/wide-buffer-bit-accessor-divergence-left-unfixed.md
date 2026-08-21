# Wide-buffer `read-ubits`/`write-bits` offset divergence: confirmed leaked MoarVM behavior, left unfixed

Split out of the `Digest` dist bundling work (not a `Digest` blocker — the dist never
exercises this path), this was a narrow gap in `buf32`/`buf16`/`buf64`'s bit accessors.
The bit accessors were already fixed to index the buffer's raw byte storage, so a bit-write
no longer destroys a wide buffer's element width. What remained open: on a buffer whose
element width is greater than one byte, the bit *offset* itself diverges from MoarVM, where
the offset appears to select whole elements rather than raw bits:

```raku
buf32.new(0x11223344, 0x55667788).read-ubits(8, 8)   # raku: 0x55667788
my $c = buf32.new(0, 0); $c.write-bits(8, 8, 0xAB)   # raku: (0, 0xAB)
```

Width-1 buffers (`buf8`/`Blob`) — every practical use, including every `Digest` call site —
are unaffected by this gap.

## Investigation: this isn't a well-defined semantic to replicate

Probing further against real `raku` (mutsu's `read-ubits`/`read-bits`/`write-ubits`/
`write-bits` implementation already has a clean, self-consistent "raw byte slice,
big-endian bit order" model for width-1 buffers, which is correct) turned up behavior that
doesn't correspond to any coherent bit-addressing scheme at all:

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

`read-ubits(0, 8)` — asking for 8 bits — returns `0x11223344`, a full 32-bit magnitude, not
an 8-bit value. `read-ubits(0, 16)` returns `0x1177777788`, a roughly 40-bit number that
isn't a clean sub-range of either buffer element. And a 32-bit-wide read is rejected
outright with an undocumented "1..16 bits" limit. None of this is explainable as a bit
offset into a flat byte buffer with endian adjustment; it looks like MoarVM's
`read_ubits` implementation for a non-uint8 native-typed buffer does raw,
alignment-sensitive pointer arithmetic over its internal C representation and leaks
whatever bytes happen to be adjacent, rather than implementing any well-defined
bit-addressing scheme. That reading is consistent with the behavior being
non-portable across MoarVM versions or host byte order.

## Decision: leave unfixed

Combined with zero roast coverage and zero real-dist impact (every known `Digest` call
site only uses width-1 buffers, which already work correctly), replicating leaked,
implementation-defined pointer arithmetic byte-for-byte is a poor investment. This is left
unfixed unless a roast test or a real distribution surfaces a concrete, well-defined case
that needs it.

## Verification (2026-08-21)

Re-ran the same probe against current `raku` and confirmed the identical nonsensical
outputs and the same "1..16 bits" rejection reported above — the underlying MoarVM
behavior, and therefore the conclusion not to chase it, is unchanged.
