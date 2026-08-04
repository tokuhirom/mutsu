# A `buf64` element assignment saturated at `i64::MAX`

`Digest::SHA2`'s `sha512`/`sha384` returned the wrong digest. The constants, the
padding and every round primitive checked out against raku; a per-round trace of
the 80-round compression showed the message schedule going wrong at `t = 25`,
with `$w[25]` reading back as `7FFFFFFFFFFFFFFF` — `i64::MAX`, a saturation
marker, not a plausible SHA word.

The buffer write paths all mask an element to the node's own width on the way
in, and `elem_to_u64` deliberately routes a `BigInt` through `to_u64` first
precisely so that a `uint64` element above `i64::MAX` keeps its bits. But the
`$b[i] = v` and `$b[i, j] = v, w` VM paths converted the value *before* handing
it over:

    arr[pos] = Value::int(crate::runtime::to_int(&v));

`to_int` saturates a `BigInt` at `i64::MAX`, so by the time `encode_elems` ran
there was nothing left to mask — every `buf64` element at or above 2**63 became
`0x7FFF_FFFF_FFFF_FFFF`. The comment on those lines already said the masking was
`encode_elems`'s job; the pre-conversion was simply redundant *and* lossy. A
buffer could therefore disagree with itself depending on how it was filled:
`blob64.new(0xFFFF_FFFF_FFFF_FFFF)` was right, `$b[0] = 0xFFFF_FFFF_FFFF_FFFF`
was not, and `.splice` and `write-uint64` sat on the correct side too.

The fix stores the value unconverted and lets `encode_elems`/`elem_to_u64` do
the coercion and the width masking, which is what every other write path already
does. Narrow buffers are unaffected — a byte still wraps mod 256 — because the
masking never moved.

`Digest::SHA2` now produces the correct `sha384` and `sha512` digests for the
NIST vectors, so the `Digest` dist's `t/sha.t` passes its SHA-1 and SHA-2
subtests in full. Pinned by `t/buf-uint64-element-assign.t`.
