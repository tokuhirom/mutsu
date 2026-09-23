# TRIR list ops read their list straight out of the slot (ADR-0116 D2.1-D2.3)

ADR-0116 measured that native lowering of TRIR could remove at most ~21% of a
JSON::Fast record, and reordered ADR-0112 Step 4 to shrink the op *bodies*
first. This slice does the first three of its D2 items.

## What changed

- **Operand-direct list ops (D2.1).** `nqp::elems`, `nqp::shift_i` and
  `nqp::push_i` whose list operand is a plain boxed slot now compile to
  `ElemsLocal(n)`, `ShiftILocal(n)` and `PushILocal(n)` instead of
  `LoadObj(n)` followed by `ElemsO` / `ShiftIO` / `PushIO`. The generic form
  cloned the slot's `Value` (a refcount increment) only to drop it again
  after one read. `unjsonify-string`'s per-character loop runs three of these
  pairs. A `push_i` only takes the direct form when computing the pushed
  value is straight-line code that cannot store into the slot, because the
  direct form reads the slot after the value rather than before it.
- **The backing array is borrowed (D2.2).** `with_nqp_backing_array` hands a
  plain array or a `Uni`'s codepoint array to its caller by reference.
  `nqp_backing_array` returned an owned clone of it on every `elems`,
  `shift`, `push` and `atpos`. An `IterationBuffer` still takes the owned
  path, since its storage may have to be vivified.
- **ASCII skips the normalizer (D2.3).** `nqp::strtocodes` and
  `nqp::strfromcodes` return an ASCII string unchanged in every normalization
  form. No ASCII codepoint decomposes or has a combining class, and no
  canonical composition has an all-ASCII source pair. Every string in the
  SPDX document is ASCII.

To keep files under 500 lines, the normalizer moved to
`runtime/nqp_normalize.rs`, the backing-array helpers and `push_elem` to
`runtime/nqp_backing.rs`, and TRIR's list-op arms to `trir/exec_list.rs`.

## Measured

`benchmarks/bench-json-fast-spdx.raku` (727 records, section time, release,
4-core container, 6 runs each): `main` 0.165-0.176 s, this change
0.116-0.121 s, about 1.43x. These are local numbers for choosing the next
step. The bench CI series is the one to quote.

## Pinned by

- `t/vm/codegen/adr0112-trir-list-ops.t`: a new `rotate` routine that
  consumes and refills a routine-local `Uni` through the direct forms. TRIR on,
  TRIR off and rakudo agree.
- `t/vm/nqp-text-unicode-ops.t`: NFKC and NFKD still reach the normalizer
  next to ASCII, an ASCII string round-trips unchanged, and `strfromcodes`
  still composes a non-ASCII sequence.

Found on the way, and filed as #9207: `my $f := $a; $a := [...]` re-binds
`$f` too.
