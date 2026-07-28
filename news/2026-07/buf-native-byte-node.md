# `Buf`/`Blob` stops being a boxed `Int` per byte

A `Buf` was a `Value::Instance` whose one attribute held a `Value::Array` with
**one boxed `Value::Int` per element**. A megabyte buffer therefore cost a
million boxed `Value`s and a million GC edges to trace, the element type existed
only as a substring of the class name, and there was no contiguous memory
anywhere for a C function to be handed a pointer into.

It is a [`BufData`] node now: contiguous little-endian bytes, plus the element
width and signedness as data. This is
[ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
P2's representation change — the step the survey called "where the judgment is".

## What made it a contained change

The two preceding slices. [Step 1](buf-storage-accessor-chokepoint.md) routed all
~104 attribute touches through `src/value/value_buf.rs`; [step
2](buf-byte-and-width-accessors.md) added the byte and count accessors most of
those callers actually wanted, and collapsed four `cn.contains("16")` ladders
into one width probe. With both in place, swapping the representation is a
change to *that file* — the ~170 call sites across forty files are untouched.

What did change shape is the write side: **reads need no class name, but
construction does.** The node carries the element type, so every read — and
`with_buf_elems_mut`, which re-encodes at whatever width the buffer already
has — works from the node alone. The functions that *create* storage
(`buf_attrs`, `set_buf_elems`, `set_buf_bytes`, `store_buf_*`) now take the
class `Symbol`, because the name is where Raku keeps the element type and there
is nowhere else to read it from. That is 20-odd call sites, every one of which
had the class name in scope already.

## Three parity gaps closed on the way

None of these were the goal; all three are the same bug — the element type was
not data — seen from different sides.

| | raku | mutsu before | mutsu now |
| --- | --- | --- | --- |
| `Blob[int8].new(-1)[0]` | `-1` | `255` | `-1` |
| `Blob[int8].new(200)[0]` | `-56` | `200` | `-56` |
| `Buf[int16].new(-2)[0]` | `-2` | `65534` | `-2` |
| `buf64.new(0xFFFF_FFFF_FFFF_FFFF)[0]` | `18446744073709551615` | `-1` | `18446744073709551615` |

Signed elements are sign-extended from their own width on the way out; a
`uint64` element above `i64::MAX` decodes to a `BigInt` rather than wrapping
negative. Two display paths had to follow: `.gist` and `.raku` both matched
`ValueView::Int` and fell back to zeros, so `buf64`'s largest value gisted as
`0x<0000000000000000>` and `.raku`'d as `.new(0)`. The hex formatting — which
was duplicated in two files — is now one `elem_hex` in `value_buf`, formatting
from the element's unsigned bit pattern so a signed or oversized element prints
the bytes it actually occupies.

## Cost and shape

`BufData` is a **payload-only** GC node: it holds no `Value`s, so `Trace` is an
empty body, `drop_gc_edges` is a no-op, it can never take part in a cycle, and
ADR-0001's container type filter keeps paying nothing for it beyond the refcount
every `Gc` has. Both `Trace` methods are written out rather than defaulted, so
adding a `Value` field later shows up as an obvious omission.

Adding the variant turned out to reach exactly **six** exhaustive matches
(`.^name`, `value_type_name`, `isa`, `truthy`, `to_string_value`, serde), plus
`==` and `eqv`. It stays out of `Buf`'s Raku-visible surface entirely —
`Buf.new(1,2).^attributes` is empty, so nothing at the language level ever names
the storage attribute.

## Verification

The raku comparison script from step 2 — construction, gisting, `.raku`,
encode/decode, `.bytes`, indexed assignment, `push`, the `write-*int` and
`write-num` methods, `read-ubits`, `.Buf`/`.Blob` coercion, concatenation,
`eqv` — is byte-identical to `raku` apart from the already-recorded `write-ubits`
masking gap (`todo/tickets/buf-numeric-bitneg-and-write-ubits-mask.md`), which
this change does not touch. Six new unit tests pin the node's byte layout, the
signedness round trip, the `BigInt` decode, and that in-place mutation preserves
the element width.

## What is left of P2

The body. `Buf.REPR` still answers `P6opaque`; making it answer `VMArray`
requires the synthesised `MVMArrayB` block behind it in the same commit
(ADR-0015 §2.1's ordering rule), and that is what `NativeHelpers::Blob`'s
`BODY_OF` — and through it `DBIish`'s mysql driver — is waiting for. The node is
the storage that block will point at.
