# `Buf`/`Blob` gains a byte view and one element-width probe

[ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
P2 step 1 made `src/value/value_buf.rs` the single place that knows how a
`Buf`/`Blob` stores its elements. It exposed that storage the only way the old
representation could: as `Vec<Value>`, one boxed `Int` per element. That is fine
for a caller that really wants elements, and wrong for the majority, which want
either a byte string or a count — and which therefore each open-coded their own
element→byte loop on top of the accessor.

This change adds the second half of the chokepoint, so the module now answers
three questions instead of one, and P2 step 2 (the contiguous native node) has
somewhere to land.

## What was actually there

Thirty-five call sites went through `with_buf_elems`. Counting what they did
with the slice:

- **seven** wanted only `items.len()`;
- **most of the rest** immediately mapped every element to a `u8` and collected,
  i.e. they wanted the buffer's bytes and paid a boxed intermediate to get them;
- a handful genuinely wanted elements.

The byte loops did not agree with each other. Three conventions were in the
tree — truncating `i as u8`, `.clamp(0, 255) as u8`, and one going through
`to_int` — and the survey
([`todo/deep/adr0015-p2-buf-storage-survey.md`](../../todo/deep/adr0015-p2-buf-storage-survey.md))
deliberately left them alone during step 1, because unifying them is a
behaviour change and step 1 was meant to preserve behaviour exactly.

Separately, the **element width** — the one piece of a `Buf`'s type that is not
in its data at all, only in its class name — was re-derived from that name by
four different `cn.contains("16")` ladders (`.bytes`, `.raku`, `.gist`, and the
constructor's mask), two of which tested the widths in a different order than
the other two.

## What it is now

`value_buf` grows a byte level next to the element level:
`elem_to_u8`, `buf_bytes`, `buf_bytes_or_empty`, `buf_bytes_in`,
`with_buf_bytes`, `set_buf_bytes`, `store_buf_bytes`, `make_buf_from_bytes`,
plus `buf_len` / `buf_len_or_zero` for the callers that only ever wanted a
count. `buf_elem_width` is the single width probe the four ladders collapsed
into.

Every byte-shaped and length-shaped caller now goes through them, which is
what removes 275 lines against 232 added across 25 files.

## Which convention won, and why it is safe

**Truncation** — the one Raku itself stores by. `Buf.new(300)` holds `0x2C` and
`Buf.new(-1)` holds `0xFF` in both mutsu and Rakudo, and every mutation path
(`.new`, `[i] =`, `push`, `append`, `unshift`, `splice`) was verified to mask on
the way in, in both implementations. So the stored elements are always already
in range for the buffer's width, and the three read-side conventions could only
ever disagree about a state that cannot be constructed — except for a *wider*
buffer, whose elements legitimately exceed a byte. There truncation yields the
element's low byte (`buf16.new(0x1170)` → `0x70`) and clamping yielded `0xFF`;
Rakudo agrees with neither, since `read-ubits` on a non-`uint8` buffer is not
a defined operation, so nothing is lost by picking the coherent one.

A 37-line smoke script comparing mutsu against `raku` across construction,
gisting, `.raku`, encode/decode, `.bytes`, indexed assignment, `push`,
`write-uint16`/`write-int32`/`write-num32`, `read-ubits`, `.Buf`/`.Blob`
coercion, concatenation and `eqv` produces byte-identical output apart from the
one already-recorded `write-ubits` masking gap
(`todo/tickets/buf-numeric-bitneg-and-write-ubits-mask.md`).

## What this sets up

Under the native node a width-1 buffer — every `Buf`, `Blob` and `utf8` — *is*
its bytes, so `with_buf_bytes` hands its storage straight to the caller and
`buf_len` is a field read. The element accessors become the encode/decode
boundary, and they now have far fewer callers to serve. `buf_elem_width` is the
one line that moves from reading a class name to reading the node.
