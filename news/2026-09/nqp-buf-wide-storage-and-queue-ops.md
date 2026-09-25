# nqp:: Buf ops address raw storage on wide buffers, and shift/unshift are O(1)

Follow-up to #9132, closing #9191. Two shapes of `nqp::` buffer op were still
O(e) per call (e = buffer elements) where MoarVM is O(1).

**Byte ops on a wide buffer** (`buf16`, `buf32`, `Buf[int32]`, ...). The
`nqp::` byte ops went through a *byte view* that, for a width above 1,
projected every element to its low byte, edited that, and re-encoded the whole
buffer. That was not just slow but wrong: MoarVM's `read_buf`/`write_buf`
address the buffer's **raw storage**, placing the access `offset * width` bytes
in, bounds-checking `offset + size` against the element count, and growing to
`offset + size` elements. Measured against rakudo:

```raku
my $b := buf32.new;
nqp::writeuint($b, 0, 0x01020304, 4);   # uint16
say $b;   # rakudo: Buf[uint32]:0x<00000304 00000000>
          # mutsu before: Buf[uint32]:0x<00000004 00000003>
```

`writeuint`/`writeint`/`writenum` and `readuint`/`readint`/`readnum` now edit
and read the storage in place at any width, sharing the offset/growth rule the
`.write-uint32` family already had (`buf_write_int::write_byte_offset`, now
generic over a `Vec<u8>` or a live node's storage). `nqp::decode` decodes the
raw storage (`nqp::decode(buf16.new(0x6261), 'utf8')` is `ab`), `nqp::slice`
and `nqp::splice` work in whole elements (a `buf16` element spliced into a
`buf8` truncates, a `buf8` one into a `buf16` widens, as in MoarVM), and
`nqp::readfh` refuses a wide buffer with MoarVM's
`read_fhb requires a native array of uint8 or int8` before touching the
handle. The O(e) low-byte projection (`with_buf_bytes_mut`) is gone.

**Front-end queue ops on any Buf.** `BufData.bytes` is now a `BufBytes`: a
`Vec<u8>` plus a head offset, the way MoarVM's `VMArray` keeps `start` in its
body. `nqp::shift*` advances the head and compacts only once the dead prefix is
as long as the live bytes; `nqp::unshift` uses the dead prefix as front slack
and, when it runs out, rebuilds with slack as large as the buffer. Both are
O(1) amortized. The live bytes stay contiguous and `as_ptr` is element 0, so
ADR-0015's REPR body block (`start` stays 0, `any` points at element 0) is
unchanged.

Along the way `nqp::unshift_i` / `unshift_n` / `unshift_s` were added — rakudo
rejects `nqp::unshift` of a boxed value onto a native buffer, so the typed form
is the one real code uses on a Buf.

`scripts/nqp-complexity-check.sh` gains `writeuint (buf32)`,
`readuint (buf32)`, `slice (buf32)`, `splice (buf32 append)`,
`shift_i (buf8)` and `unshift_i (buf8)` cases. Pin:
`t/vm/nqp-buf-wide-and-queue-ops.t`.
