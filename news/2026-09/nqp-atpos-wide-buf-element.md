# `nqp::atpos_i` on a wide Buf returns the whole element

`nqp::atpos_i` / `nqp::atpos_n` on a Buf wider than one byte returned only the
element's low byte: `nqp::atpos_i(buf32.new(258), 0)` gave `2` where Rakudo
gives `258`, and a signed `Buf[int8]` lost its sign (#9133).

The op's Buf branch read through `value_buf::with_buf_bytes`, whose byte view
collapses every width-w element to its first (low) byte — and, for a wide
buffer, built that view by copying the whole buffer on every call. It now uses
`value_buf::buf_elem_at`, which decodes the single requested element at the
buffer's own width and signedness in O(1), so the fix is also a complexity fix
for wide buffers.

Pinned by `t/vm/nqp-atpos-wide-buf.t` (buf16/buf32/buf64, signed int8/int32,
buf8, out-of-range index).
