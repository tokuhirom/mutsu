# nqp:: Buf ops edit the storage in place instead of re-encoding the whole buffer

The `nqp::` ops that write to a `Buf` — `writeuint` / `writeint` / `writenum`,
`bindpos_i` / `bindpos_n`, `splice`, `readfh`, and the queue ops `push_i` /
`pop_i` — all went through `value_buf::with_buf_elems_mut`, which decodes every
element of the buffer into a boxed `Value`, runs the edit, and re-encodes the
lot. The read ops — `readuint` / `readint` / `readnum`, `slice`, `decode` —
copied the whole buffer into an owned `Vec<u8>` just to look at 1-8 bytes. So
every call was O(e) in the buffer's length, and filling or walking a buffer
with them was O(n²) (#9132). `CBOR::Simple`, a hard dependency of
`Log::Timeline` and hence of Cro::HTTP, is written almost entirely in these
ops.

A new child module, `src/value/value_buf/inplace.rs`, adds the missing
in-place layer to the buffer storage node:

- `with_buf_storage_mut` runs a closure over the node's raw bytes, writing
  through an unshared node (so a pointer C holds stays valid, as `put_bytes`
  already guaranteed) and forking a shared one (`.Buf` / `.Blob` re-tag one
  node under two names, and a write to one must not show through the other);
- `with_buf_bytes_mut` is the byte view the `nqp::` byte ops address — the
  storage itself for a width-1 buffer;
- `set_buf_elem`, `pop_buf_elem` and `shift_buf_elem` encode or decode a
  single element at the buffer's own width.

The reads now borrow the storage through the existing `with_buf_bytes`, and
`nqp::push` / `push_i` on a buffer reuse `extend_buf_elems`, which #7680 had
already made encode only the new element.

`scripts/nqp-complexity-check.sh` (release, 4-core container), ratio =
t(2N)/t(N), where ~2 is linear and ~4 quadratic:

| case | before (N=10000) | ratio before | after (N=40000) | ratio after |
|---|---:|---:|---:|---:|
| writeuint (append) | 1.1342 s | 3.87 | 0.0147 s | 1.84 |
| bindpos_i (buf8, append) | 0.7721 s | 4.00 | 0.0142 s | 1.81 |
| splice (buf8 append 1 byte) | 1.1157 s | 4.15 | 0.0271 s | 2.29 |
| readuint (walk) | 0.2079 s (N=80000) | 3.70 | 0.0122 s | 1.84 |
| slice (1-byte slices) | 0.2697 s (N=80000) | 3.18 | 0.0302 s | 1.98 |

The `atpos_i (buf32)` row in the issue had already been fixed by #9133 (it
reads one element via `buf_elem_at`). Two new cases, `push_i (buf16)` and
`pop_i (buf16)`, were added to the script and are linear.

Still O(e), and filed as #9191: the byte ops on a buffer wider than one byte
(which also address the low-byte projection rather than the raw storage, as
MoarVM does), and `shift` / `unshift` on any buffer, which memmove the
remaining bytes where MoarVM's `VMArray` keeps a start offset.

Pinned by `t/vm/nqp-buf-inplace-ops.t` (growth, alias visibility,
copy-on-write of a re-tagged buffer, element width, splicing a buffer into
itself), which also passes under Rakudo.
