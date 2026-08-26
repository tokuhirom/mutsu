# `Blob`/`Buf.contents` implemented

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Language/experimental.rakudoc:32`), where the `use experimental :pack` example
happens to demonstrate the method: `pack("H*", "414243").contents` should print
`(65 66 67)`.

## Root cause

`.contents` is a plain `Blob` method — the buffer's elements as a `List`, the
same shape `.list` yields — and has nothing to do with the `:pack` pragma the
doc example pairs it with. mutsu simply never implemented it, so
`Buf.new(1,2,3).contents` died with `No such method 'contents' for invocant of
type 'Buf'`.

## Fix

Added a `"contents"` arm to the 0-arg native dispatch
(`src/builtins/methods_0arg/coercion.rs`), decoding the buffer's storage node
through `value_buf::buf_elems_or_empty` and wrapping the result as a `List`.
The arm is gated on `is_buf_or_blob_class`, so it decodes the whole `Buf`/`Blob`
family (`buf16`, `utf8`, `Blob[uint32]`, …) at each buffer's own element width,
and declines for every other receiver — which is what keeps a `contents`
*attribute* on an unrelated instance (`Pod::Block.contents`) resolving through
its own accessor. A matching `("Blob", "contents", …)` row went into
`native_method_row_table.rs` so `.^methods` enumeration and the dispatch
admission resolver see it too.

Verified against `raku` v2026.06: `(1, 2, 3)` and `(List)`, an empty `Buf` gives
`()`, and `buf16.new(300, 400).contents` gives `(300, 400)` rather than the
underlying bytes. Pinned by `t/buf-and-list-mutators.t`.
