# Sized Blob and Buf checks use the buffer's type

`Blob[uint8]` and `Buf[uint8]` checks now inspect the buffer's nominal type
instead of accepting any buffer whose current elements fit `uint8`. This
rejects plain `Blob` and plain `Buf` where Rakudo does, while preserving plain
`Buf`'s `Blob[uint8]` role and sized encoding buffers.
