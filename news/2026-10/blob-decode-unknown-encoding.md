# Blob.decode rejects unknown encodings and buf16 defaults to utf-8

`Buf.new(1).decode('nonesuch')` now throws `X::AdHoc` ("Unknown string encoding: 'nonesuch'")
instead of answering a lossy decode, and `buf16`/`blob16` `.decode` without an encoding applies
`utf-8` to the buffer's bytes as Rakudo does (`utf16.decode` stays UTF-16). Closes #12341.
