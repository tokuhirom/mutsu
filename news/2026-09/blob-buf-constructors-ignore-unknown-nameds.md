# Blob and Buf constructors ignore unknown named arguments

`Blob.new`, `Buf.new`, their typed aliases, and `.new` called on an existing
buffer now leave unknown named arguments out of the byte sequence. Their shared
constructor treats call-site named arguments as part of the implicit named
slurpy, matching Rakudo while retaining positional byte values.
