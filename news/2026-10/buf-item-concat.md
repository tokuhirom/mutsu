# `~` and `~=` accept an itemized Blob

`$blob.item ~ $other-blob` (and `~=`) died with `X::Buf::AsStr` because the Scalar
holder produced by `.item` hid the Blob from the byte-concatenation branch. The
holder is now peeled first, as rakudo does. Found via LWP::Simple, whose
`t/get-unsized.t` now passes.
