# A buffer has no `Cool` text methods

`Buf.new(1, 2, 3).contains(2)` answered `True`, and `starts-with`, `ends-with`, `index`, `rindex` and `substr-eq` answered too,
because the interpreter's by-name text dispatchers stringify any receiver. A `Blob` is not `Cool` in Rakudo, so each of these is
`X::Method::NotFound` there. The gate that already refused `Cool`-only methods on the `Any`/`Mu` type objects now refuses them
on a buffer (an instance or type object, `utf8`, a user class doing `Blob[uint8]`), unless the buffer's class declares the
method (or a `FALLBACK`); `chars` and `codes`, which `Blob` declares itself, still raise `X::Buf::AsStr`. `Cool` gained its
`substr-eq` row, so `Any.substr-eq` is `X::Method::NotFound` too (#13313).
