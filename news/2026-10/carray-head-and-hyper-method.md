# `CArray[T]`: `.head(n)` and `».method` walk the elements

`CArray[int32].new(1,2,3).head(2)` answered the CArray itself and `$a».Str` answered
`NativeCall::Types::CArray<N>`: the pure counted-`head` entry and the hyper method call each took
the buffer-backed positional mixin for one item. `head` now declines such a receiver so the
call routes through its `.list` like `grep`/`map`/`first`/`tail`, and the hyper call hypers over
the elements its `.list` reads (#12649).
