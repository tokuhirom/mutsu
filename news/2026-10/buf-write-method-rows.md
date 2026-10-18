# `Buf`'s `write-*` methods are one set of rows

`write-int*`, `write-uint*`, `write-num*`, `write-bits` and `write-ubits` had six separate
implementations spread over the VM fast paths and the interpreter's method entries. They are now
`Buf` rows sharing one byte transform (ADR-11276 §9.170), and every call form reaches the same
code: a type object, a variable, a sigilless binding, an array element and a temporary.

Several answers now match Rakudo:

- `buf8.write-bits` and `buf8.write-ubits` work on the type object.
- `write-*` on a `Blob`, `blob8` or `utf8` raises `X::Method::NotFound`.
- A wrong argument count gives Rakudo's bind error.
- A `Blob` refuses `push`, `append`, `unshift` and `prepend` with `X::Multi::NoMatch`.
- A `Blob` has no `pop`, `shift` or `reallocate` at all, so those raise `X::Method::NotFound`.
