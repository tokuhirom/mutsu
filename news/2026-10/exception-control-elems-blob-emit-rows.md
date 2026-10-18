# Exception's control methods, `Any.elems`, `Blob`'s class methods and `Mu.emit` are rows

Four more clusters of methods answered by name moved into the built-in method
table (ADR-11276 §9.163).

`Exception`'s `throw`, `rethrow`, `die`, `fail`, `resume` and `Failure`, and
`Failure`'s `rethrow`/`die`/`fail`, are interpreter rows reached by the
receiver's MRO, refused on a type object by their `:D` invocant. Two by-name
blocks of the slow path and three VM-side steps are gone. `.fail` on an
unhandled `Failure` now throws its exception, and `Exception.^can('Failure')`
is true, both as in Rakudo.

`Any.elems` answers every receiver the zero-argument cascade's last family did,
and `Array.default`/`Hash.default` reach typed, shaped and lazy containers, so
that family is deleted. A typed container's default is now its element type
(`my Int @a; @a.default` is `Int`).

`Blob`/`Buf` `allocate`, `encoding` and `chars` are rows (`Buf.new.allocate(2)`
is `X::Multi::NoMatch`, as in Rakudo), and `Mu.emit` is a row that shares the
`emit` routine's implementation.
