# RakuAST type objects expose their model hierarchy

`RakuAST::*` type objects and nodes now report their registered namespace and
semantic hierarchy through `.^mro` and `.^parents`. Previously those operations
fell through to the generic runtime class registry, which knew only the concrete
package name followed by `Any` and `Mu`.

The model layer now supplies one linearized hierarchy used by ClassHOW. For
example, `RakuAST::IntLiteral.^mro` includes `IntLiteral`, `Term`, `Expression`,
`Node`, `Any`, and `Mu`; statement types retain their namespace parent before
`Node`. Parent queries, including `:local`, `:all`, and `:tree`, derive from the
same hierarchy, and concrete RakuAST node values use their node class rather
than the generic value representation.

Pinned by the expanded `t/rakuast-type-objects.t`.
