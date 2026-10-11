# rotate, Any.splice, name/dynamic and AT-POS are method-table rows

`List`/`Array.rotate`, `Array`/`Hash` `.name` and `.dynamic` are `Narrow` rows over the value (the container descriptor travels with the
`Array`/`Hash`, so no binding name is needed), `Any.splice` is the `Mut` row that ends every owner chain, and `List`/`Array.AT-POS` are
interpreter rows (ADR-11276 §9.67, #12622).

Behaviour moves toward Rakudo: `splice` on anything but an `Array` or `Buf` (a `Hash`, a `Seq`, a type object, a `Blob`, a user instance) raises
`X::Multi::NoMatch` with Rakudo's message instead of `X::Method::NotFound`, and `@a.rotate(1, 2)` raises the too-many-positionals error.
