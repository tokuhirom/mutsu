# Core role metaobjects and junction-valued type parameterization

Natively modelled core roles now answer like rakudo's: `Blob.^candidates` has one
candidate with a `ParametricRoleHOW`, `Blob[uint8]` / `Buf[T]` report a
`CurriedRoleHOW`, and `Blob[T].new.^roles` lists the curried role first.

`Kind[A | B]` (a Junction in `Type[...]` of a type with its own `^parameterize`)
now reaches the metamethod whole instead of being threaded. That needed
several fixes along the path: `proto method ...(Mu) {*}` and a `Junction:D`
parameter bind a Junction whole, `Metamodel::Primitives` methods and a
metamethod reached as `self.kind: |args` do not thread, rakudo's
multi-`ACCEPTS` threading quirk is reproduced, and a subset minted with a
free-text name (`Kind[any(A, B)]`) is type-checked by its registered
definition rather than taken apart as a parameterization. `Kind`'s
`t/01-typecheck.t` "roles" subtest passes. Part of #15349.
