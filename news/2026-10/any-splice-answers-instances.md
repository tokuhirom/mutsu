# `Any.splice` answers instances of plain classes, Blobs and hash-backed classes

`Foo.new.splice(0, 1)`, `Blob.new(1, 2).splice(0, 1)` and `.splice` on an `is Hash` /
`is BagHash` instance raised `X::Method::NotFound`; Rakudo resolves them to the `Any`
proto and raises `X::Multi::NoMatch` ("Routine does not have any candidates"). The
`Any.splice` row now answers them. A new `InstanceBacking` helper names the native
storage behind an instance once, for the storage bridges and for the row (ADR-11276
§9.68, #12647).
