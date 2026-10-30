# A same-named lexical package of another kind reports its own metaclass

`{ my module Foo {} } { my package Foo {} }` kept answering `ModuleHOW` for the second
`Foo.HOW`: the first declaration's cached `.HOW` was never dropped when `SetPackageKind`
recorded a different kind. Redeclaring a package name now invalidates the cached
metaobject, so Kind's `modules` and `packages` subtests pass (#15520).
