# A redeclared lexical package name gets its own metaobject

`{ my enum Foo <A B> }` followed by `{ my module Foo { } }` (or `my package`)
left the enum's cached metaobject under the name, so `Foo.HOW` of the module
reported `EnumHOW` and a `Kind[Metamodel::ModuleHOW]` typecheck of it failed. A
`module`/`package` declaration now drops the HOW cached under its name, and an
enum declaration drops both the cached HOW and the name's `package`/`module`
kind. This takes Kind's `t/01-typecheck.t` from 5 to 7 of 10 subtests; the rest
is filed as #15569.
