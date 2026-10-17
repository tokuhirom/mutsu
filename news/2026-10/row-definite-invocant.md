# A built-in method row states its `:D` invocant

A method whose invocant Rakudo declares `:D` now says so on its row of the
built-in method table (`RowFlags::DEFINITE`, or `RowFlags::DEFINITE_MULTI` for a
`multi` whose every candidate is `:D`), and one refusal answers a type object
that calls it: `X::Parameter::InvalidConcreteness` or `X::Multi::NoMatch`, as in
Rakudo. Three by-name lists that did this before are gone: the numeric methods
of the zero-argument cascade, `IO::Path`'s list in the VM's entry gate, and the
two `Supply` lists of the interpreter's method call.

The answers moved toward Rakudo's: `Int.sqrt`, `Int.round` and `Int.chr` find
no candidate instead of naming a concreteness error, `IO::Path::Unix.e` and
`IO::Path.split` refuse the type object, and `Supply.map({ ... })` is
`X::Parameter::InvalidConcreteness` rather than a message of mutsu's own.

The same change moved the last methods the pure arity cascades answered by
name onto rows: `Attribute.DEPRECATED`, `Thread.yield`, `Kernel.hostname` and
`Kernel.endian` on the type object, `Any.nl-out`, `Cool.AST` with `:compunit`
or a slang (four copies before), `Exception.resume`, the `RakuAST` node
builders and `Pod::Block::Declarator`'s accumulators.
