# `@_`/`%_` from an enclosing routine, and `is List` keeps its elements

JSON::Fast::Hyper's encoder is
`my multi sub to-json-hyper(@_, *%_) { @_.map({to-json $_, :!pretty, |%_}) }`.
In rakudo the block reads the sub's `%_`: a `@_`/`%_` in a block is a
placeholder only when no enclosing scope declares that name. mutsu always made
it the block's own `*%_` parameter, so the block lost its `$_` signature and
died with "Too many positionals passed", and a pointy block
(`-> $x { %_ }`) was rejected at parse time as overriding its signature. The
compiler now leaves a block's synthesized `*@_`/`*%_` out when an enclosing
compiled frame has that lexical, and the parser keeps a small stack of the
implicit slurpies the routines around it declare (each `@_`/`%_` in a
signature, plus a method's implicit `*%_`) so the pointy-block check skips
them. `sub f() { -> $x { %_ } }` and a method's `@_` still die as before.

`my @l is List = 1, @a, %h` stored `$[...]` and `${...}`: the initializer went
through the Array assignment, which wraps each element in a Scalar, and the
`is List` trait then only retagged that array. It now builds the List from the
raw initializer that `StashVarDeclInit` already captures for custom container
traits, so the elements are the Array and Hash themselves, as in rakudo.

With these, tests 1 and 2 of JSON::Fast::Hyper's `t/01-basic.rakutest` pass.
Test 3 stops on #11193: the module's own `to-json` call resolves to the
importer's same-named alias.
