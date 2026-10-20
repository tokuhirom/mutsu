# A custom `EXPORTHOW::DECLARE` declarator is a term

`(model Foo { })`, `given model C is table<c> { } { ... }`, `my $x = model Bar { }` and the anonymous
spellings (`model :: is table<t> { }`, `monitor { }`) parse wherever `class` does, instead of only at
statement start. Found through the Red distribution (`t/57-experimental.rakutest` now passes;
`t/01-tdd.rakutest` and `t/23-metamodel-model.rakutest` now get past the parse error).
