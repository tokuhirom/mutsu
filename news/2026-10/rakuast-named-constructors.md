# RakuAST: the nodes `.AST` produces can be built with `.new`

A survey of every class rakudo 2026.09 gives its own `new` found 49 classes
that mutsu's `.AST` produces but that `.new` could not build. So the `.raku`
of a parsed tree holding one could not be `EVAL`ed back into a tree. Among
them: `Ternary`, `Statement::For` / `Given` / `When` / `Default` / `Loop` /
`Catch` / `Need` / `Import`, `FatArrow`, `Term::Self`, `Term::Reduce`,
`Stub::Die`, `Type::Subset`, `VarDeclaration::Constant` / `Term`,
`Call::MetaMethod` / `PrivateMethod` / `QuotedMethod` / `Term`,
`MetaInfix::Hyper`, `Assignment`, `Contextualizer::*`, `Nqp`, `Nqp::Const`
and `StatementSequence`.

They are constructible now, with rakudo's required arguments, its defaults
(`For`'s `mode => "serial"`, `Term::Reduce`'s `triangle => False`, no `scope`
on a `VarDeclaration::Term` declared `my`) and its `.raku` order
(`rakuast/named_ctor.rs`). The fix also covers three neighbours:

- `Block.new` keeps `implicit-topic` / `required-topic`;
- `Label.new("L")` takes its name positionally, as rakudo does and as `.raku`
  prints it;
- `Parameter.new` accepts a sigilless `ParameterTarget::Term`.

A parsed `for` / `given` / `loop` / stub / subset / `|c` signature now
round-trips through `EVAL($ast.raku)`. Found by the RakuAST construction
survey (#7564); part of #14926.
