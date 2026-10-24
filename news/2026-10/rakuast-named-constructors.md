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

A roast-wide check of `EVAL($ast.raku).raku eq $ast.raku` exposed the
remaining gaps. Some classes had no named constructor yet: `Pragma` with
an argument, `ColonPair::Number`, `Substitution`, `Transliteration`,
`Call::NameAsMethod` / `TermAsMethod`, and `Name.from-identifier` with
`colonpairs`. Others had constructors that dropped an argument:
`Trait::Is`'s `type`, `Call::Method`'s `dispatch`, `Type::Enum`'s scope,
`Class`'s `repr`, `Parameter`'s `traits`. A proto's `{*}` body was refused.
`Parameter` now renders in rakudo's field order. On a sample of 182 roast
files the round trip went from 79 to 167 identical. The rest are files
`.AST` cannot parse yet, and one `EVAL` text-scan bug (#14931). Found by the RakuAST construction
survey (#7564); part of #14926.
