# RakuAST: declaration headers, shaped arrays and declaration traits

The head of a `class` / `grammar` / `module` / `package` / `enum` / `subset`
declaration now reads back as the node rakudo has (measured on rakudo 2026.09),
and the lowering rebuilds the parser's own expansion, so the round trip is the
parsed program:

- **Name adverbs.** `class A:ver<1.0>:auth<me> { }` is a `Class` whose `name` is
  `Name.from-identifier("A", colonpairs => (ColonPair::Value(key => "ver",
  value => QuotedString(<words val>, "1.0")), ...))`. The parser spells these as
  `__MUTSU_SET_META__` calls around the declaration; `ast::package_header`
  builds and recognises that wrapping for both directions.
- **`is export`** is a `Trait::Is(name => export)` (with `argument => (:tag, ...)`
  for tags) on a class, grammar, module, package, enum and subset (before a
  subset's `of`); the parser's export registration and the lexical class marker
  are recognised and rebuilt.
- **Scopes.** `my module` / `my package` / `my enum` / `my subset`, `unit
  module|class|package` (whose body is the rest of the unit), `augment class`
  and `our proto sub` carry a leading `scope`.
- **Relationship traits.** `trusts T` is a `Statement::Trusts`; `hides T` a
  `Trait::Hides`; `is hidden`, `is rw` and a grammar's `is Base` / `does R` are
  traits in the written order; an enum's and an augmented class's `does R` is a
  `Trait::Does`.
- **Custom traits** `class C is labelled(5)` and `my $x is marked(5)` are a
  `Trait::Is` by name with an argument, a type (`is SetHash`) a `Trait::Is` by
  type.
- **Shaped arrays.** `my @a[2;3] = ...` is a `VarDeclaration::Simple` with a
  `shape` (one statement per dimension) and the data as its initializer; the
  parser's `Array.new(shape => ..., data => ...)` expansion is shared through
  `ast::shaped_decl`.
- **A proto's return type** (`proto sub f(Str $s --> Int) {*}`) is its
  signature's `returns`.

New tests: `t/rakuast/rakuast-declaration-headers.t` and
`t/rakuast/rakuast-declaration-traits.t` (the tree part of both also runs under
`raku`). Slice S3 of #7564.
