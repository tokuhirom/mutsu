# RakuAST origins: pointy signatures with typed or named parameters, word quotes

More nodes carry the `.origin` rakudo 2026.11 gives them (#15780):

- the parameters of a pointy block, a regex declaration or a `my (...)`
  signature with a non-simple type (`Int:D`, `Array[Int]`, `Str(Int)`) or a
  named parameter (`:$y`, `token word(:$e) { ... }`), which the signature
  reader gave up on;
- the segments of an interpolating word quote (`«a $x b»`, `<<a $x b>>`,
  `qqw/a $x b/`), and the value of `:k«a b»`;
- a method call on an angle-quoted number (`<1/2>.Rat`), which starts at the
  number inside the brackets, as rakudo's does.
