# RakuAST blocks of expression terms and module names carry their origins

`.AST` now gives the following nodes the ranges Rakudo does:

- the `Blockoid` and `StatementList` of a bare `{ ... }` term, and the
  statements in them;
- the `Block` of `do`, `start`, `gather`, `try` and the other statement
  prefixes;
- the `Blockoid` of an anonymous `sub { ... }`;
- a pointy block's `Signature`, `Parameter`s and `Blockoid`
  (`-> Int $x, $y { ... }`), also as the block of a `whenever`;
- the module `Name` of `use`, `need` and `import`;
- the `Call::Method` of a method called on a parenthesized operand
  (`(1, 2).map({ ... })`) or with a colon argument list (`.map: -> $x { ... }`).

Each block is placed from the range of the term that holds it: it runs from
its `{`, read back from the source text, to the term's end. A block whose
text holds a `#` keeps its `Blockoid` but no `StatementList` range, because
a comment could start its first statement later than the text does: exact or
absent.
