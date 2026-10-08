# Anonymous `regex { }` / `token { }` / `rule { }` cross the RakuAST boundary

`my $r = token { \d+ }` stopped 45 `t/` files at `.AST` ("literal Regex"):
the parser kept the term as a bare `Regex` value with its execution pattern
and no source tree, so the converter had nothing to render.

- The term now carries the declarator's source tree (with its kind), exactly
  as a named declaration does. The converter renders it as the named node
  without `name` (`RakuAST::TokenDeclaration.new(body => ...)`, plus a
  `signature` for `token ($x) { ... }`), and the lowering builds the same
  `Regex` value again.
- `RakuAST::RegexDeclaration.new` / `TokenDeclaration.new` /
  `RuleDeclaration.new` accept a missing `name` (the `name` accessor answers
  the `Name` type object, as in rakudo).
- The anonymous term used a copy of the braced-body scanner that trimmed the
  trailing whitespace; a `rule`'s last atom therefore lost its
  `WithWhitespace` wrapper. It now shares the scanner the named declarations
  use, and the duplicate is removed.

Known and left alone: rakudo keeps the `scope` accessor of a nameless
declaration at `anon`; here it still answers `has`.
