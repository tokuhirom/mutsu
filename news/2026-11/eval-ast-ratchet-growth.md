# 1169 more t/ files pass as `EVAL(slurp($file).AST)`

A re-measurement of the `t/` files not on `ci/rakuast-ast-eval-passing.txt`
found 1163 that already pass when run through the RakuAST round trip. Each was
checked twice before joining the list. Six more pass after fixes to the
lowering of a tree that `EVAL` runs:

- `take(1, 2)`, `take-rw(1, 2)` and `fail("a", "b")` keep every argument
  (they kept only the first).
- `temp &f.wrap(...)`, `temp` over a call on no container, raises
  `X::Localizer::NoContainer` instead of refusing the tree.
- The declarator docs of an `enum`, a `subset`, an anonymous sub and a
  parameter (`sub f($a, #= doc`) reach `.WHY` and `$=pod`, filed under the
  parser's keys.

Of the 71 files that still fail, 40 fail the same way on rakudo (most of them
`use v6.e.PREVIEW`, which rakudo refuses inside an `EVAL`), so they can never
join the list (#16356).
