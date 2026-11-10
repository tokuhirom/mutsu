# More `.DEPARSE` leftovers: `is rw` parameters, anonymous `@` / `%`, `:$!attr`

From a fresh 107-file sample of `t/` (`.AST.DEPARSE` of the whole file vs `raku`):

- `for @a -> $x is rw, $y { }` printed `<->`: the parser's block-wide rw flag is also set by a
  parameter written `is rw`; only a block with no such parameter is `default-rw`.
- A declared `my @` / `my %` / `my int @` is `VarDeclaration::Anonymous` (it printed the
  parser's `@__ANON_ARRAY__`), and `EVAL($ast)` lowers it back.
- `submethod BUILD(:$!x)` binds under the plain name `x` (`names => "x"`), so it deparses as
  `:x($!x)`.
