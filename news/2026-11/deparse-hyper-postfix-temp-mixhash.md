# `.DEPARSE`: hyper postfix operators, `temp %h<k>`, `my %h is MixHash`

Three more `.DEPARSE` corpus mismatch classes now match rakudo (#14845):

- `@a>>++` / `@a>>--` (and `@a>>.++`) are `MetaPostfix::Hyper(Postfix)`, not a hyper
  method call named `postfix:<++>`; both directions are converted.
- `temp %h<a>` / `let %h<a> = 5` keep their angle-bracket subscript instead of
  deparsing as `%h{"a"}`.
- `my %h is MixHash` / `is Mix` write only the trait; the type the parser derives
  from it no longer shows up as `my MixHash %h`.
