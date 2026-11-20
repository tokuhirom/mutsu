# `.DEPARSE`: written trait order, `< >` enum spacing, `constant :=`, `>>²` and `|<<`

More classes of `.DEPARSE` differences against rakudo 2026.11 (#14845):

- An `enum`'s and a `subset`'s traits keep the order they were written in:
  `enum O is export does A <...>` no longer reads back as `does A is export`,
  and `subset S of Int is export` no longer as `is export of Int`. The parser
  records where `of` and `is export` stood among the clauses
  (`TypeDeclSpans::order`).
- A `< >` enum body keeps its spacing (`enum Colors < Red Green Blue >`,
  `enum C <Red  Green>`), as it already did for an empty body.
- `constant X := EXPR` keeps its `Initializer::Bind` and deparses with `:=`.
- A hyper superscript power (`@a>>²`) is `MetaPostfix::Hyper(Postfix::Power)`
  and the hyper slip (`|<< <a b>`) is `MetaPrefix::Hyper(Prefix("|"))`; both
  used to read back as what the parser runs (`@a >>**>> 2`, `|<a b>`).
- `&.cb` / `&.cb(args)` (a callable attribute's accessor) is
  `Var::Attribute::Public("&.cb")` instead of the `self.cb` call it runs as.
