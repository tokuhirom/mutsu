# RakuAST: an attribute's `handles` clause

"attribute with traits / smiley / scope" led the `.AST` refusals with 80
`t/` files. The refusal now names the part it could not render. That showed
`handles` as the largest part, with 28 files, ahead of custom traits (23),
`where` constraints (9) and `my`/`our` attributes (8).

Measured on rakudo 2026.09, `has $.x handles <a b>` carries a
`Trait::Handles` holding the written term: a `QuotedString`, a list,
`Term::Whatever` for `*`. mutsu's parser read the clause straight into
delegation specs (`HandleSpec::Name` per method, `Wildcard`, …), so the
spelling was gone. Each `HasDecl` now also keeps the written term of every
clause in `handles_terms`. A term is kept only when
`parser::handle_specs_from_term` reads it back into exactly the specs the
parser produced: a quoted name, a word or parenthesised list of names, or
`*`.

- The converter renders those terms as `Trait::Handles`.
- The lowering derives the specs from the lowered term through the same
  function, so the round trip delegates exactly as the parsed program does.

A rename pair (`:exposed<target>`, which shares its tree with
`exposed => 'target'`), a regex, a variable or a bare name is not kept, and
such an attribute stays refused. So does `handles` beside another trait,
since the parser does not keep the traits' source order. A multi-word list
renders as a comma list rather than rakudo's `<words val>` quote, as every
`<a b>` list still does.
