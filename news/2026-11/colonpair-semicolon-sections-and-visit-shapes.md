# `:s(1,2;3)` keeps its `;` sections; three RakuAST visit shapes

A colonpair's parenthesized value read `;` as `,`. As a result,
`f(:s(1,2;3))` passed the flat `(1, 2, 3)` where Rakudo passes
`((1, 2), 3)`, and `f(:s(1;))` passed `(1,)` where Rakudo passes `1`. The
colonpair parser now builds its sections with the same
`finalize_paren_sections` a parenthesized list uses. Each `;` section is one
element, and a lone section is not wrapped. The spelling is kept too:
`.AST` gives `:shape(2;2)` one statement per section in its `SemiList`, as
Rakudo does, placed.

Measured on rakudo 2026.11 (#18598):

- a coercion type as a term (`say Rat()`, `say Int(Str)`) places its base
  type and constraint;
- `Rat()` also visits the implicit `Type::Setting(Any)` constraint that its
  `.raku` leaves out;
- a `proto ... { * }` body (`OnlyStar`) visits an empty, unplaced
  `StatementList`.

The fourth difference #18598 lists, `also is rw` kept as a `Statement::Also`,
is still open.
