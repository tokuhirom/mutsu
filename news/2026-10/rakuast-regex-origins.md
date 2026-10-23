# RakuAST regex nodes carry their origins

The nodes of a quoted regex (`/ a b+ /`, `rx/.../`, `m:i/.../`) and of a
`token`, `rule` or `regex` declaration had no origins: the regex source tree
(ADR-0088) keeps no positions. `.AST` now lays the converted nodes over the
regex text in source order and gives each the range Rakudo does:

- literals (an escaped punctuation character too), quotes, sequences,
  alternations, `[ ]` and `( )` groups, named captures (`$<n>=...`);
- quantified atoms with their quantifier (`*`, `+`, `?`, their `?` / `!` /
  `:` modifiers, `** 1..3`) and separator (`% ","`);
- backslash classes (`\d`), `.`, the `^` / `$` / `^^` / `$$` anchors,
  `<name>` / `<.name>` assertions and enumerated classes (`<[a..z]>`,
  `<-[x y]>`, `<[a]+[b]>`);
- a `WithWhitespace` runs over the whitespace after its atom, as Rakudo's
  does; a quoted regex's `:i`-style adverbs and a declaration's name are
  placed too.

Each node is checked against its spelling at the cursor; the first one that
is not spelled there, or whose kind is not read yet (code blocks, lookaround,
interpolations), ends the walk, and it and every node after it keep no
range. The regex literal a smartmatch reads directly (`$x ~~ /.../`) is now
noted as a term, so its `QuotedRegex` and the infix around it are placed, and
`visit-children` visits a quoted regex's adverbs before its body.
