# A slang can add lexemes that open a pointy block (Slang::Lambda)

`Slang::Lambda` adds `λ` (and `→`, `↔`) to the grammar's `pointy-block-starter`
token. Two things stood in the way:

- `$*LANG.^name` was `Mutsu::Slang::CompLang`. Slangify picks the RakuAST roles
  with `$LANG.^name.starts-with('Raku::')` and fell back to the legacy `lambda`
  role, which mutsu cannot map. The language handle is now
  `Raku::Slang::CompLang`, like rakudo's `Raku::Grammar`.
- The override is now supported: a `pointy-block-starter` token written as an
  alternation of quoted literals registers those lexemes with the parser,
  which accepts them wherever a term-position `->` block is accepted.

`Slang::Lambda`'s `t/01-basic.rakutest` matches rakudo (1 of 1).
