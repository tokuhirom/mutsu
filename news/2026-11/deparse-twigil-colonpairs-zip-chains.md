# `.DEPARSE`: twigil colonpairs, `@<a>`, `$~MAIN`, `Z` chains, `.()` and `Str.raku` escapes

A `:$x` colonpair over any variable the grammar reads now keeps its
`RakuAST::ColonPair::Variable` shape: `:$=pod`, `:$?PACKAGE`, `:$^f`, `:@:f`,
`:$<fee>`, `:@<fie>`, `:%<foe>` and `:$~MAIN` used to read back as the
`key => value` pair they evaluate to, so `.DEPARSE` printed `pod => $=pod`.

`@<a>` and `%<a>` keep their sigil (`Var::NamedCapture(sigil => "@")`) instead
of reading back as `$<a>.list` / `$<a>.hash`, and `$~MAIN` (any sigil) is the
new `RakuAST::Var::Slang`, as on rakudo 2026.11. The colonpair `:@<fie>` /
`:%<foe>` also evaluates to the list / hash of the capture now, where it used
to hold the capture itself.

The same change closes more of the `.DEPARSE` corpus differences against
rakudo 2026.11:

- a chain of three or more `Z` operands inside parentheses or an argument list
  (`((1, 2) Z (3, 4) Z (5, 6)).List`, `1, 2 Z(&) 3, 4 Z(&) 5`) keeps its list
  infix instead of reading back as the `zip(...)` call the parser runs;
- `.(args)` is a `Term::TopicCall(Call::Term)` and deparses as `.()`, not `$_()`;
- an angle-subscript assignment written inside parentheses or as an argument
  (`(%h<a> = 5)`) keeps its `<a>` form;
- `GLOBALish` and `EXPORT` are `Term::Name`s, as every compunit declares them;
- `Str.raku` writes a grapheme led by a `Prepend` codepoint (`"\x[600]"`,
  `"a\x[600,62]"`) and a leading combining grapheme (`"\x[D01,D00]"`) as its
  codepoints, as rakudo does.
