# RakuAST round trip: bound `where`, `:a<2/3>` and a lone slurpy `for` parameter

Three round-trip differences fixed:

- `my $b where { ... } := 7` lowers back with its `where` block (the binding
  expansion keeps the constraint, as the parser does) instead of being refused.
- `:a<2/3>` is quote-words, so its single word is an allomorph (`RatStr`);
  the lowering now builds it with the parser's `angle_word_value` rather than
  the `Rat` a lone `<2/3>` term gives.
- `for LIST -> *%h { }` keeps its lone slurpy parameter in `params` as the
  parser does, so the loop still takes zero positionals and dies.
