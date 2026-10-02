# RakuAST: hash composers and `%(…)` round-trip as hashes

`.AST` rendered a hash literal, `{a => 1}` or `%(a => 1)`, as a
`RakuAST::Block` whose body was the pairs. That was the shape older rakudos
gave the composer, but it is not what rakudo 2026.09 has, and lowering the
`Block` back gave a *closure*: under the `MUTSU_RAKUAST=1` round-trip mode
`my %h = %(a => 1, b => 0)` silently built a Block instead of a Hash. These
were the most common of the round-trip failures that run to completion with
a wrong answer rather than refusing.

The parser builds both spellings as one internal `Expr::Hash`, so it now
records which one the source wrote (`HashSpelling`), the way `unless` and
`with` keep their keyword. The boundary renders rakudo's nodes and lowers them
back to a Hash:

- `{a => 1, b => 2}` is `Circumfix::HashComposer` around the pair (or a comma
  list of pairs), with `.expression`; `{}` is an empty composer, rendered with
  rakudo's blank line;
- `%(a => 1)` is `Contextualizer::Hash` over a new `RakuAST::StatementSequence`
  (with `.target` and `.statements`).

A composer used as a regex subrule colonpair value (`<word(:x{ a => 1 })>`)
keeps working through the new node.

The round-trip ratchet grows from 1630 to 1789 of 5881 `t/` files. Pinned by
`t/rakuast/rakuast-hash-literal.t`, which passes under mutsu, raku and
`MUTSU_RAKUAST=1`.
