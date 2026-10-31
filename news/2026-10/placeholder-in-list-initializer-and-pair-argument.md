# Placeholders in `my (...) = EXPR`, and Pair arguments to placeholder subs

Two placeholder bugs behind `PDF`'s `t/io-serialize.t`, which sorts with
`sub obj-sort { my ($na, $ga) = @( $^a.value ); my ($nb, $gb) = @( $^b.value ); ... }`:

- A placeholder written only inside the initializer of a declarator list
  (`my ($x) = $^a`) was not collected: the walk over the parsed `SignatureDecl`
  node never visited its right-hand side, so the sub took no parameters
  (`sort requires a callable with at least 1 parameter`). The initializer now
  counts as part of the enclosing placeholder scope.
- A Pair held in a variable, passed to a sub whose parameters are positional
  placeholders (`sub f { $^x.key }; f($pair)`), was read as a named argument
  (`Too few positionals passed`). With no `$:name` placeholder to receive a
  colonpair, it is now an ordinary positional, as in rakudo.

`t/io-serialize.t` goes from 6 of 17 ok to 17 of 17, as `raku`.
