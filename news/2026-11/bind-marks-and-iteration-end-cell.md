# Rebind markers after the source, and an IterationEnd held in a cell

Two gaps found with Sequence::Generator (its `t/01-basic` matches rakudo; the
`t/02-exhaustive` run now gets through the string sequences up to the
multiplicative ones).

- `$p := (my $q := $p).succ` emitted the rebind's marker ops before compiling
  the source, so the declaration inside the source consumed them; `$q` (an alias
  of `$p`) followed `$p` to its new value, which broke `UnendingSucc.pull-one`
  (`'a' ... *` came out as `f f f f f`). The markers are emitted after the
  source now.
- A `pull-one` that returned a bound attribute holding `IterationEnd` handed the
  consumer the cell, not the sentinel, so `Seq.head(8)` of a finite
  `.pred` sequence listed `IterationEnd` values. The consumer reads what the
  cell holds.
