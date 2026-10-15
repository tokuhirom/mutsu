# A closure's `EVAL` or `::('$x')` sees its own blocks' lexicals

Two more shapes of a closure looking a lexical up by string now match rakudo
(#13595). Both are written at program scope:

- **A sub declared in the closure's body.** In
  `do { my $s = 3; -> { sub inner { ::('$s') }; inner() } }`, `inner` finds
  `$s` instead of dying with "No such symbol". The closure is now compiled
  as naming lexicals by string, so it captures its whole creation scope.
- **A shadowing block lexical.** In
  `my $s = 1; my $k = do { my $s = 5; -> { ::('$s') } }`, calling `$k` gives
  5 instead of 1. The program-scope `$s` no longer wins over the block's
  own. The block's lexicals are shared with the closure as cells, so a
  later write in the block is seen too. A program-scope lexical is still
  read live.
