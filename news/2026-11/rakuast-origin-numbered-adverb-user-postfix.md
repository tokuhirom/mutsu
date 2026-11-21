# RakuAST: origins of numbered adverbs, user postfixes on a Whatever, indirect name tails, one-line anonymous bodies

Three more `.origin` gaps closed (#15780):

- a numbered regex adverb (`s:2nd/b/Y/`, `m:2nd/a/`) places its
  `ColonPair::Number` over the digits and the key;
- a user-defined postfix applied to a Whatever (`(*!)(4)`) is viewed as a
  `WhateverCode::Argument` operand, as Rakudo curries it, so the tree is
  placed and evaluates the same;
- an indirect package name with a tail (`$::($u)::y7`) runs its `Name` from
  after the sigil to the end of the tail.

The statements of a one-line anonymous routine body
(`sub ($a, $b) { my $c = $a; $c + $b }`) carry no line marks, so a term
the parser kept no range for left them unplaced. The body's text is now
parsed again on its own and its ranges copied over
(`anon_body_origins::statements`).
