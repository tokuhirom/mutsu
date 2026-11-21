# RakuAST origins of angle-number argument lists and regex aliases

`.AST` now places, as rakudo 2026.11 does:

- the `ArgList` of a call whose arguments start or end with an
  angle-quoted number (`Int.new(<1+2i>.re)`, `Int.new(<1/2>)`). Rakudo places
  the number inside its brackets, so the list starts after the `<`, and a
  list the number ends runs past its `>`;
- a regex's callable assertion `<&wc>` with its callee, and the aliased
  assertions `<wc=&wc>` and `<x=.alpha>`, the inner assertion without
  brackets of its own (#15780).
