# RakuAST: keep the priming scope and the `END` number across the round trip

Under `MUTSU_RAKUAST=1` (ADR-10723, #7564) 161 `t/` files ran but behaved
differently from the ordinary frontend. Two of the causes were decisions the
parser makes that the converted tree could not say again:

- **Where a `*` is primed.** `(* + 1).WHAT` primes `* + 1`; rakudo's tree for it
  drops the parentheses of the postfix's operand, and lowering that tree primed
  the whole postfix chain (`WhateverCode.new`). The node an `Expr::WhateverCurry`
  wrapped now carries a hidden `thunk` field (rakudo marks the same decision on
  the node itself), and lowering puts the marker back. A hand-built tree has
  none and is still primed from its shape.
- **The source-order number of an `END`.** The main program installs every `END`
  up front by the number the parser gave it, so a lowered `END` without one never
  ran at all. The phaser node carries it as a hidden `end-index` field; a
  hand-built or `EVAL`'d tree still installs where execution reaches it.

Both fields are part of the model but not of the constructor form or the gist,
like the statement `origin`. 27 of the 161 files pass now (the WhateverCode and
`END` families); `t/rakuast/rakuast-parser-decisions-kept.t` pins the behaviour.
