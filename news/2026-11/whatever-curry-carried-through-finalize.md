# The WhateverCode priming marker is carried through the executing finalization

`Expr::WhateverCurry`, the marker the parser plants around a maximal `*` priming
scope, was converted to RakuAST and lowered back by `finalize_unit`. The view
marks the node the marker wraps with a hidden `thunk` field and the lowering puts
the marker back around the lowered body, so the round trip changes nothing. It is
now carried as the parser built it and its body is finalized where it stands
(ADR-15760 slice 2). `.AST` and `EVAL($ast)` are unchanged.
