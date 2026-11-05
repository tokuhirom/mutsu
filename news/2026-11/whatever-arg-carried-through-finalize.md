# The `*` of a priming scope is carried through the executing finalization

A census of the constructs `finalize_unit` still converted to RakuAST and
lowered back over 700 `t/` files put `Expr::WhateverArg` (the `*` operand of
`* + 1`, `*.uc`, `@a[*-1]`) second, after barewords. `WhateverCode::Argument`
reads back as the same leaf and the priming scope is planted by the
`WhateverCurry` marker around it (carried since #16318), so the leaf is now
carried too; `finalization_is_the_ordinary_tree` pins sources with each form
(ADR-15760 slice 2). `.AST` and `EVAL($ast)` are unchanged.
