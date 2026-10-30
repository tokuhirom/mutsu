# More of the RakuAST surface `static`'s analysis reads

A second batch for `static` 0.0.1 (#15610), each measured against rakudo
2026.09:

- `VarDeclaration::Simple.container-type`, `SemiList.code-statements`,
  `is-resolved` on enum, subset and name terms;
- `compile-time-value` of a constant (its evaluated initializer), of a type
  capture (`Any`) and of an anonymous `sub`;
- `is-block-statement` and `Block.is-immediate-block-user-body` (1 for the body
  of a conditional or a loop), and `WhateverCode::Argument.name`;
- a unit routine's parameters are the routine's own `Parameter` objects, so the
  analysis can match `$node.meta-object` with the signature it dispatches over
  (`where` constraints become checkable);
- `is hidden-from-backtrace` removes a routine's frame from a backtrace's text,
  anonymous `sub`s included, so a `warn` raised from one names the visible
  caller, as rakudo does.

`static` 0.0.1 against rakudo: `t/01`, `t/13` and `t/14` match; the other twelve
files still differ (e.g. `my Int $x where ...` anonymous subsets).
