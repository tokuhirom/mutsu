# Imports, declared operators and inherited methods, for `static`

For `static` 0.0.1 (#15610), measured against rakudo 2026.09:

- `$*CU` read from a module's scope (`static`'s `CHECK`) sees the routines the
  *importing* unit imported, and the unit's own routine names are the ones its
  tree is resolved against (not those of the last unit lowered): a call of an
  imported routine resolves to it, with its signature and return contract;
- an operator the unit declares (`sub infix:<>>`) resolves to that routine,
  not the setting's;
- a method object of a built-in type is one object per declaration, and a type
  inherits its parent's (`Array.^find_method('pick') ===
  List.^find_method('pick')`), which the analysis uses to recognise the CORE
  `pick`/`roll`.

With these, every file of `static` 0.0.1's suite but `t/15-containers-and-effects`
matches rakudo (0 `not ok`); `t/15` has 2 left, a negative literal subscript
(#15742).
