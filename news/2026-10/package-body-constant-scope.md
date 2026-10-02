# A package body's constants leave scope with the body

An exported constant of an inline module, read after `import`, gave `Nil`
whenever a user operator was registered before the mainline compiled (#10558):

```raku
module M8 { constant k8 is export = 8; }
import M8;
say k8;                                   # was Nil, now 8
module M3 { our sub infix:<foo>($a, $b) { "$a$b" } }
```

The operator turns compile-time constant folding off for the unit, so `say k8`
no longer compiled to `LoadConst(8)`. It then read the local slot that
`module M8`'s body had allocated for `k8` in the mainline frame. `PackageScope`
restores that frame's locals when the body exits, so the slot read back `Nil`,
and the imported binding was never consulted.

A non-unit `package`/`module` body is now compiled as a lexical scope of its
own (`push_dynamic_scope_lexical` / `pop_dynamic_scope_lexical`, the same
bracketing a bare block gets). Its constants stop resolving to their local
slots, and stop being folded, once the body ends. Outside the body they are
reached by package name (`M8::k8`) or through an import, and constant folding
no longer changes the answer.
