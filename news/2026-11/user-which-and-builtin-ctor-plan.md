# A user `method WHICH` is an ordinary method call; built-in constructor plans are kept

Two per-call costs Red's AST comparisons pay on every node:

- A user class's own `method WHICH` (or `method WHY`) was routed with the
  reflection macros (`WHAT`, `HOW`, `VAR`, ...) through the general
  `call_method_with_values` probe chain before the user method ran — about
  three times the cost of an ordinary user method call. In rakudo `WHICH` and
  `WHY` are ordinary methods, so a class that declares one now takes the
  user-method dispatch on both the `CallMethod` and `CallMethodMut` paths.
- `ValueObjAt.new`, `ObjAt.new` and the other built-in types with a native
  constructor are not registered classes, so their native-constructor plan
  was rebuilt on every `.new` (it is frozen only for registered classes). The
  plan of such a type is now kept, valid while the name stays unregistered and
  cleared with the registered plans.
