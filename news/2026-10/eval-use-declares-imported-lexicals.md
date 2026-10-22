# An EVAL's `use` declares what it imports as the EVAL's own lexicals

An `EVAL`'d unit's top-level `use` now declares the variables, constants and
types it imports as the EVAL's own lexicals, as a block's `use` already did
(ADR-8751, #14140). They used to reach the snippet as env aliases that the
EVAL's import scope rolled back afterwards; in a census over `t/` and the
whitelisted roast, these were most of the env aliases still written inside an
import scope.

- The EVAL's lexical cleanup counts the `use`'s bindings among the snippet's own
  `my`s. The pre-run of a top-level `use`, which runs before the CHECK-time
  checks, imports into the same lexical import set. Those checks read the
  imported names from the module's parse-time scan.
- A `my constant Name` at an EVAL's top level no longer leaks into later EVALs,
  and it no longer hides a same-named constant of the caller from them.
- A `sub EXPORT` hook's uppercase literal type keys (`'MyAlias' => Int`) are
  declared like its other literal keys. A `sub EXPORT` below a `unit module`
  line, which rakudo does not call, declares none.
- A hook's `'NativeCall' => NativeCall` binding still lets `::('NativeCall')`
  and `::('NativeCall::EXPORT::ALL')` resolve at the program's top level.
