# A statement modifier after `f(...), g()` governs the whole list

`skip-rest("..."), exit with %*ENV<RED_DATABASE>;` (Red `t/14-union`) parsed as the
statement call `skip-rest("...")` followed by a separate `exit with ...`, so the
modifier no longer guarded the first item and the test ran its skip unconditionally.
A comma after a parenthesised known/imported call now marks the call as the start of a
list expression, which the expression-statement parser owns together with the modifier.
Red `t/14-union` now passes (18 planned, 18 run).
