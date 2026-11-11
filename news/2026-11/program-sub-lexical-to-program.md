# A program's `sub item` no longer replaces the `item` a module calls

A routine declared in a script is lexical to the script, so a module's own
by-name call (`Test.rakumod`'s `_push_vars` calls `item`) keeps reaching the
core routine whatever the program declares. mutsu registered the script's
package-less `sub item` under the shared `GLOBAL::item` key the module's call
also reaches, so a test file declaring a helper named like a core routine broke
every `subtest` after it, and a block's `sub any` changed what a module's
`any(...)` meant (#17433).

A program routine named like a core routine is now recorded when it is
installed, and the ADR-11136 visibility gate hides it from code running in
another compunit; the program itself, the `EVAL`s it runs, and the callbacks it
hands to a module (blocks, code values, methods) still see it. Only core names
are recorded, so the name-keyed resolution caches keep serving every other
routine. EEL 0.3's `t/44-actions-dynamic-repeat.t` went from 38 to 55 of 56,
`t/28-index-runtime.t` from 10 to 24 of 25.
