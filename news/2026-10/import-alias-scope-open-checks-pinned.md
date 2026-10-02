# ADR-0081's open import-alias checks are pinned by tests

ADR-0081 scopes a unit module's imported aliases to its own compilation unit.
Its §6 listed five less common paths to re-check, and none of them had a test
until now. `t/modules/module-import-alias-scope-paths.t` covers all five,
each checked against rakudo:

- a `require` from inside a method;
- an `EVAL` nested in an imported routine;
- a closure that libc's `qsort` calls back into through NativeCall;
- two modules that import the same short type name (`Thing`);
- a block-scoped `use`, repeated after the module is already loaded.

In each case the module's own routines see their imports, and nothing leaks
into the caller. mutsu already behaved correctly on all five, so the change
adds tests only (#9925).
