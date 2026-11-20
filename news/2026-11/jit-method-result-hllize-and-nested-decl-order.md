# JIT method calls HLL-ize their result; class loop-decl names are ordered

Two nondeterminism/consistency bugs behind the red nightly stress run (#16590):

- The JIT's `CallMethod` / `CallMethodMut` helpers and the TRIR method call did
  not HLL-ize the result the way the interpreter arm does, so under
  `MUTSU_JIT_THRESHOLD=2` a method returning a raw `VMHash` answered `BOOTHash`
  instead of `Hash` once the enclosing code was compiled
  (`t/routines/signature/array-param-binds-itemized-array-result.t`).
- `class_nested_decl_names` collected the names declared in a class body's loops
  from a `HashSet`, so the compiled class declaration plan differed per process
  and `MUTSU_PRECOMP_VERIFY` flagged a cached compile as different from a fresh
  one (Cro-HTTP, HTTP-HPACK and others). The names are now sorted.

Regression tests: `t/oo/method/jit-method-result-hllized.t` and a unit test for
the ordering.
