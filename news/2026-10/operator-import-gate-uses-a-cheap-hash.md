# The operator-import gate on every call uses a cheap hash

Once any unit imports an operator (`use Test` does), every function call asks
`operator_has_import_scope_sym` whether the callee name has an import scope. That
table was a std `HashMap<Symbol, _>`, so each probe ran SipHash over a small integer
(~65 instructions), about 16 times per loop iteration of the `use Test` tax loop of #7565.
It is now an `FxHashMap`.

Measured (callgrind Ir slope between 200 and 1000 iterations of #7565's loop,
`MUTSU_JIT=off MUTSU_GC=off`, `profiling` build, both binaries built the same day): the
`use Test` tax went from 8,276 to 6,781 Ir/iteration (-18%); the import-free floor is
unchanged within layout noise (57.1k vs 57.4k). The tax is still 11.8%, against the
ticket's 3% goal, so this is one slice of #7565, not its close.
