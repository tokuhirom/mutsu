# Lvalue calls through an `&`-variable use that code object

`k($v) = 5`, where `&k` holds an `is rw` routine returned from `EVAL`
(`my &k = EVAL Q[sub h($x is rw) is rw { $x }; &h]`), used to re-resolve the
routine by its declared name `h`. That name is lexical to the EVAL unit, so the
assignment died with `Unknown call: h`; when the variable was itself named `&h`
the lookup led back to the same variable and overflowed the stack. The
assignment now runs the code object the call site names and reads its
rw-capability (`is rw`, `is raw`, `return-rw`) off that object's compiled
routine (#10965).

`++k($v)` / `k($v)--` through a lexical `&`-variable holding an `is rw` routine
were refused with "the parameter requires mutable arguments" even without
`EVAL`, because the increment path only consulted named routines. They now
step through the same code object.
