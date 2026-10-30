# A `&name` loop or pointy-block parameter is not replaced by a same-named module routine

`for @middlewares -> &middleware { &middleware($req, $res, ...) }` inside a module that also
declares `multi sub middleware` called the module's multi family instead of the loop's callable
whenever the callable came from the importing program (Humming-Bird's `Route.CALL-ME`: "Cannot
resolve caller middleware(...)"). The by-name `&name` read treated the env entry as a binding
inherited from another compunit. The compiler now records the `&name` reads it sees inside a
lexical `&name` binding (a `for`/`map` parameter, including the re-compiled `map`/`grep` body whose
parameters are bound in env), and the VM keeps the env binding for those reads. Humming-Bird's
`t/04-middleware`, `t/07-advice` and `t/09-routers` now match `raku`.
