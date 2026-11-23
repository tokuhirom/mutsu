# An EVAL's compile error names the unit under `$*CWD`

rakudo compiles an `EVAL` string, and a `Str.AST` string, as a unit named
`EVAL_<N>` joined onto `$*CWD`. The unit's own `$?FILE` stays the bare
`EVAL_0`. A compile error in that unit reports the absolute name, both in its
`.filename` and in the uncaught report:

```
===SORRY!=== Error while compiling /home/me/EVAL_0
Two terms in a row
at /home/me/EVAL_0:2
```

mutsu reported the bare `EVAL_0`. It now uses the absolute name, as rakudo
does.
