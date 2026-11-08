# A saved REPL context keeps a scalar that is named like a caller's class

An `EVAL`'d unit's undeclared-variable check treated a bare `env` entry as "not a variable" whenever
a class of the same spelling existed, even when the entry held the variable's value. With
`class Out { }` in the program and `my $Out = []` in a unit saved through `$*CTXSAVE`, the next unit
died with `Variable '$Out' is not declared`. A bare entry that holds a real value now declares the
scalar whatever types exist; an entry that is the type object itself still does not. This is what
made Jupyter::Kernel's `t/02-sandbox.rakutest` lose `$Out`/`$In` from test 29 on. (#17087)
