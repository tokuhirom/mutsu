# Resolve lexical callables in Promise chain callbacks

Promise `then`, `andthen`, and `orelse` callbacks now run through the VM callable path. A callback can therefore call a lexical `&run` or `&uc` that shadows a core routine, whether its source Promise was already resolved or settles later. The change removes a callback-only difference in callable resolution; the new regression covers both timing paths and passes under Rakudo.
