# An uncaught mainline exception is reported before the END phasers run

`END note "e"; die "y"` printed `e` and then the exception, where rakudo prints
the exception first and runs END afterwards. `main` now installs an uncaught-exception
reporter on the interpreter (`Interpreter::set_uncaught_reporter`), and `run` calls it
on a mainline failure just before `finish()` runs the END queue. The `Test` plan
diagnostics still come after the exception. Closes #11020.
