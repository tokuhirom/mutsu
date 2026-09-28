# `Rakudo::Internals.INCLUDE` returns the `-I` list

`Rakudo::Internals.INCLUDE` now answers the running process's command-line
`-I` paths as a `List` of `Str`, read from `%*COMPILING<%?OPTIONS><I>` the way
Rakudo reads it. Test suites use it to re-exec `$*EXECUTABLE` with the same
include path. RakuDoc::Test::Files' `t/01-methods.rakutest` does exactly that,
and used to die on its first `run-test()` with "No such method 'INCLUDE'". It
now passes all 16 assertions, the same as rakudo.

The `Rakudo::Internals` type-object methods (`IS-WIN`, `IS-MACOS`, `INCLUDE`)
now share one helper, answered from the VM's native method path instead of
only from the interpreter's `call_method_with_values` fallback. `MUTSULIB`
entries no longer appear in `%*COMPILING<%?OPTIONS><I>`: like `RAKULIB`, it is
not a command-line option.
