# A user `sub` named like a builtin runs in an lvalue call

`sub get($x is rw) is rw { $x }; get($v) = 5` died with "Expected IO::Handle":
the named-sub lvalue assignment resolved the user's routine, then called it by
name through `call_function`, whose builtin arm for `get` (`IO::Handle.get`)
matched first. A name that is a builtin and resolves to a user routine now runs
the resolved definition itself (`call_routine_def`), as the plain call does.
