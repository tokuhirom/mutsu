# Keep module strict pragmas lexical

Module mainlines execute in the importing interpreter, so a module containing
`use strict` previously left the interpreter-wide strict-mode flag enabled for
an otherwise lax caller. Module loading now restores the caller's strict mode
after the module mainline finishes, including when its execution returns an
error. A regression test covers both lax and strict importing scopes.
