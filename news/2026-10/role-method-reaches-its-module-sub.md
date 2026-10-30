# A role method can call a sub of the module that declares the role

A role declared in a module whose method calls one of that module's own subs
failed with `Unknown function` once the role was composed into a class of
another file. The bare-name package search for a frame running under `GLOBAL`
only added the routine's lexical package (`Bd::Rr`), not the packages that
enclose it (`Bd`), so the module's own subs were never searched. It now walks
the lexical package's ancestors in that case too. This unblocks MVC::Keayl's
`Broadcastable` role.
