# An unreached `my` reads as undefined, and a missing required sub-signature leaf fails its bind

A `CATCH` after a `die` that precedes a `my` died with `Variable '$x' is not
declared` when it read the variable. A Nil local slot is a declared variable --
the compiler resolved the name to that slot -- so the read no longer probes the
env for the name (#14325). The probe had stood in for one case: a required named
leaf of a sub-signature (`:at-rule(%)! (:at-keyw($type)!)`) that the argument
lacked was left unbound and the later read died. The sub-signature binder now
raises `Required named argument 'at-keyw' not passed in sub-signature` itself.
