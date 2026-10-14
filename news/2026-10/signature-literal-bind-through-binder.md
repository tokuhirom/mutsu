# `:(...) := capture` binds through the signature binder

A signature-literal bind now runs the right-hand side through the same
signature binder a call uses, in a scratch scope, and binds each variable to
what its parameter bound to. Every check a call makes therefore applies:
`@`/`%`/`&` parameter types (`:(Int @a) := \((1, "x"))`), `where` clauses,
definiteness smileys (`:(Int:D $a) := \(Int)`) and arity
(`:($a, $b) := \(1)`). A slurpy takes the rest of the positionals
(`:($a, *@tail) := (1, 2, 3)` used to die), and a renamed named parameter
(`:(:type($t))`) binds its inner variable. The per-name type check that only
covered typed `$` parameters is gone.
