# A method with container parameters is replayed from its call plan

The method-call plan (ADR-15737 D1) lets a repeated plain method call skip
resolution and the fast-path gate. It refused every method with a parameter
that may alias the caller's container -- sigilless `\x`, `is raw`, `is rw` --
because such a parameter binds by where the argument came from, which the
plan's key (argument types) does not record. So `method set(Mu $target is
raw, $v)` re-resolved and re-ran the whole gate on every call.

Such a plan now records that it has container parameters, and each replay
re-checks the call's argument sources with the same test the gate uses
(`method_container_args_light`) and promotes the caller's variables to their
shared cells (`promote_method_container_args`) before the fast binder runs.
An argument the fast binder cannot bind sends that call through the full
dispatch tail, exactly as before.

A call of `method ub(Mu $instance is raw, Mu $val is raw)` given `self` and a
variable went from 26,157 to 23,909 Ir (callgrind slope, profiling build).
AttrX::Mooish does not move: its methods of this shape are called on an
`Attribute` with a role mixed in, a receiver the plain-method lane (and so
the plan) does not serve yet. Stage 3 of #18267.
