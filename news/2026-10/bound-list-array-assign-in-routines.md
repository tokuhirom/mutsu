# `my @a = $l` flattens a `:=`-bound List inside a routine

`my $l := (1, 2, 3)` binds the List straight to the name, so it owns no Scalar
container and `my @a = $l` flattens it. mutsu got that right in the declaring
scope but kept the List as one item inside any sub, method or lambda that
closed over the variable (`sub f { my @a = $l; @a.elems }` answered 1, rakudo 3).

The decision is made at run time: `ItemizeVar` looks for a
`__mutsu_bound_decont::<name>` marker in the env, behind a sticky "some marker
exists" gate (`bound_decont_active`). The marker was visible in the callee's env,
but every call boundary saved and cleared the whole mark-context flag word, which
includes that gate, so the callee never probed for it. The gate now survives the
boundary in both directions (the callee inherits it, and a marker the callee
bound keeps the caller's gate on after the return); only the one-shot store marks
are isolated, as before.

`for $l` was already fixed at compile time by handing the container-less
bindings down to child compilers, so the two spellings now agree.
