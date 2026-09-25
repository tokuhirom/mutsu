# A `:=` rebind made inside a closure is seen by sibling closures

A closure can rebind an outer lexical (`my &c = { $a := $b }`). In Rakudo that
changes the outer binding, so the enclosing frame and every other closure over
`$a` see the new container. mutsu updated the enclosing frame but not a sibling
closure: `my &r = { $a }; c(); say r()` printed the old value (#9307).

The #9237 fix already gives a captured lexical a *binding cell* when the
declaring frame rebinds it after the capture. It did not cover this case, for
two reasons. The rebind sits in the closure's chunk, where `$a` is a free
variable with no slot to record. At run time the rebind is a by-name
`SetGlobal` on the closure's own env entry, which never touched the cell.

The compiler now records the target name of a `:=` that has no own slot
(`CompiledCode::rebound_free_names`). `compute_free_vars` folds those names up
through the nested closures (`free_var_rebinds`) until they reach the chunk
that declares the variable. That chunk marks the variable's slots as rebound,
so its captures get the binding cell. At run time `SetGlobal` notes the cell
before a rebind and moves the new binding into it afterwards
(`reseat_env_binding_cell`). The frame and every sibling closure then read the
new binding. A name bound earlier with `my $f := $a` still keeps the old
container (#9207).

The design note is ADR-0097 §14.1. The regression test is
`t/routines/closure/closure-rebind-seen-by-siblings.t`.
