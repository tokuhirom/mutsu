# `substr-rw($t, ...) = v` writes into `$t`, not an equal string elsewhere

With `use Test` loaded, `substr-rw($t, 1, 1) = 'X'` inside a block left `$t`
unchanged and rewrote an outer `$s` that merely held an equal string.

The sub form reaches the runtime as
`__mutsu_assign_named_sub_lvalue("substr-rw", [ARGS], v)`, so its first
argument arrives only as a *value*. To write the result back, the runtime
scanned the env for the first variable whose value was `values_identical` to
it. For value-typed strings that is any variable holding an equal string, so
the write went to whichever came first in the env. That is why the repro needed
extra lexicals in scope, and why it did not reproduce under `-e`.
`subbuf-rw` used the same scan.

The method form (`$t.substr-rw(...) = v`) never had the problem, because its
helper carries the receiver's variable name. The sub form now carries the name
too: when the first argument of `substr-rw` / `subbuf-rw` is written as a plain
scalar variable, the compiler (`named_sub_lvalue_with_target_var`) appends
that name as a fourth argument, and `assign_named_sub_lvalue_hinted` writes
back to it. The value scan remains only as the fallback for a first argument
that is not a plain variable.

The related bound-Proxy form (`my $r := substr-rw($s, 1, 1); $r = "Y"`) is a
separate gap, filed as #9200.

Pinned by `t/regex/subst/substr-rw-sub-form-target.t` (#9183).
