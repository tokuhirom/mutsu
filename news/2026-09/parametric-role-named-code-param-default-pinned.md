# Parameterized role named `&`-parameter defaults are pinned

Issue #9391 reported that a role declared as
`role R[::TYPE = Any, :&cmp = &infix:<cmp>]` left `&cmp` as `Nil` inside the
role body when composed as plain `does R`, which kept
`Concurrent::PriorityQueue` 0.0.2 from resolving its `inserts ..., :&cmp`
call into `Array::Sorted::Util`.

On re-investigation the bug no longer reproduces: the role-composition fix in
`3a182d74` ("preserve role callables and custom positional pushes") restored
the `&` alias of callable role parameters, and that also covers the defaulted
named form. Every shape checked now matches rakudo: the default bound with no
arguments, with only a positional type argument, overridden by an explicit
`:cmp(...)`, and on a punned role (`R.new`), including forwarding `:&cmp` to a
multi that requires it.

That commit's regression test only covered a positional `[&cmp]` parameter,
so the named-default shape is now pinned separately by
`t/oo/role/parametric-role-named-code-param-default.t`.
