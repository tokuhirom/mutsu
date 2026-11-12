# Passing an array variable to a `$` parameter takes the light call path

`sub f($h) { ... }; f(@a)` took the general signature binder on every call.
That cost about 7 µs in a release build, against 0.2 µs on rakudo and 0.8 µs
for the same call to an `@h` parameter.

A gate (`call_shares_container_into_scalar_param`) sent every call that
passes an `@`/`%` variable to a plain `$` parameter away from the light
paths. Its premise was that those paths bind a detached copy, so a mutation
through the parameter would not reach the caller.

That is no longer true. A mutsu `Array`/`Hash` is mutated in place, so binding
the same value into the parameter's slot already shares the caller's
container, which is how the light paths have served `@`/`%` parameters since
#12917. With the gate removed:

- the whole `t/` suite gives the same results;
- a probe of the container semantics matches raku: `.push`, element stores,
  `my @b := @$n`, `.splice`, a closure or a nested call writing through the
  parameter, two parameters bound to one array, `is copy`, and the parameter
  itself staying read-only.

The gate is gone from every sub call path, TRIR's entry included. The method
call path keeps its own analogue for now.
