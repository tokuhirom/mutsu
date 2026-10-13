# A sub call with an `@x` parameter costs 2x a `$x` call, down from 3.4x

`sub g(@x) { 1 }` called as `g($l)` cost 9,882 instructions per call, against 2,944 for
`sub g($x) { 1 }` with `g($v)` (release build, callgrind). Plain `@x`/`%x` parameters
already took the positional-light bind (#12917), but that bind did several writes on
every call that nothing read (#12526):

- It mirrored the parameter into the callee env on every call. It now mirrors only
  when the body reads the name, as for a scalar parameter. `compute_needs_env_sync`
  now counts the ops that resolve a `@`/`%` local by name: a method call on it
  (`@x.push`) and the named element stores.
- It recorded the parameter's `.VAR.name` source. Only `.VAR` reads that record, so
  it is now written only when the body or a nested closure names `VAR`
  (`CompiledCode::names_var_method`).
- It recorded the binding for thread spawns. Only a block the frame made can spawn
  a thread, so a body with no closure skips the record.
- It registered an untyped container's (empty) type constraint even when no
  `__mutsu_type::` record of that name existed to shadow.

Two general costs of light calls went too. A routine body that never names `$_` no
longer unmarks (and replays) a caller `for` loop's readonly topic. A call site with
argument sources but no `|` argument no longer allocates its slip-position vector.
Which parameters are plain aggregates is now a bitmask computed when the routine is
registered, instead of being re-derived on every call.

`g($l)` now costs 5,883 instructions, 1.998x `g($v)`; `g(@a)` went from 10,135 to 6,133.
