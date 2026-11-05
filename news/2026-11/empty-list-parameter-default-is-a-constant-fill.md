# A `()` parameter default no longer takes the call off the light path

`sub f($a, $p = ()) { ... }` called without `$p` evaluated the default per call
through the general binder (`eval_param_default`'s topic and parameter-name
save/restore, ~26k instructions), because only literal scalars, `*` and `\()`
were constant fills. `()` is an immutable empty `List` (`push` dies with
`X::Immutable`, as in `raku`), so every call can share one. `[]` and `{}` are
mutable containers and still get a fresh one per call.

Callgrind slope, profiling build, warmed, (300 - 100) / 200 calls of
`f(1)`: 32,262 to 6,255 Ir per call (a literal `5` default is 6,085). Refs
#14854: the enum-constant default (`$p = pvmAuto`) is still ~35k.
