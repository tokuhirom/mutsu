# A `:D` smiley no longer makes `Any:D` narrower than `Any` in multi dispatch

mutsu ranked `multi f(Any:D $x)` ahead of `multi f(Any $x)` (and of an untyped
`$x`) because the smiley counted as a typed parameter. rakudo ties them: the call is
ambiguous, or, when a named parameter is in play, the required-named candidate
(then the one with more named parameters, then declaration order) wins. A
`where`-refined or `::T Any` candidate with a required named parameter such as
Deps' `register(::Type Any, :$name, Str() :$priority! where {...})` therefore
loses to `register(Any:D $value, ...)` in mutsu and wins in rakudo. `Mu` and `Any`
with any smiley no longer count as a typed parameter in the nominal tier;
`Mu` versus `Any:D`, `Int` versus `Any:D` and `Int:D` versus `Int:U` still rank by
type and definiteness as before.

Pinned by `t/routines/dispatch/multi-any-vs-any-d-tie.t`; the reduction from
issue #16081 now matches rakudo (`A1 B1 C1 D1`).
