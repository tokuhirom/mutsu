# An untyped routine parameter rejects `Mu` on every call path

`sub f($x) { }; f(Mu)` now dies with rakudo's "Type check failed in binding to
parameter '$x'; expected Any but got Mu (Mu)" (#10878). An untyped routine
parameter is implicitly `Any`; the general binder already checked that for a
positional, but the fast call paths skipped it: the positional-light and
typed-light sub paths classified the parameter as unconstrained, the TRIR entry
bound it unchecked, the method fast path only checked declared constraints, and
an untyped named `:$x` was not checked anywhere — not even by the general
binder. An explicit `Any $x` had the same gap on the light paths.

The parameter plan now has a `RequiresAny` class (`FastParamCheck::implicitly_any`
is the binder's rule: a routine's untyped `$x`/`\x`/`:$x`, never a block's,
which is implicitly `Mu`), checked with one value-shape match — only a type
object or a user-class object needs the type lookup. A bare `Mu` argument is
also no longer reported as a compile-time "will never work": `Mu` is a
supertype of every parameter type, so rakudo leaves it to the run-time binder.
