# Method calls on a parameterized role type object stop re-selecting the candidate

`R[Int].method`, or a method on the `$M` that `.^parameterize` returns, was
answered by working out afresh on every call which role candidate the
arguments select -- a clone of every candidate's `RoleDef` and a trial bind
of each candidate's type parameters against a cloned environment -- before
re-dispatching to the parameterization's pun class. The pun itself was
already kept; only the decision was not. Rakudo parameterizes once and keeps
the pun on the parameterization, so the call costs it nothing extra.

The decision is now remembered per role, type-object arguments and method,
and tagged with the registry write generation, so registering, composing or
withdrawing any type invalidates it. A value argument (`R[5]`) is still
selected afresh, since that can run a `where` clause. `R[Int].mt` drops from
~84k to ~57k instructions per call.

Also, the per-call "does a class-scoped accessor (`my $.x`) take this
method?" query walked the receiver's MRO on every method call on a type
object. The registry now records every name a class-level attribute was ever
declared under, and the query returns at once for any other name: a method
on a bare parametric role type object (`R.mt`) drops from ~43k to ~37k
instructions. Monad `t/14_concurrent_types.t` at one thread runs ~6% fewer
instructions (#17331).
