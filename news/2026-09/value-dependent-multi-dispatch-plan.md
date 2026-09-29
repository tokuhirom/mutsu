# A `where`/subset multi no longer re-gathers its candidates on every call

A `multi` family with even one value-dependent candidate — a `where` clause, a
literal, a value constant, or a subset such as `UInt` — could not use the
per-argument-type winner cache, so every call re-ran the whole resolver: gather
the candidates from the registry under four key patterns, sort them, rank each
one, and only then try to bind. FiniteField's `multi infix:<*>(UInt $a, UInt $b)
{ callsame() mod $*modulus }` paid ~55,000 instructions of that per `*`.

The resolver now splits the work the way rakudo does. The gathered candidate
passes and their narrowness ranking depend only on the argument *types*, so they
are cached as a dispatch plan per type key (`src/runtime/multi_dispatch_plan.rs`);
per call, only the bind checks run, and those are the only place a `where` clause
or subset predicate is evaluated. A candidate whose rank itself reads the value
(`multi f(Inf)`, `multi f(NaN)`, a `constant` in a type position) has its rank
recomputed per call, and an imported operator's visibility (#9944) keys the plan
by the executing compunit.

Two further costs on the same path went away:

- A value-constant parameter (`multi infix:<*>(Int $n where ..., G)`) now checks
  the constant's type before comparing identities. It used to warm both sides'
  `WHICH` first, so every `Int * Int` that reached secp256k1's candidate ran the
  user `Point.WHICH` on `G` — two field inversions.
- Operator-visibility filtering runs once when the plan is built, not for every
  candidate of every pass on every call.

EC's `G.double` ×20 (issue #9967, 4-core container, release build): 2.50 s → 0.32 s,
against rakudo 2026.07's 0.044–0.084 s on the same machine.
