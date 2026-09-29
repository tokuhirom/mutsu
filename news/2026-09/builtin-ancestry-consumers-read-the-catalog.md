# Built-in type ancestry has one oracle for every consumer

ADR-0051 P2 is finished. Smartmatch's static matcher, `isa_check`/`does_check`,
signature `~~`, the compile-time default check, `are()` and the multi-dispatch
narrowness chains now answer built-in ancestry from the builtin type catalog.
Before this, each of them kept its own hand-written `Cool` allowlist, denylist or
chain. The new `builtins::builtin_type_ancestry` module provides the two queries
they share: `builtin_type_is_a` (a class in the catalog MRO, or a role the type
composes) and `builtin_type_narrowness_chain`, which puts each role right after
the class that introduced it.

Making every consumer ask the same table also fixed the places where they had
disagreed with Rakudo:

- `(1, 2).Seq.isa(Cool)`, `(1..3).isa(Cool)`, `Nil.isa(Cool)` and `/a/.isa(Block)`
  are now True, and `Code.isa(Block)` is now False;
- `multi f(Numeric $)` / `multi f(Real $)` now picks `Real` for an `Int`, `Rat`,
  `Bool` or `Instant` argument, as Rakudo does. `Pair` no longer has a `Cool`
  ancestor in narrowness ranking, and `Seq` is no longer ranked as `Positional`;
- `:(Seq $) ~~ :(Cool $)`, `:(Instant $) ~~ :(Numeric $)` and
  `:(Stash $) ~~ :(Associative $)` now hold, and `:(Junction $) ~~ :(Any $)` no
  longer does.

The same change closed a related dispatch leak (#9948). The receiver-blind
native cascades used to answer `Cool`-subtype-only methods for receivers that do
not have them: `$/.succ`, `5.lazy`, `"x".lazy`, `(1=>2).lazy`, `"10".base(2)`,
`(1+2i).polymod(2)` and `"abc".bytes` each returned a made-up value. They now
throw `X::Method::NotFound`, as in Rakudo. `Duration.new(3).polymod(2)` returns
`(1 1)` instead of `(0 0)`.
