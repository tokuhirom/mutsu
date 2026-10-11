# Multi subs and multi methods rank through one type-distance walk

Multi-sub and multi-method dispatch already compared candidates with one
`MultiRank` relation (#12468), but each still computed the per-argument type
distance with its own walk, and the two had drifted apart: a slurpy that
swallowed an argument cost twice as much for a method, a coercion parameter
`Int()` ranked as `Any` for a method even when the argument already was an
`Int`, only a method knew that `Nil` is narrower under `Any` than under `Mu`,
and methods never counted typed positionals in the nominal tier.

Both now go through one walk (`src/runtime/multi_type_walk.rs`): the callers
differ only in the parameters they pass (a method drops its invocant), the
package a bare type name in the signature resolves from, and whether the call
site's literal-argument mask applies (#12776). The method side gets the typed
tier, and `MultiRank` gains rakudo's arity-mismatch rule: when two candidates
differ in both positional count and required positional count, the one
without a slurpy is narrower and no parameter types are compared, so
`multi method m(Any $x, Any $y)` beats `multi method m(Int $x, *@r)` for
`m(1, 2)` in either declaration order. An omitted optional positional no
longer counts toward the typed tier, so `m(Int $x)` beats
`m(Int $x, Int $y?)` for `m(1)` for methods as it already did for subs.
