# Ranking a multi's candidates memoises "does this declared type conform to that one"

A multi with `c` candidates asks `nominal_type_conforms` once per ordered pair
while it ranks them: 159,200 asks for 201 candidates, each a general
`type_hierarchy_distance` walk (several env and registry lookups, about 9k
instructions). The first call of `D.f(C5.new)` on 200 `multi method f(Ci:D $a)`
plus `Mu:D` cost 697M instructions, and a second argument type paid the same
again although the relation between the candidates' *declared* types does not
depend on the arguments.

A ranking now opens a `ConformanceScope`. Inside it, two plain registered classes
(not a role, enum, builtin type, parametric or RakuAST name) are answered from
the class's ancestor set (MRO plus composed roles), provided the env leaves the
`sup` name alone (no alias, constant or mixin bound to it) and, across scopes,
only while the registry's content stamp is unchanged. The stamp is replaced on
every write through the registry's `DerefMut`, the only path to a
`&mut Registry`, so a memoised answer cannot outlive a change to a class, role,
enum or subset. Debug builds recompute each fast answer the general way and
assert the two agree.

Profiling build, callgrind, difference against zero iterations, second run:
first call 697M -> 37M instructions; a second argument type +693M -> +29M.
(The remaining ~9M of that are `declared_wins` and the pairwise loop itself.)
