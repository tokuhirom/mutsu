# Multi ranking answers "role conforms to class?" from a cached ancestor set

Ranking multi candidates asks whether one declared type conforms to another
(`nominal_type_conforms`). A registered class answered from a memoised ancestor set
(`conformance_fast`); a registered *role* as the narrower side always took the general
`type_hierarchy_distance` walk, which also reads the env. Red declares its AST nodes as roles
(`Red::AST`, `Red::AST::Infix`, ...) compared against its classes (`Red::AST::Value`,
`Red::Column`), so ~6,700 of the ~8,000 general walks per `Bla.^create` were role-vs-class
questions with a fixed answer.

A role now has an ancestor set too: the names reachable through its role-parent graph, the roles
it composes, and its own MRO, computed once per registry stamp (the stamp moves on every
registry write, so it cannot go stale) behind the same env gate on the `sup` name. A parametric
`sup` (`R[Int]`, which the walk reduces to `R`) still takes the general walk. Debug builds
assert every fast answer against the general walk, and the full `t/` tree runs clean.

Measured on a real Red 0.2.7 model create (`use Red; model Bla {...}; Bla.^create(...) for ^N`,
callgrind slope N=5 -> 15, profiling build, warm): `nominal_type_conforms` 81.5M -> 21.8M Ir per
create; the whole create 720M -> ~685M Ir (the loop's slope varies about +-5% between runs).
