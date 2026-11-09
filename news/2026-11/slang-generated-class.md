# A class a slang production builds works in the importing unit

A slang production that builds a class with the metamodel
(`Metamodel::ClassHOW.new_type`, `add_method`, `add_role`, `compose`) and installs it
through `$*R.current-scope.merge-generated-lexical-declaration` now gives the importing
unit a working class. The slang host records the metamodel calls on the types a hook
minted, and the unit replays them in its own interpreter (ADR-13370), so `Tally.new`
and the methods, closures and roles the production gave it behave as in rakudo.
Qwiratry's `aggsub Tally[Int]($n) { ... }` is the motivating case.
