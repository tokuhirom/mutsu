# `class ... does Iterator` in EVAL'd code gets the role's default methods

The builtin `Iterator` role's defaults (`push-exactly`, `push-all`, ...) are a
prelude injected into a unit that composes the role. An `EVAL` of a string held
in a variable is asked of the unit's tree, which only reported `Iterator` as a
type name, so the gate that keys on the spelling `does Iterator` stayed closed
and `class K does Iterator { ... }.new.push-exactly(...)` died with "No such
method". The tree's name view now spells a class's composed roles as
`does Role`, the way the source does (#16387).
