# A `my class` names itself inside its own body

A `my class` used to bind its own name only after its body had run. When
something in the body named the class, the name was resolved through the
package lookup instead. Two body features do this: trait arguments
(`is referencing(*.id, :model(FixtureTree))`) and attribute defaults.

The package lookup is a problem when the same `EVAL` or `EVALFILE` runs
several times from a method. An earlier run of that code has already left a
binding of the bare name to its own class, so the lookup could hand back that
older type instead of the class being declared.

The lexical binding is now installed before the body runs, as rakudo does. A
bare-name alias left by an earlier evaluation of the same declaration is
replaced at that point. An outer type with the same spelling is left alone.

Red's t/160 loads its configuration file with `EVALFILE` once per fixture
command. Its "cyclic row references are rejected" test now passes, and the
whole file matches raku (#17547).
