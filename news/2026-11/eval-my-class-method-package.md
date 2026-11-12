# A `my class` EVAL'd from a method keeps one identity

A `my class` declared by an `EVAL` or `EVALFILE` that runs inside a method
belongs to the method's package, as in rakudo. For `L.load`, the class is
`L::FT`. mutsu compiled its methods' `::?CLASS` for `L::FT` but registered the
type as plain `FT`. A method body does not always switch the runtime package
to its class, so the registration used the global package instead.

There was a second problem in the same place. The class's own name was only
bound after its body had run, so a trait argument in the body that named the
class (`is referencing(*.id, :model(FixtureTree))`) resolved through the
package lookup. It could pick up the type left by an earlier evaluation of the
same file.

Two changes fix this:

- A class declaration compiled inside a routine now carries the package it
  was compiled in.
- A `my class` binds its name before its body runs.

Red's t/160 loads its configuration file with `EVALFILE` once per fixture
command. Its "cyclic row references are rejected" test now passes, and the
file matches raku (#17547).
