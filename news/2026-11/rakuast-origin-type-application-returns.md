# RakuAST origins of type applications and return types

`.origin` now matches rakudo 2026.11 on two more families (#15780):

- A type application (`Array[Int]`, `Hash[Str, Array[Int]]`) written as a
  term, as a declaration's type (`my Array[Int] $x`) or as a parameter's
  type: the `Type::Parameterized` and its base `Type::Simple` span the whole
  application, the base `Name` only the name, the `ArgList` the text between
  the brackets and each argument its own text. A parameter's declaration
  (`.target.declaration`) visits the application too, as rakudo's does.
- The return part of a signature (`--> Int`, `--> 42`, `--> Str:D`,
  `--> Array[Int]`) on subs, methods and pointy blocks: the `Signature` runs
  to the end of the return part (from the `-->` when there are no
  parameters) and the return node is placed over its text. A pointy block
  with a return part used to have no ranges at all below it.

The parser now records the return part's range with a routine's other
positions (`RoutineSpans.returns`). `rakuast-type-parameterized-ctor.t` is
identical to rakudo in the origin differential.
