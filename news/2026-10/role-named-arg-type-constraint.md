# A role type constraint with named arguments is named and checked without them

`has NT[Int, :coerce(&cf)] $.d` and `my NT[Int, :coerce(&cf)] $x` used to carry the source
spelling of the colonpair into the type: `.^name` printed a broken `NT[Int,:coerce(&cf]`, an
assignment of a `NT[Int, :coerce(&cf)].new` failed its type check, and the `my` declaration died
with "Type is not declared". The parser now keeps only the positional arguments in a parameterized
type constraint, as rakudo does, and the display name of a curried role leaves its named
arguments out (#15504).
