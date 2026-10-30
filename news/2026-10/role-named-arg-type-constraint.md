# A role type constraint with named arguments is named and checked without them

`has NT[Int, :coerce(&cf)] $.d` and `my NT[Int, :coerce(&cf)] $x` used to carry the source
spelling of the colonpair into the type: `.^name` printed a broken `NT[Int,:coerce(&cf]`, an
assignment of a `NT[Int, :coerce(&cf)].new` failed its type check, and the `my` declaration died
with "Type is not declared". The parser keeps the written spelling (RakuAST renders it back); the
type check, the declared-type check of a `my` and the user-facing type name now read a
parameterized type without its named arguments, as rakudo does (#15504).
