# `.origin` of type traits, dotted subscripts and `is built(:bind)`

Three forms now carry the ranges rakudo 2026.11 gives them:

- **A variable's type trait** (`my %h is MixHash`, `my @a is Array`): the
  `Trait::Is` runs over `is TYPE`, and its type over the type name.
- **A dotted subscript** (`$x.[0]`, `%h.<a>`, `*.{"k"}`): the subscript starts
  after its dot. An operand variable the parser left without a range is
  placed at the start of the application.
- **The rebuilt colonpair of an attribute's `is built(:bind)`**: placed over
  the text inside the parentheses.

(#15780)
