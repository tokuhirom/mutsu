# `|$pair` of a role-mixed Pair, and Hash/List constants as parameters

Two gaps behind CSS::Properties' `calc()` (#14158):

- `|$pair` where the Pair carries a role mixin (`Pair but R`, how CSS::Grammar types
  its tokens) was not slipped as a named argument, so `evaluate |$expr` reached
  `sub evaluate(:%func!)` without it ("Required named parameter '%func' not passed").
- A `constant` holding a Hash or List used as a parameter (`multi calc(OpenParen, ...)`
  with `my constant OpenParen = %( :op<(> )`) was compared by object identity, so no
  argument could ever match; it now smartmatches by value like Rakudo.

The distribution's `t/calc.t` still stops at a separate multi-dispatch ranking
difference for `infix:<*>` (#14575).
