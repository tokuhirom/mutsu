# RakuAST origins of re-read parameters and colon-argument aliases

`.AST` now places, as rakudo 2026.11 does:

- the left operand variable of an infix in a routine literal passed as an
  argument (`on(sub ($d) { $d + 1; $d })`). There the parameter and the
  variables that read it again all have the same shape, so the conversion
  could not tell which range is which. The variable is now read back as
  its spelling right before the operator, which itself is written right
  before the placed right operand;
- an aliased named assertion with colon arguments (`<alias=word: "a">`)
  (#15780).
