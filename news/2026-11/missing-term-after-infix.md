# An infix operator with no term after it reports rakudo's error

`my $a = 42 *`, `1 +`, `my $x = (1 +)`, `1 xx`, `2 **` and the other binary
operators whose operand parser failed at the end of a statement or bracket
now fail with `X::Comp::AdHoc: Missing required term after infix`, the
exception class and text rakudo uses, instead of a several-hundred-character
`X::Syntax::Confused` list of everything that could have started a statement.
The diagnosis is made where the parse failure is rendered, from the operand
parsers' own "expected expression after ... operator" alternatives, and only
when nothing but a statement or bracket end follows. A few operators still
take the generic error (`~~`, `and`, `??`, a lone prefix `!`).
