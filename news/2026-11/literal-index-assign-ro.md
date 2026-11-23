# A positional store into a literal dies with X::Assignment::RO

`5[0] = 3` silently did nothing. rakudo stores through `AT-POS`, which at
index 0 is the literal itself, and so dies with "Cannot modify an immutable
Int (5)". The compiler now recognises a scalar-literal target (Int, Str, Num,
Rat or Bool, parenthesized or not), evaluates the operands in rakudo's order,
reads the element as `AT-POS` would (so `5[1] = 3` dies with "Index out of
range"), and raises X::Assignment::RO naming it. An `is rw` routine's result
is not a literal and stays assignable. X::Assignment::RO raised for a literal
now also carries the refused value in `.value` (#18960).
