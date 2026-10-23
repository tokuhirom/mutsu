# RakuAST statement modifiers carry their origins

`.AST` now gives a statement modifier (`if`, `unless`, `with`, `without`,
`when`, `for`, `while`, `until`, `given`) the range Rakudo does, from its
keyword to the end of its condition, and gives the expression it modifies the
range from the statement's start to just before the keyword. The keyword is
read back from the source text before the condition, so a range that the text
does not confirm (a comment between the expression and the keyword) is left
off: exact or absent.

This also fixes a wrong range. The parser desugars a modifier into a
statement holding its condition first, so equal terms on both sides of the
keyword (`say $x if $x`) took each other's ranges. Term noting now visits a
modifier statement in the order the source was written.
