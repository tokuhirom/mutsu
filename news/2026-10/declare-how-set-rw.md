# `is rw` on an EXPORTHOW::DECLARE declaration reaches the HOW's `set_rw`

A class declared with an `EXPORTHOW::DECLARE` declarator (`value-class Bla is rw { }`) now
calls the declarator HOW's `set_rw` with the type object, so a HOW that refuses rw classes
can die as it does under rakudo. The expression forms of such declarators (`given model C { }`,
`my model :: { }` in a list) already parse; they are pinned by a test now (#13624).
The attribute metaclass half (`<declarator>-attr`) is #14588.
