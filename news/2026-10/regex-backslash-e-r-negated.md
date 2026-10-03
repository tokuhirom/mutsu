# Regex `\E` and `\R` are negated character classes

In Raku every backslash class letter negates by its upper case. mutsu
rejected `\E` ("not ESC") as an unrecognized backslash sequence and gave `\R`
Perl 5's meaning (any newline) instead of "not a carriage return". Both now
match as Rakudo does, and inside a character class `\R`, `\E`, `\T` and `\F`
are accepted as the negations of `\r`, `\e`, `\t` and `\f` (#11444).
