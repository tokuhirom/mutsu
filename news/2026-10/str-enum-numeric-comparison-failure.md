# A Str-valued enum in a numeric comparison is a Failure, not a throw

`enum T (A => "text", B => "block"); B != A` died with `X::Str::Numeric`
because the operand pre-check that turns a non-numeric string into the lazy
Failure did not recognise an enum whose value is a string. It now does, the
same way `Enumeration.Numeric` numifies through `$!value`: `B == A` and
`B + 1` are Failures, and the negated `!=` / `!==` answer `True`, as in
Rakudo. Template::Jinja2's lexer compares its Str-valued `TokenType` enum
with `!==`, so every template used to die at tokenizing (#11578).
