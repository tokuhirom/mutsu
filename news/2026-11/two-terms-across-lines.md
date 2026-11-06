# A missing `;` between two lines is X::Syntax::Confused

A single-line expression statement with no terminating `;` that is followed,
on the next line, by a plain term (a scalar or array variable, a number or a
quoted string) is no longer silently taken as two statements:
`$a ** 3` newline `$a ** 2 ** 3` fails with rakudo's
`X::Syntax::Confused: Two terms in a row across lines (missing semicolon or
comma?)`. The check is deliberately narrow (other statement shapes, and a
next line an infix or method call could continue, are untouched). A REPL
sandbox such as Text::CodeProcessing's now reports the error text rakudo does.
