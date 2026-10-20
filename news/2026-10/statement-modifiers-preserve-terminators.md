# Statement modifiers preserve their terminators

Inline statement modifiers leave the separating semicolon for their enclosing
statement or semilist parser. A modified expression or binding inside `try`
therefore ends before a following conditional, rather than taking that
conditional as another modifier. This fixes the RakuPM::Author parse failure.

Statement-prefix terminator restoration also retains separators already present
in rewritten heredoc buffers and avoids slicing the original source at an
invalid UTF-8 boundary.
