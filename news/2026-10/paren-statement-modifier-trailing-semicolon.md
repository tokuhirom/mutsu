# `( ... if $x; )`: a modified statement in parentheses may end with `;`

The content of `(...)` is a list of statements, so `(|(1, 2) if $export;)` is legal; only the
modifier form without the `;` parsed. Found loading Logic::Ternary, whose 5 test files now match `raku`.
