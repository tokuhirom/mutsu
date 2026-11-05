# `1,;` is a list, and `--> Enum::Value` returns the value inside a module

A statement ending in a comma before the semicolon (`$o ?? (A, $n) !! $n,;`) is a one-element
list, as it already was before a closing `}`. And a routine declared `--> Precedence::Termlike`
(or the bare `--> Comma`) in the module that declares the enum returned Nil: the definite-return
check and the value lookup only knew the caller's bare enum names. Both now read the enum
registry. Found with SQL::Abstract (t/01-basic 4 -> 13 of 140 assertions; the rest is tracked
in the PR).
