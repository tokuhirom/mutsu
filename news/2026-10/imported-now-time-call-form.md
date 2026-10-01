# An imported or declared `now` / `time` routine is callable as `now()`

`now` and `time` are CORE terms, so mutsu rejected their call form `now()` at
compile time with "Undeclared routine" — the right answer when nothing else is
in scope, but it also fired when a module exported a routine of that name
(`our sub now(--> DateTime) is export { ... }`), which in Rakudo shadows the term
(#10369, reported from a real program). A locally declared `sub now` escaped the
error but still misparsed: `now()` became the term followed by an invocation of
its result ("No such method 'CALL-ME'").

The term parser now declines the adjacent-paren call form whenever a sub of that
name is declared in an enclosing scope or imported by `use`, letting the general
identifier parser build one ordinary call; with no such routine the compile-time
error is unchanged.
