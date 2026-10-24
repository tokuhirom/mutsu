# `"@m>>.gist()"` interpolates a hyper method call

A hyper method call (`>>.meth()` / `».meth()`) after an interpolated variable
stayed literal text in a double-quoted string; rakudo interpolates it like any
method call whose chain ends in parentheses (#14886, found by the roast-wide
RakuAST comparison: S03-operators' set tests).
