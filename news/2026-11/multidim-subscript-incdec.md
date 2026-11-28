# `++` and `--` on a multi-dimensional subscript

`@a[0;1]++` and `%h{"a";1}++` (a shaped hash) died with `X::Multi::NoMatch`:
the inc/dec operand shapes knew a variable and a single index, but not a
`MultiDimIndex`. All four forms (`++x`, `--x`, `x++`, `x--`) now read the
element, step it in a temp and write the temp back through the same
subscript, keeping the prefix/postfix result. A subscript with a side effect
(`@a[$i++;0]++`) is evaluated once (#19318).
