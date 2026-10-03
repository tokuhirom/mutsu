# Assigning an aggregate to the topic `$_` itemizes it

`for @a { $_ = [1,2] }`, `given $t { $_ = [1] }` and friends now store an itemized
aggregate (`$[1, 2]`, `${:a(1)}`) like Rakudo. The topic is itemized when it aliases a
`Scalar` (an element or a `$` variable) and still written back raw when it aliases a whole
bare `@`/`%` container (`given @a { .=reverse }`). Fixes #11229.
