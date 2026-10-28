# An indexed bind's value is a typed node

`@a[i] := v` and `%h<k> := v` used to carry their value as a call to the magic-named
`__mutsu_bind_index_value(rhs, sources)`, which the parser built, `convert` took apart again and
about twenty compiler guards matched by name and argument position. The value is now
`Expr::IndexBindValue(rhs)`; the source-variable metadata is derived from the right-hand side
where it is compiled, once, in `Compiler::index_bind_marker_call`. The VM's runtime marker `Pair`
is unchanged and is the next step (#7564).
