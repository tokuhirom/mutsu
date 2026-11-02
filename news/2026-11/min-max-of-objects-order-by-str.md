# `min` and `max` of two objects order them by their `Str`

`$x max $y` / `$x min $y` for two instances of a class with a `Str` (or `Stringy`) method picked the
wrong operand (#15940): `min_max_values` decided with the structural `cmp_values`, which knows nothing
of user methods, while `cmp` stringifies such an object. When either operand is an object with its own
stringifier the order now comes from the `cmp` body (`cmp_value`); every other operand shape keeps the
structural order. Pinned by `t/operators/min-max-objects-with-str.t`.
