# Nested declaration groups read their own RHS element

`my ($a, ($b, $c)) = 1, (2, 3)` indexed every target by its flattened position, so
`$b` received the whole `(2, 3)` and `$c` stayed unset. The expansion now derives each
target's group path from the declaration's `group_shape` and reads the nested staged
element (`@tmp[1][0]`), with slurpy and optional handling scoped per group (#15452).
