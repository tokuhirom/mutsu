# A named param's `where` constraint can reference its sibling named params

A `where` constraint on a *named* parameter could not see the other named
parameters of the same signature. Given

```raku
multi method get(:$line!, :$col! where $line == *) { 1 }
multi method get(:$line!, :$col! where $line >  *) { ... }
```

the `where` on `:$col!` references the sibling `:$line!`. mutsu never bound the
sibling named params into scope before evaluating the constraint, so `$line` was
undefined during dispatch, the candidate never matched, and the call failed with
`Cannot resolve caller ...; none of these signatures matches`.

Positional parameters already worked: each is bound into the environment as
matching proceeds, so a later param's `where` clause sees the earlier ones. Named
parameters took a different path that bound only `$_` and the param under test.
Now, before evaluating a named param's `where` constraint, every supplied sibling
named param is bound into the environment too, mirroring the positional path.
Whatever-currying on either side (`$line == *`, `* == $line`) and explicit block
forms (`where { $col <= $line }`) all work.

This fixes the `Math::PascalTriangle` distribution: its `get(:$line!, :$col!)`
dispatch is built entirely on sibling-referencing `where` constraints, and
`t/02-triangle.t` now passes 13/13. Pin: `t/where-named-param-sibling-ref.t`.
