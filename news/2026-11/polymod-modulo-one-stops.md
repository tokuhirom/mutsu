# polymod stops at a divisor of 1 for finite divisor lists too

`120.polymod(10, 1)` is `(0 12)` and `120.polymod(1, 10, 100)` is `(120)`, as under
rakudo 2026.09: a divisor of 1 ends the decomposition and the rest of the number is
the last element. mutsu applied that only to a lazy divisor list (`1 xx *`), so a finite
list gave `(0 0 12)` and `(0 0 12 0)` (found by the raku-doc differential sweep,
`Type/Int.rakudoc`, whose own `# OUTPUT:` predates the behavior).
