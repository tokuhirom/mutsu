# A parenthesized `*` operand is the Whatever value

In Rakudo, `(*) + 1` adds a `Whatever` and dies (`Cannot resolve caller
Numeric(Whatever:D: )`). Only a bare `*` operand primes a WhateverCode. mutsu
looked through every layer of parentheses except the frozen `((*))` spelling,
so `(*) + 1`, `-(*)`, `(*) ~ "a"` and `(*) eq "x"` all became WhateverCodes
(#18698). The same rule made `((*) ~~ Int).WHAT` curry the `.WHAT`.

A parenthesized bare `*` is now the Whatever value whenever it is the operand
of an infix or prefix operator, smartmatch included. A method call or
subscript target still primes through one layer, as in Rakudo: `(*).abs` and
`(*)[0]` are WhateverCodes.
