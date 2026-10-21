# A user subset's type object coerces as the type it refines

`subset S of Int; S.Int` warned about an uninitialized value and answered `0`;
Rakudo answers the type object `(S)`. A subset's type object is not in the
catalog, so the coercion never reached `Mu.Int`. The shapeless coercion lookup
now runs the row for the type the subset refines (the generalization of the
`UInt` special case), and a coercion that answers its invocant answers the
subset's type object.
