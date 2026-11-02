# `@a «+=« (1, 2, 3)` over a shorter array applies the op to the updated slot

A hyper compound assignment whose right side is longer than the array cycles the array, and each
cycled step now applies the operator to the slot's already-updated value, as in Rakudo:
`@d = 1, 2; @d «+=« (1, 2, 3)` is `[5 4]` (it was `[4 4]`). The expression's value is the slots as
the assignment left them, cycled to the right side's length (`(5 4 5)`). The merge keeps the right
operand in a temporary so the operator is applied without re-evaluating it (#15764).
