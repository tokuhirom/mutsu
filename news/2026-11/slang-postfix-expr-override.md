# A slang can override the core `POSTFIX-EXPR` action

An actions role's `method POSTFIX-EXPR(Mu $/)` now runs over every postfix application that follows
the activating `use`, like `INFIX-EXPR` and `PREFIX-EXPR` (ADR-15762 slice 3, #13369). `callsame`
answers the core node and the attached node replaces the application.
