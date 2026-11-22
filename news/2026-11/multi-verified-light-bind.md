# A multi winner its dispatch program checked binds without checking again

A multi call whose candidates carry a `where` clause or a subset type is
resolved by a stage program built once per argument-type key. When that
program picks the winner, it has already checked every parameter's type and
value constraint against the call's arguments. The positional light bind
still admitted only parameters it could check itself (`Int`, `Str`, ...), so a
subset-typed winner (`multi f(Small $x)`) always took the generic call path.
A program-picked winner now binds through the light bind without its
constraints being checked a second time (ADR-18580 D2). A plain call, and any
winner the program did not pick, is checked as before. `bench-multi-dispatch`'s
subset section goes from 583M to 366M instructions, close to its nominal
section.
