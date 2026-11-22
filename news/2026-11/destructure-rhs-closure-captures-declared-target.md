# A closure on the right of `my (...) = ...` sees the declared variable

```raku
my &get;
{ my ($a, $c) = 5, (&get = { $a }); say get() }
```

This printed `(Any)`; rakudo prints `5` (#18414). A signature declaration
declares its plain targets before the right-hand side runs, since the RHS
already sees them, and then declared each target a second time from its
element of the staging list. The closure had captured the first container,
and the second declaration gave the name a fresh one that the closure never
saw.

The element step of a predeclared target is now an assignment into the
container the declaration made, the same shape the BEGIN-prologue split
already used. Scalar, array and hash targets all keep their container; a
group `is default(...)` declaration still re-declares its targets with the
trait.

That exposed an older bug: `my &go := { ... }` marked the bare name `go`
readonly, which is also the key of a same-named `$go`, so `my $go; my &go :=
{ 1 }; $go = 3` died with "Cannot assign to an immutable value". A code
variable's bind now marks only its own `&go` key.
