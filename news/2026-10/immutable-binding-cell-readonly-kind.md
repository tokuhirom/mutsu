# An immutable `:=` binding keeps its readonly kind across frames

`my $x := 42` binds `$x` straight to a value, so assigning to it is
"Cannot assign to an immutable value". A routine that writes the outer
`$x` used to lose that refusal when its caller had a readonly parameter
also named `$x`. The caller's parameter re-kinded the name-keyed readonly
mark, and the entry reconcile then dropped it, so the write went through
(#11142, and the method form from #11085).

This is the first implementation slice of ADR-11142, which makes
readonly-ness a property of the binding. A `$` variable bound to a value or
a type object now holds that value in a binding cell that carries the
binding's readonly kind. Every holder of the binding shares the cell: the
declaring frame, a sub's captured unit lexical, a closure and a `my $y := $x`
alias. An assignment to a free variable resolves the name to its binding first
and lets the cell answer, whatever the registry holds under the name in the
calling frame. The type-object form keeps its own wording ("assign requires a
concrete object (got a Int type object instead)").

The other readonly writers (parameters, loop aliases, `constant`, sigilless
terms) still record only in the registry. #11165 tracks a direct-call case
where that still gives the wrong answer for a writable variable.
