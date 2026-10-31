# A closure no longer sees the dynamic variables of a frame that exited

`my $*X = 1` belongs to the call stack, but a closure captured it with the rest
of its creating scope, so `{ $*X }` returned from the binding routine still saw
`1` when called later (raku: undef), and so did a lazy `map`/`gather` pulled
after the routine returned (Red's `ResultSeq` fallback tripped over it, #15669).
The closure capture now leaves dynamic variables (`$*x`, `@*x`, `%*x`, `&*x`) to
the live call stack: a closure called inside the binding frame, or by a caller
that binds the variable, still sees it, as in rakudo.

Not yet: `.Seq` of a lazy `map` forces it where rakudo keeps it lazy, so
`return (1,2).map({ $*X }).Seq` from the binding routine still sees the
variable.
