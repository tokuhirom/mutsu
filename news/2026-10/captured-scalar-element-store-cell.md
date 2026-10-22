# An element store on a captured undefined scalar reaches the scalar

`my $a; my $c = -> { $a{"k"} = 1 }; $c()` left `$a` undefined: the closure-capture analysis
only recognised an element store as a write to the captured scalar when the constant-pool
name started with `$`, but a scalar's name is stored without its sigil, so the closure kept a
by-value snapshot. Every non-aggregate name now counts (#14637, #14647). A named `sub`
capturing the scalar still loses the store; that is left on the issues.
