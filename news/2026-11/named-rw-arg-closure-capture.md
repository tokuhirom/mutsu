# A named `is raw`/`is rw` argument is seen by earlier closures

The closure-capture analysis only decoded positional call-argument sources, so
`f :p($y)` with an `is raw` parameter wrote `$y` without a closure created
earlier sharing a cell, and the closure kept reading the old value. Named
arguments (`[FALSE, descriptor]`, `key=var`) are now decoded like positional
ones. Found with Deps' nested `deps-root :deps(my $root1)` (#16121).
