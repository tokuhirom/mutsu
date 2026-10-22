# A bare `my @a` / `my %h` in a loop body is fresh per iteration

An expression-position container declaration without an initializer
(`==> my @t`, `(my @a)`, `==> my %t`) reused the container of the previous
loop iteration, because the per-site `__do_decl_init` marker survived across
iterations. The compiler now tracks whether it is inside a real loop body and
skips the marker there; statement-modifier loops (`(my @a).push($_) for ^3`)
keep accumulating into one container, as in Rakudo. Fixes #14626.
