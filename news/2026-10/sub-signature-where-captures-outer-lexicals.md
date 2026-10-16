# A sub-signature's `where` and defaults see the routine's own outer lexicals

A `where` constraint or a default inside a sub-signature (`sub f(*@ ($x
where * < $lim))`, `sub g([$a, $b = $fallback])`) now resolves its outer
lexicals where the routine is declared, as a top-level parameter's already
did. Before, it read whatever same-named lexical the *caller* held, so
`sub c { my $lim = 0; f(5) }` failed the constraint, and a write it made
(`where { $count++ }`) reached the declaration only by way of the call's
return merge.

The ADR-12529 phase 3 census of what that merge still carries back now lists
the 26 test files that depend on it, by shape, as the work list for removing
it.
