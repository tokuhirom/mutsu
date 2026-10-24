# A `my` in a value-position `if`/`with` branch no longer leaks into the enclosing scope

`my $x = 1; my $a = do if $c { my $x = 2; 7 }; my $b = do with $c { my $x = 3; 8 }; say $x`
printed `2`: the inner `my $x` shares its name with the outer slot, so its env mirror was
written and nothing took it back when a value-position branch ended. The next scoped branch
to exit restored "the shadowed outer binding" from that stale env entry into the outer slot.
Inside a sub the same shape made `$x` after the branch read the branch's value.

A value-position branch that declares a block-local `my` now runs as a real `do { }` block,
as one that declares a dynamic `my $*x` already did. That avoids the statement-position
scope (`BlockLocalScope`), a nested run that a suspended `gather` cannot re-enter, which is
why wrapping the branch in it earlier hung `t/collections/lazy-seq/gather-nested-loop-pull.t`.
`return`, `next` and `last` out of such a branch, `given`/`when` values, `state` and the
loop bodies around it behave as before.

Closes #14801.
