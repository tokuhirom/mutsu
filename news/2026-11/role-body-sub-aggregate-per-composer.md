# A role body sub writes its own composer's `my @a` / `%h`

A role body runs once for each composing class, and a `sub` the body declares
closes over that run's lexicals. Body scalars already worked this way
(#13957). Body arrays and hashes did not: every composer's body sub reached
whichever composer's aggregate the environment held last, so
`role T { my @log; sub note($x) { @log.push: $x }; ... }` composed into two
classes printed `0 2 0` instead of `1 1 2`.

The body's subs are now declared again for each composing class, closed over
that class's own `@`/`%` statics as well as its scalar cells. The
composition no longer leaves any of the role body's lexicals bound in the
composing scope.
