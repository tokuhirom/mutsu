# A name rebound to a call's aggregate result is that aggregate

`$x := f(@a)` as a statement (a rebind of an already declared `$x`) now leaves `$x`
without a Scalar of its own, as the declaration `my $x := f(@a)` already did, so
`$x =:= @a` holds when `f` handed `@a` back. The same holds for a Hash and for an
object returned by the call, and an assigned copy (`my $x = @a`) still owns its own
Scalar. Found running the `hyperize` distribution, whose `t/01-basic.rakutest` now
matches rakudo (8 of 8).
