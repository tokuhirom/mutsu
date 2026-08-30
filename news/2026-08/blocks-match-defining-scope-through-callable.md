# Blocks retain their match scope through callable invocations

A bare block now keeps its defining scope's `$/` when it is invoked through a
`Callable` parameter in another routine. The block captures that match variable
in a shared cell, and match publication writes through the cell rather than
replacing it. The invoking routine retains its independent implicit match scope.

The regression test covers `call-it({ "yy" ~~ /(y)/ })` following an outer
match, which now leaves the outer `$/` as `y` as Rakudo does.
