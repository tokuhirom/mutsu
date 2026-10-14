# A `for` body's `my` names are no longer visible to an `EVAL` after the loop

`for <x y> { my $k = 1 }` followed by `EVAL('$k')` returned `Any` instead of
dying with an undeclared-variable error (#13531). While a `for` body's frame
is parked for its routine hoist, the hoist noted the body's declarations in the
ENCLOSING scope's EVAL-visible names; they now go to the body's own frame.
