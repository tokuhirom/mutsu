use Test;

# A `return` in a CATCH handler that ran for an exception thrown inside a
# called routine returns from the routine that installed the CATCH, whatever
# call path reached the thrower. A later call took the fast call path, which
# took the `return` for the thrower's own. Identical under rakudo.
plan 4;

sub thrower { die "x" }
sub untyped { my $p = 2; CATCH { default { return $p } }; thrower(); 0 }
sub typed(--> Int) { my $p = 3; CATCH { default { return $p } }; thrower(); 0 }

is untyped(), 2, 'untyped routine returns from its CATCH';
is typed(), 3, 'a typed routine called afterwards does too';
is typed(), 3, 'and again';
is untyped(), 2, 'and the untyped one again';
