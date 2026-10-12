use Test;

# A `with`/`given` body holding a CATCH still yields its tail value as the
# routine's value (PDF::Content::Ops's `op` multi: `with %Ops{$op} { CATCH {...}; $op => ... }`).

plan 5;

sub f($h) { given $h { CATCH { default { die 'e' } }; 5 } }
is f(1), 5, 'given + CATCH tail';
sub g($h) { with $h { CATCH { default { die 'e' } }; 6 } }
is g(1), 6, 'with + CATCH tail';
sub h($h) { with $h { CATCH { default { die 'e' } }; $_ => 7 } else { 0 } }
is-deeply h(2), (2 => 7), 'with + CATCH tail with else';
is (do given 1 { CATCH { default { die 'e' } }; 8 }), 8, 'do given + CATCH';
my $r;
for 1..2 { given $_ { CATCH { default { } }; $r ~= $_ } }
is $r, '12', 'a non-final given with CATCH leaves the stack balanced';
