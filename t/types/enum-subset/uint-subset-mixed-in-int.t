use Test;

# A mixed-in Int (`$n but Role`) still satisfies `UInt` when the number it
# wraps does, both in a smartmatch and in multi dispatch (Bitcoin's
# `multi address(UInt $key where 1..^G.order, ...)` called with a
# `$int but Bitcoin::PrivateKey`).

plan 7;

role R { }

my $big = 0x3aba4162c7251c891207b747840551a71939b0de081f85c4e44cf7c13e41daa6;

ok (5 but R) ~~ UInt, 'a small mixed-in Int is a UInt';
ok ($big but R) ~~ UInt, 'a big mixed-in Int is a UInt';
nok (-5 but R) ~~ UInt, 'a negative mixed-in Int is not';
ok (0 but R) ~~ UInt, 'zero is';

multi f(UInt $n where 1..^2**256) { "uint" }
multi f($other) { "other" }
is f($big but R), 'uint', 'a big mixed-in Int dispatches to a UInt candidate';
is f(-5 but R), 'other', 'a negative one does not';

sub g(UInt $n) { $n + 1 }
is g(41 but R), 42, 'a UInt parameter accepts a mixed-in Int';
