use v6;
use Test;

# Regression: a Failure operand to a numeric bitwise / shift operator must
# throw its contained exception rather than being silently coerced to 0.
# Surfaced by the Gray::Code::RBC dist (TODO_dist T-040): `gray-encode("A")`
# does `$n.=Int` (a Failure for non-numeric strings) then `$n +^ ($n +> 1)`,
# which must die so the module's `dies-ok` assertions pass.

plan 8;

dies-ok { my $f = "A".Int; $f +^ 1 }, 'Failure +^ Int dies';
dies-ok { my $f = "A".Int; $f +& 1 }, 'Failure +& Int dies';
dies-ok { my $f = "A".Int; $f +| 1 }, 'Failure +| Int dies';
dies-ok { my $f = "A".Int; $f +< 1 }, 'Failure +< Int dies';
dies-ok { my $f = "A".Int; $f +> 1 }, 'Failure +> Int dies';
dies-ok { my $f = "A".Int; 1 +^ $f }, 'Int +^ Failure dies';

# The `+^=` compound-assign path must throw too.
dies-ok { my $f = "A".Int; $f +^= 1 }, 'Failure +^= Int dies';

# Ordinary bitwise still works (regression guard for the fast path).
is (5 +^ 3), 6, 'valid +^ still computes';
