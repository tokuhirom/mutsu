use Test;
use lib $*PROGRAM.parent(3).add('lib-where-import');

# From the Statistics::Distributions distribution: `benford-dist(UInt:D $b where
# $b > 2, ...)` is exported by a unit module; the `where` clause was skipped
# when the importing scope called the routine, so a base of 2 got through.
plan 8;

use WhereSub;

is above-two(3), 3, 'an argument satisfying the where clause is accepted';
dies-ok { above-two(2) }, 'one failing the where clause is refused';
dies-ok { above-two(0) }, 'also for another failing value';
is above-two-block(5, 2), 10, 'a block where clause with a defaulted parameter accepts';
dies-ok { above-two-block(2) }, 'and refuses';
is tally(4), 2, 'a whatever-star where clause accepts';
dies-ok { tally(3) }, 'and refuses';

my $y = 2;
dies-ok { above-two($y) }, 'a variable argument is checked too';
