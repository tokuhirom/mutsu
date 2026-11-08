use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang token calls the core `<statement>` (ADR-16888): it reads one core
# statement, which the action returns in the token's place.

plan 1;

use HostMethodsSlang;

my @got;
sub capture(*@a) { @got.push(@a.join('|')); 'r' }

justdo capture(4, 5);
is @got.join(','), '4|5', '<statement> reads one core statement';
