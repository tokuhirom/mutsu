use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# Overriding the action of a core `given`, `for`, `if` or `while` and `callsame`ing
# leaves the statement as it was (#13370), including `elsif`/`else` chains.

plan 5;

use PassStatementControlSlang;

my $n = 0;
given 3 { $n += $_ }
is $n, 3, 'given';
for 1..3 { $n += $_ }
is $n, 9, 'for';
if $n > 50 { $n += 100 } elsif $n > 2 { $n += 10 } else { $n = 0 }
is $n, 19, 'if with an elsif clause';
my $i = 0;
while $i < 3 { $i++; $n += 1000 }
is $n, 3019, 'while';

my @r = do for 1..2 { $_ * 2 };
is-deeply @r, [2, 4], 'a for used as an expression';
