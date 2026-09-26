use v6;
use lib 't/lib';
use Test;
use L10N::Testish 'no-slangification';

# A `use` argument reaches the slang module's `sub EXPORT` at parse time, so an
# EXPORT that declines to slang when given one leaves this unit's grammar alone
# (#9550; the L10N::XX test files load their vocabulary this way).

plan 3;

my $n = 0;
$n++ unless False;
is $n, 1, 'the ASCII `unless` modifier is still the keyword';

unless False { $n++ }
is $n, 2, 'the ASCII `unless` block is still the keyword';

if True { $n++ }
is $n, 3, 'the ASCII `if` block is still the keyword (not replaced by `iffy`)';
