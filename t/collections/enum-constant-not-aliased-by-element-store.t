use lib 't/lib';
use Test;
use EnumConstElemStore;

# Storing an imported enum member into an array element through an accessor
# inside a closure used to leave the slot sharing the constant's cell, so a
# later store into the slot rewrote the constant itself. Math::NIntegrate's
# RangeEndCases (`RE_RIGHT` became `RE_LEFT`) made every later integration
# wrong (#16900).

plan 8;

my $expected = 'E_BOTH,E_LEFT,E_RIGHT,E_NONE';
is members(), $expected, 'the members start intact';

my $right = Cell.made(0);
is $right.edges[0].key, 'E_RIGHT', 'the closure stored E_RIGHT';
is members(), $expected, 'storing it did not touch the constants';

$right.flip;
is $right.edges[0].key, 'E_LEFT', 'flip replaced the slot value';
is members(), $expected, 'and the constants are still intact afterwards';

my $left = Cell.made(1);
$left.flip;
is $left.edges[0].key, 'E_RIGHT', 'the other direction flips too';
is members(), $expected, 'the constants are intact after both flips';
is E_RIGHT.key, 'E_RIGHT', 'a bareword read of the member still answers itself';
