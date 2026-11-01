use Test;

# #14371: the light call of a lexically imported routine decides on its
# arguments (spread, Pair, Junction) without decoding the site; every shape
# must still reach the right entry.

plan 8;

use lib 't/lib';
use ImportedLightCallArgs;

my $s = 0;
$s = ilc-inc($s) for ^5;
is $s, 5, 'plain positional call in a loop';
is ilc-id(3), 3, 'literal argument';
is ilc-named(1, :k<v>), '1/v', 'named argument takes the general entry';
is ilc-named(1), '1/none', 'named parameter omitted';
my @a = 1, 2, 3;
is ilc-slurpy(|@a), 3, 'a |slip argument is spread';
is ilc-id((a => 1)).value, 1, 'a Pair argument stays a positional Pair';
is ilc-inc(1 | 2).raku, (2 | 3).raku, 'a Junction argument autothreads';
my $j = 10 & 20;
is ilc-inc($j).eigenstates.sort.join(","), "11,21", 'a Junction in a variable autothreads';
