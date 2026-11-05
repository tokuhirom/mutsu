use lib 't/lib';
use Test;

# The code in a regex literal names routines of the compunit the literal was
# written in, wherever the regex is matched: a routine this file imported is
# still found when another compunit's routine does the match (#16512, #16573;
# the shape of `like $str, / $(imported()) /` through Test::like).

use RxDefUnitFmt;
use RxDefUnitMatcher;

plan 5;

is match-in-module('x p5', / $(fmt-pence(5)) /).Str, 'p5', '$( ) code calls an imported routine';
ok bool-in-module('x p7', / $(fmt-pence(7)) /), 'a Bool result';
nok bool-in-module('x p8', / $(fmt-pence(7)) /), 'and a non-match';

my @seen;
bool-in-module('5', / '5' { @seen.push(fmt-pence(9)) } /);
is @seen.join(','), 'p9', 'a { } block calls an imported routine';

my $re = / $(fmt-pence(1)) /;
is match-in-module('p1', $re).Str, 'p1', 'a regex held in a variable';

done-testing;
