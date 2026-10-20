use Test;
use lib $*PROGRAM.parent(3).add('lib-term-vs-routine');

# From the CSS::Properties distribution (CSS::Properties::Calculator): the
# class imports `&dimension` from `CSS::Units`; calling `CSS::Units.value`
# re-runs the role body, whose `sub dimension ... is export` is the SAME
# declaration, not a redeclaration of the imported alias.
plan 1;

use Calc;
my $c = Calc.new;
$c.run = 5;
is $c.seen, 'px Lengths', 'the role body re-ran without X::Redeclaration';
