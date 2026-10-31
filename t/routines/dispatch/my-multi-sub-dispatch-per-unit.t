use Test;
use lib $*PROGRAM.parent(3).add('lib-private-multi-per-unit');

# ADR-15737 / #15786: a compunit-private multi (`my multi sub`) is dispatched
# through a cached plan keyed by the asking unit. Two modules declare a private
# `pick` with the same signatures; interleaved and repeated calls (the cache is
# warm after the first) must always reach the caller's own candidates, and a
# script-level `my multi sub pick` must not see either.

plan 12;

use PrivMultiA;
use PrivMultiB;

for ^3 {
    is a-pick-int(1),   'A.Int(1)',     "A Int (round $_)";
    is b-pick-int(1),   'B.Int(1)',     "B Int (round $_)";
}
is a-pick-str('x'),   'A.Str(x)',     'A Str';
is b-pick-str('x'),   'B.Str(x)',     'B Str';
is a-pick-two(1, 2),  'A.Int2(1,2)',  'A two-argument candidate';
is b-pick-two(1, 2),  'B.Int2(1,2)',  'B two-argument candidate';

my proto sub pick(|) {*}
my multi sub pick(Int $x) { "main.Int($x)" }
my multi sub pick(Str $x) { "main.Str($x)" }
is pick(1),   'main.Int(1)',  'the script has its own private pick';
is a-pick-int(2), 'A.Int(2)', 'A unaffected by the script declaration';
