use Test;

# ADR-8751 stage 2: the user candidates of the built-in string operators
# (`~`, `eq` ... `ge`, `leg`) that a block-scoped `use` imports add to the
# operator inside the block, in every form the block uses it, and only there:
# a callee written outside the block does not see them, and a closure made in
# the block keeps them after it exited. Checked against rakudo.
plan 22;

use lib 't/lib';

sub outside-reduce($a, $b) { ([~] $a, $b) ~~ Str ?? 'built-in' !! 'candidate' }
sub outside-hyper($a, $b) { (($a,) »~« ($b,))[0] ~~ Str ?? 'built-in' !! 'candidate' }
sub outside-assign($a, $b) { my $d = $a; $d ~= $b; $d ~~ Str ?? 'built-in' !! 'candidate' }
sub outside-leg($a, $b) { ($a, $b).reduce(&infix:<leg>) }
sub outside-eq-reduce($a, $b) { [eq] $a, $b }
my &keeps-concat;
my &keeps-leg;
my ($x, $y);
{
    use UseInBlockStringOpLexical;
    $x = Tx.new(s => 'x');
    $y = Tx.new(s => 'y');
    is ($x ~ $y).s, 'x+y', 'the operator';
    is 'a' ~ 'b', 'ab', 'the built-in still answers what no candidate takes';
    my $d = $x;
    $d ~= $y;
    is $d.s, 'x+y', 'a compound assignment';
    is ([~] $x, $y, $x).s, 'x+y+x', 'a reduction';
    is (($x,) »~« ($y,))[0].s, 'x+y', 'a hyper';
    is ($x Z~ $y).map(*.s).join, 'x+y', 'a zip';
    is ($x X~ $y).map(*.s).join, 'x+y', 'a cross';
    is ($x R~ $y).s, 'y+x', 'a reversed operator';
    is &infix:<~>($x, $y).s, 'x+y', 'the operator as a value';
    is &infix:<~>('a', 'b'), 'ab', 'which falls back to the built-in';
    is $x eq $y, 'custom-eq', 'eq';
    ok 'a' eq 'a', 'the built-in eq';
    ok $y lt $x, 'lt';
    is $x leg $y, More, 'leg';
    is ($x, $y).sort(&infix:<leg>).map(*.s).join, 'yx', 'leg as a sort comparator';
    is outside-reduce($x, $y), 'built-in', 'a reduction in a callee written outside the block';
    is outside-hyper($x, $y), 'built-in', 'a hyper in a callee written outside the block';
    is outside-assign($x, $y), 'built-in', 'a compound assignment in a callee';
    is outside-leg($x, $y), Less, '&infix:<leg> in a callee';
    nok outside-eq-reduce($x, $y), 'a reduction over eq in a callee';
    &keeps-concat = -> $a, $b { ($a ~ $b).s };
    &keeps-leg = -> $a, $b { $a leg $b };
}
is keeps-concat($x, $y), 'x+y', 'a closure keeps ~ after the block';
is keeps-leg($x, $y), More, 'a closure keeps leg';
