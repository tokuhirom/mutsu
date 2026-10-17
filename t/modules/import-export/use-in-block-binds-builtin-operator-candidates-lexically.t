use Test;

# ADR-8751 stage 2: the user candidates of a built-in operator that a
# block-scoped `use` imports (`multi infix:<+>(Pt, Pt)`) add to the operator
# inside the block, in every form the block uses it, and only there: a callee
# written outside the block does not see them, and a closure made in the
# block keeps them after it exited. Checked against rakudo.
plan 26;

use lib 't/lib';

sub outside-plus($a, $b) { try { ($a + $b).gist } // 'no candidate' }
my &keeps-plus;
my &keeps-cmp;
my $Pt;
{
    use UseInBlockBuiltinOpLexical;
    $Pt = Pt;
    my $p = Pt.new(x => 1);
    my $q = Pt.new(x => 2);
    is ($p + $q).gist, 'Pt(3)', 'the operator';
    is 1 + 2, 3, 'the built-in still answers what no candidate takes';
    my $r = $p;
    $r += $q;
    is $r.gist, 'Pt(3)', 'a compound assignment';
    is ([+] $p, $q, $q).gist, 'Pt(5)', 'a reduction';
    is (($p, $q) »+« ($q, $q)).map(*.gist).join(','), 'Pt(3),Pt(4)', 'a hyper';
    is &infix:<+>($p, $q).gist, 'Pt(3)', 'the operator as a value';
    is &infix:<+>(1, 2), 3, 'which falls back to the built-in';
    is ($p + ($q | $p)).gist, 'any(Pt(3), Pt(2))', 'a Junction operand threads';
    ok $p == Pt.new(x => 1), 'a numeric comparison';
    is ($p cmp $q), More, 'cmp';
    is ($p max $q), 'custom-max', 'max';
    is ($p * 3).gist, 'Pt(3)', 'mixed operand types';
    is (-$p).gist, 'Pt(-1)', 'a prefix';
    is -3, -3, 'the built-in prefix';
    is ~$p, 'str-pt', 'another prefix';
    is 5 - 2, 103, 'a candidate for UInts reaches the built-in through callsame';
    throws-like { 7 % 3 }, X::Multi::Ambiguous,
        'a candidate as narrow as the core one is ambiguous with it';
    is (sort { $^a cmp $^b }, ($p, $q)).map(*.gist).join(','), 'Pt(2),Pt(1)',
        'a comparator block a built-in calls';
    is ($p, $q).sort(&infix:<cmp>).map(*.gist).join(','), 'Pt(2),Pt(1)',
        'the operator as a sort comparator';
    is &infix:<cmp>.arity, 2, 'the operator value has the operator\'s arity';
    is &infix:<cmp>.count, 2, 'and its count';
    is outside-plus($p, $q), 'no candidate', 'a callee written outside the block does not see them';
    &keeps-plus = -> $a, $b { ($a + $b).gist };
    &keeps-cmp = -> $a, $b { $a cmp $b };
}
is keeps-plus($Pt.new(x => 4), $Pt.new(x => 5)), 'Pt(9)', 'a closure keeps them after the block';
is keeps-cmp($Pt.new(x => 1), $Pt.new(x => 2)), More, 'a closure keeps cmp';
is (try ($Pt.new(x => 1) + $Pt.new(x => 2)).gist) // 'no candidate', 'no candidate',
    'after the block the operator is the built-in alone';
is 5 - 2, 3, 'and so is the Int candidate';
