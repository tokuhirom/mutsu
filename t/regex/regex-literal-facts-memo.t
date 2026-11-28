# What a regex literal's pattern text says about evaluating it (whether it
# embeds code, whether it interpolates a string as regex source) is settled
# once per compiled chunk instead of on every evaluation (#19296). These pin
# that each literal of a chunk keeps its own answer across repeated
# evaluations. Values are rakudo's.
use MONKEY-SEE-NO-EVAL;
use Test;

plan 7;

sub helper($s) { $s.uc }

{
    my @r;
    for ^3 -> $i {
        @r.push: so 'ab' ~~ / a { @r.push("block$i") } b /;
    }
    is @r.join(','), 'block0,True,block1,True,block2,True', 'a code block runs on every evaluation';
}

{
    my @r;
    for ^2 {
        @r.push: ~('xAB' ~~ / $(helper('ab')) /);
    }
    is @r.join(','), 'AB,AB', 'a $( ) call reaches the defining unit\'s routine';
}

{
    my $re = 'b+';
    my @r;
    for ^2 {
        @r.push: ~('abbc' ~~ / a <{ $re }> /);
        $re = 'b';
    }
    is @r.join(','), 'abb,ab', 'a <{ }> interpolation is rebuilt per evaluation';
}

{
    my @r;
    for ^2 {
        @r.push: so 'aaa' ~~ / ^ a ** {3} $ /;
        @r.push: so 'aaa' ~~ / ^ a ** 2..3 $ /;
        @r.push: so 'ab' ~~ / ab /;
    }
    is @r.join(','), 'True,True,True,True,True,True', 'several literals in one chunk';
}

{
    my $n = 0;
    my $rx = / x { $n++ } /;
    'xx' ~~ $rx for ^2;
    is $n, 2, 'a stored literal with a block';
}

{
    my @hits = <ab cd ab>.grep({ $_ ~~ / a { } b / });
    is @hits.elems, 2, 'an empty code block';
}

{
    my @r = (^3).map({ so "a$_" ~~ / a \d / });
    is @r.join(','), 'True,True,True', 'a literal inside a map closure';
}
