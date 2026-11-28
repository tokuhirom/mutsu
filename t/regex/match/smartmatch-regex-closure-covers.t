# `$x ~~ / $needle /` no longer publishes `$needle` into the env before the
# match when the regex literal's closure captures it from a slot that holds a
# value: the closure carries it and the match installs it (#19296). These pin
# the values such a match sees, including the cases that still publish (an
# unset variable, a variable the pattern reads from a code block). Values are
# rakudo's.
use Test;

plan 10;

{
    my $l = 'abc host12 x';
    my @r;
    for <host node host> -> $needle {
        @r.push: so $l ~~ / $needle \d+ /;
    }
    is @r.join(','), 'True,False,True', 'a loop variable is read on every match';
}

{
    my $needle = 'b';
    my @r;
    for ^3 {
        @r.push: so 'abc' ~~ / $needle /;
        $needle = $_ == 0 ?? 'z' !! 'c';
    }
    is @r.join(','), 'True,False,True', 'a reassigned lexical';
}

{
    my $v;
    my @r;
    @r.push: so 'abc' ~~ / a $v /;
    $v = 'b';
    @r.push: so 'abc' ~~ / a $v /;
    $v = Nil;
    @r.push: so 'abc' ~~ / a $v /;
    is @r.join(','), 'False,True,False', 'an unset, then set, then reset variable';
}

{
    my $x = 'b';
    {
        my $x = 'c';
        ok 'ac' ~~ / a $x /, 'an inner lexical shadows the outer one';
    }
    ok 'ab' ~~ / a $x /, 'and the outer one is read again outside';
}

{
    my $w = 'b';
    my $seen;
    ok 'ab' ~~ / a $w { $seen = $w } /, 'a code block in the pattern';
    is $seen, 'b', 'reads the same variable';
}

{
    my $p = 'b';
    my $q = 'c';
    ok 'abc' ~~ / a $p $q /, 'two interpolated variables';
}

{
    my $n = 'b';
    my $hit = 'xbx' ~~ / $n /;
    is ~$hit, 'b', 'the match result';
    is ~$/, 'b', 'and $/';
}
