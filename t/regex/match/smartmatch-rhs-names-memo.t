# The names a `$x ~~ RHS` op must publish before its regexes and substitutions
# run (the variables their patterns and replacements spell) are now computed
# once per compiled chunk, not on every execution. Only the NAMES are
# memoized: each execution still reads the variables' current values. Values
# are rakudo's.
use Test;

plan 8;

{
    my @seen;
    for <a b c> -> $needle {
        @seen.push: so 'xbx' ~~ / $needle /;
    }
    is @seen.join(','), 'False,True,False', 'an interpolated variable is read on every execution';
}

{
    my $n = 0;
    my $count = 0;
    while $n < 3 {
        my $want = "w$n";
        $count++ if "w0 w1 w2" ~~ / $want /;
        $n++;
    }
    is $count, 3, 'a variable declared per iteration';
}

{
    my @out;
    for <1 2 3> -> $r {
        my $s = 'a-b';
        $s ~~ s/ '-' /$r/;
        @out.push: $s;
    }
    is @out.join(','), 'a1b,a2b,a3b', 'a substitution replacement reads its variable each time';
}

{
    my $word = 'B';
    my @hits;
    for ^2 {
        @hits.push: so 'abc' ~~ m:i/ $word /;
        $word = 'z';
    }
    is @hits.join(','), 'True,False', 'an adverbed regex reads its variable each time';
}

{
    my $topic = 'kept';
    $_ = 'outer';
    my $r = 'abc' ~~ / b /;
    is $_, 'outer', 'the topic is restored after the match';
    is ~$r, 'b', 'the match result';
}

{
    my $s = 'aaa';
    $s ~~ s:g/ a /b/;
    is $s, 'bbb', 'a destructive RHS still writes back';
    my $t = 'abc';
    $t ~~ / b /;
    is $t, 'abc', 'a pure match leaves the variable alone';
}
