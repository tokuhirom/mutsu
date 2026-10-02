use Test;

# A pattern that splices a Regex value (`<$rx>`) reuses its parse while the
# variable stays bound to the same value, and re-parses as soon as it is
# rebound (#10716). Each case matches in a loop so a stale memo would show.

plan 9;

{
    my $rx = / 'handled in' \s \d+ 'ms' /;
    my $l = 'request 12 handled in 36ms';
    my $h = 0;
    for ^50 { $h++ if $l ~~ / <$rx> / }
    is $h, 50, 'the same Regex value matches on every iteration';
}

{
    my @rxs = / a+ /, / b+ /, / \d+ /;
    my @got;
    for @rxs -> $rx {
        @got.push: ~$/ if 'xx aaa bb 42' ~~ / <$rx> /;
    }
    is-deeply @got, ['aaa', 'bb', '42'], 'a rebound loop variable re-parses';
}

{
    my $rx = / foo /;
    my @got;
    for ^4 -> $i {
        @got.push: so 'foobar' ~~ / ^ <$rx> /;
        $rx = $i %% 2 ?? / bar / !! / foo /;
    }
    is-deeply @got, [True, False, True, False], 'reassigning the variable re-parses';
}

{
    my $rx = / x /;
    my @got;
    for ^3 -> $i {
        @got.push: so 'yyy' ~~ / <$rx> /;
        $rx = $i == 0 ?? 'y' !! / z /;
    }
    is-deeply @got, [False, True, False], 'switching between Regex and Str values';
}

{
    my @got;
    for <a b c> -> $c {
        my $rx = / $c /;
        @got.push: ~$/ if 'abc' ~~ / <$rx> /;
    }
    is-deeply @got, ['a', 'b', 'c'], 'a Regex value that interpolates a lexical';
}

{
    my @got;
    for <x y> -> $c {
        my $inner = / $c+ /;
        my $rx = / <$inner> /;
        @got.push: ~$/ if 'xxyy' ~~ / <$rx> /;
    }
    is-deeply @got, ['xx', 'yy'], 'nested Regex value interpolation';
}

{
    my $a = / \d+ /;
    my $b = / <[a..z]>+ /;
    my @got;
    for ^2 {
        @got.push: ~$/ if 'ab12' ~~ / <$b> <$a> /;
        ($a, $b) = / <[a..z]> /, / <[a..z]> /;
    }
    is-deeply @got, ['ab12', 'ab'], 'two spliced values, both rebound';
}

{
    my $rx = rx:i/ hello /;
    my $h = 0;
    for ^5 { $h++ if 'say HELLO' ~~ / <$rx> / }
    is $h, 5, 'a Regex value with adverbs';
}

{
    my $rx = / \d+ /;
    my @got;
    for <a b> -> $p {
        @got.push: ~$/ if 'a1 b2' ~~ / $p <$rx> /;
    }
    is-deeply @got, ['a1', 'b2'], 'a bare scalar beside the spliced value';
}
