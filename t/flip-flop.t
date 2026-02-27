use Test;

plan 8;

sub test_ff($code, @a) {
    my $ret = '';
    for @a {
        $ret ~= $code.($_) ?? $_ !! 'x';
    }
    $ret;
}

is test_ff({ /B/ ff /D/ }, <A B C D E>), 'xBCDx', 'ff keeps state until rhs matches';
is test_ff({ /B/ ^ff /D/ }, <A B C D E>), 'xxCDx', '^ff excludes start value';
is test_ff({ /B/ fff^ /D/ }, <A B C D E>), 'xBCxx', 'fff^ excludes end value';

sub ff_eval($code, $lhs, $rhs, @a) {
    my $lhs_run = 0;
    my $rhs_run = 0;
    for @a {
        $code.({ $lhs_run++; ?$lhs }, { $rhs_run++; ?$rhs });
    }
    [$lhs_run, $rhs_run];
}

is-deeply ff_eval({ @_[0]() ff @_[1]() }, /B/, /B/, <A B A B A>), [5, 2],
    'ff evaluates rhs only when needed';
is-deeply ff_eval({ @_[0]() fff @_[1]() }, /B/, /B/, <A B A B A>), [3, 2],
    'fff skips lhs checks while active';

{
    my $result = '';
    for <A B C B A> -> $a {
        if $a ~~ ('B' fff 'B') {
            $result ~= $a;
        }
    }
    is $result, 'BCB', 'smartmatch with flip-flop works';
}

{
    my $ret = '';
    for 0, 1 {
        sub check_ff($_) { (/B/ ff /D/) ?? $_ !! 'x' }
        $ret ~= check_ff('A');
        $ret ~= check_ff('B');
        $ret ~= check_ff('C');
    }
    is $ret, 'xBCxBC', 'different clones get separate ff state';
}

{
    my $code = { 1 };
    my $ret = '';
    for <A B C> {
        $ret ~= $code.($_) ?? $_ !! 'x';
    }
    is $ret, 'ABC', 'call blocks do not clobber caller $_';
}
