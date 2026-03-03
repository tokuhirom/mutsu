use Test;

plan 5;

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
