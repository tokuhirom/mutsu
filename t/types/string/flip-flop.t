use Test;

plan 13;

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

# Precedence: flip-flop is at conditional precedence (looser than the chaining
# comparison `==`), so `$_ == 3 ff $_ == 5` parses as `($_ == 3) ff ($_ == 5)`.
is test_ff({ $_ == 3 ff  $_ == 5 }, 1..7), 'xx345xx', 'ff binds looser than ==';
is test_ff({ $_ == 3 ^ff $_ == 5 }, 1..7), 'xxx45xx', '^ff binds looser than ==';
is test_ff({ $_ == 3 ff^ $_ == 5 }, 1..7), 'xx34xxx', 'ff^ binds looser than ==';
is test_ff({ $_ == 3 ^ff^ $_ == 5 }, 1..7), 'xxx4xxx', '^ff^ binds looser than ==';
is test_ff({ $_ == 3 fff $_ == 5 }, 1..7), 'xx345xx', 'fff binds looser than ==';

# A constant operand is smart-matched against the topic `$_` (sed-style).
is test_ff({ 3 ff 5 }, 1..7), 'xx345xx', 'constant operands smart-match against $_';
is test_ff({ 3 ^ff 5 }, 1..7), 'xxx45xx', '^ff with constant operands';

# Item assignment is looser than flip-flop: `$x = 3 ff 5` is `$x = (3 ff 5)`.
{
    my $ret = '';
    for 1..5 {
        my $x = $_ == 2 ff $_ == 4;
        $ret ~= $x ?? $_ !! 'x';
    }
    is $ret, 'x234x', 'item assignment is looser than flip-flop';
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
