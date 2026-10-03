use v6;
use Test;
use nqp;

# `nqp::existspos`, the multi-dimensional positional ops (`atpos2d` /
# `atpos3d` / `atposnd`, `bindpos2d` / `bindpos3d` / `bindposnd` and their
# `_i` / `_n` / `_s` twins) and `nqp::atposref_s` (#11493) used to die with
# "Unsupported nqp:: op". Expected values are rakudo's (MoarVM's).

plan 6;

subtest 'existspos', {
    plan 7;
    my $l := nqp::list(1);
    is nqp::existspos($l, 0), 1, 'a bound slot';
    is nqp::existspos($l, 5), 0, 'past the end';
    is nqp::existspos($l, -1), 1, 'a negative index counts from the end';
    is nqp::existspos($l, -2), 0, 'a negative index before the start';
    my $e := nqp::list();
    nqp::bindpos($e, 3, 1);
    is nqp::existspos($e, 1), 0, 'a slot a past-the-end bind opened';
    is nqp::existspos($e, 3), 1, 'the bound slot';
    my @a = 1, 2;
    @a[4] = 5;
    is nqp::existspos(nqp::getattr(@a, List, q/$!reified/), 2), 0,
        'a hole in an Array (agrees with :exists)';
}

subtest 'object 2d / nd on a shaped Array', {
    plan 4;
    my @x[2;2];
    @x[0;1] = 3;
    my $r := nqp::getattr(@x, List, q/$!reified/);
    is nqp::atpos2d($r, 0, 1), 3, 'atpos2d';
    nqp::bindpos2d($r, 1, 0, 9);
    is @x[1;0], 9, 'bindpos2d writes through to the array';
    is nqp::atposnd($r, nqp::list_i(0, 1)), 3, 'atposnd';
    nqp::bindposnd($r, nqp::list_i(1, 1), 8);
    is @x[1;1], 8, 'bindposnd writes through to the array';
}

subtest 'native int 2d / 3d / nd', {
    plan 5;
    my @s := array[int].new(:shape(2, 2));
    nqp::bindpos2d_i(@s, 1, 1, 7);
    is @s[1;1], 7, 'bindpos2d_i';
    is nqp::atpos2d_i(@s, 1, 1), 7, 'atpos2d_i';
    is nqp::atposnd_i(@s, nqp::list_i(1, 1)), 7, 'atposnd_i';
    my @t := array[int].new(:shape(2, 2, 2));
    nqp::bindpos3d_i(@t, 1, 1, 1, 9);
    is nqp::atpos3d_i(@t, 1, 1, 1), 9, 'atpos3d_i';
    is @t[1;1;1], 9, 'bindpos3d_i writes through to the array';
}

subtest 'native str and num 2d / nd', {
    plan 2;
    my @u := array[str].new(:shape(2, 2));
    nqp::bindpos2d_s(@u, 0, 1, 'x');
    is nqp::atpos2d_s(@u, 0, 1), 'x', 'bindpos2d_s / atpos2d_s';
    my @n := array[num].new(:shape(2, 2));
    nqp::bindposnd_n(@n, nqp::list_i(0, 0), 2.5e0);
    is nqp::atpos2d_n(@n, 0, 0), 2.5e0, 'bindposnd_n / atpos2d_n';
}

subtest 'the multi-dimensional read is the one-dimensional one on the row', {
    plan 1;
    my @s := array[int].new(:shape(2, 3));
    nqp::bindpos2d_i(@s, 1, 2, 42);
    is nqp::atpos2d_i(@s, 1, 2), @s[1;2], 'atpos2d_i agrees with @s[1;2]';
}

subtest 'atposref_s', {
    plan 1;
    my @ss := array[str].new('a', 'b');
    my $ref := nqp::atposref_s(@ss, 1);
    $ref = 'z';
    is @ss.join(','), 'a,z', 'a write through the reference lands in the array';
}
