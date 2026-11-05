use v6;
use Test;

plan 3;

# An `our sub` declared in a bare block of an `EVAL` unit closes over the block's
# `my` like the program's own (#16356): callable before the block ran
# (undefined), and returning the captured value after it.

my @r;
EVAL q[{
    @r.push(&OUR::grab_a().defined);
    {
        my $a = 42;
        our sub grab_a() { $a }
    }
    @r.push(&OUR::grab_a());
}];
is @r[0], False, 'undefined before the block runs';
is @r[1], 42, 'the captured value after the block ran';

my @s;
EVAL q[{
    for 1..2 -> $i { our sub in_loop() { $i } }
    @s.push(&OUR::in_loop());
}];
is @s[0], 2, 'an our sub in a loop body keeps the loop parameter';
