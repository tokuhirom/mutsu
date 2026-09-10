use Test;

plan 10;

# The comma is TIGHTER than the list infix operators (Z, X and their meta forms) and
# tighter than the sequence operator. Every parser that splits a comma list by hand has
# to honour that, or `Z+` takes only its adjacent element as an operand.

# Array composer.
is-deeply [1, 2 Z+ 3, 4], [4, 6], 'Z+ is looser than the comma inside [ ]';
is-deeply [1, 2 X~ 3, 4], ['13', '14', '23', '24'], 'X~ is looser than the comma inside [ ]';

my @p = 1;
is-deeply $[0, |@p Z+ |@p, 0], $[1, 1], 'the Pascal step composes inside $[ ]';

# Parenthesized list and assignment RHS already agreed; they must keep agreeing.
is-deeply (1, 2 Z+ 3, 4), (4, 6), 'Z+ is looser than the comma inside ( )';
my @assigned = 1, 2 Z+ 3, 4;
is-deeply @assigned, [4, 6], 'Z+ is looser than the comma on an assignment RHS';

# The sequence operator takes every preceding comma element as a seed.
is-deeply [0, 1, *+* ... 8], [0, 1, 1, 2, 3, 5, 8], 'the sequence seeds on all preceding elements';

# The feed is looser than the sequence, so it sits outside it -- and the seeds still
# have to reach the sequence through the feed.
my $fed = do {
    1, 2, *+* ... *
        ==> (*[0..6])()
        ==> map *.Int
        ==> elems()
};
is $fed, 7, 'a feed after a seeded sequence keeps its seeds';

my @pascal = do {
    $[1], -> @row { $[0, |@row Z+ |@row, 0] } ... *
        ==> (*[0..4])()
};
is-deeply @pascal[4], $[1, 4, 6, 4, 1], 'the Pascal triangle builds through a feed';

# A sequence endpoint must not swallow the feed into a WhateverCode.
is ((1, *+1 ... *) ==> (*[0..3])() ==> elems()), 4, 'a Whatever endpoint does not absorb the feed';

# Plain composers are unchanged.
is-deeply [[1, 2], [3, 4]], [[1, 2], [3, 4]], 'a nested array composer is unchanged';
