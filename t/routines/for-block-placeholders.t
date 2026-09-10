use v6;
use Test;

# Multiple placeholder variables (`$^a`, `$^b`, ...) in a `for` loop body make
# the block's arity equal to the number of distinct placeholders, so the loop
# consumes that many elements per iteration. Placeholders bind in alphabetical
# order of their names. Previously mutsu bound only the first placeholder and
# threw "Cannot assign to a readonly variable" on the second.

plan 5;

{
    my @seen;
    for 1..8 { @seen.push: $^a + $^b }
    is-deeply @seen, [3, 7, 11, 15], 'two placeholders sum consecutive pairs';
}

{
    my @seen;
    for 1..4 { @seen.push: "$^a $^b" }
    is-deeply @seen, ['1 2', '3 4'], 'two placeholders consume two per iteration';
}

{
    my @seen;
    for 1..6 { @seen.push: "$^a-$^b-$^c" }
    is-deeply @seen, ['1-2-3', '4-5-6'], 'three placeholders consume three per iteration';
}

# Placeholders bind in alphabetical order of their names, regardless of the
# order they appear in the source.
{
    my @seen;
    for 1..4 { @seen.push: "$^one $^two $^three $^four" }
    is-deeply @seen, ['2 4 3 1'], 'placeholders bind in alphabetical name order';
}

# A single placeholder still binds one element per iteration.
{
    my @seen;
    for 1..3 { @seen.push: $^x }
    is-deeply @seen, [1, 2, 3], 'a single placeholder binds one element';
}
